##### ----- Conflict Data
#' This script creates a dataset that has a rolling sum of conflict deaths for each country and day over a 12 month period
#' This means that any terrorist attack can be classified as 'in conflict' or not based on conflict activity in the preceding
#' 12 months, rather than just on the country year pairing

### --- Libraries and Variables

# Load Libraries
f_LibraryLoader(tidyverse,
                countrycode,
                rio,
                zoo)

# Dataset Variables to keep            
GED_KEEP = c("country","date_start","best")

# Conflict Thresholds
MINOR_CONFLICT = 25
WAR = 1000

# Conflict Dates
CONF_START = as.Date(paste0(TT_FIRST_YEAR - 1, "-01-01"))
CONF_LAST = as.Date(paste0(GTI_YEAR + 1, "-01-01")) - 1
DATE_SEQ = seq.Date(CONF_START, CONF_LAST, by = "day")

# Eventually this should happen via IEPG
IEP_NAMES.df <- rio::import(IEP_NAMES) 
GEOCODES = unique(IEP_NAMES.df$geocode) # Get the unique list of country codes to include in the GTI

### --- Getting the Data

# This function downloads the latest version of the GED (set version in ProjectControl.R)
conflict_1.df <- f_DownloadGED()

# This function downloads the Candidate dataset for the latest version. This dataset is released monthly.
conflict_2.df <- f_DownloadCandidate()

# Combine Candidate and GED datasets and tidy
conflict_all.df <- bind_rows(conflict_1.df,conflict_2.df) %>%
  rename(deaths = best, date = date_start) %>%
  mutate(current_year = year(date)) %>%
  filter(current_year >= TT_FIRST_YEAR - 1) %>%
  select(-current_year) %>%
  mutate(geocode = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  mutate(geocode = case_when(
    country == "Yemen (North Yemen)" ~ "YEM",
    country == "Somaliland" ~ "SOM",
    TRUE ~ geocode)) %>%
  select(-country) 

# Calculate sum of deaths by day and country
daily_deaths.df <- conflict_all.df %>%
  group_by(geocode, date) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

# Expand dataset to include all days and countries, not just those with recorded conflict deaths, and calculate rolling lagged sum 
Conflict_Grid.df <- expand.grid(geocode = GEOCODES, date = DATE_SEQ) %>%
  left_join(daily_deaths.df, by = c("geocode", "date")) %>%
  mutate(total_deaths = if_else(is.na(total_deaths), 0, total_deaths)) %>%
  arrange(geocode, date) %>%
  group_by(geocode) %>%
  mutate(lagged_deaths = lag(total_deaths, n = 1, default = 0),
         rolling_deaths = rollsumr(lagged_deaths, 365, fill = NA, align = "right")) %>%
  filter(date >= as.Date(CONF_START + 365)) %>%
  mutate(intensity = case_when(
    rolling_deaths >= WAR ~ "war",
    rolling_deaths >= MINOR_CONFLICT & rolling_deaths < WAR ~ "minor conflict",
    rolling_deaths < MINOR_CONFLICT ~ "non-conflict",
    TRUE ~ NA_character_)) %>%
  mutate(conflict = if_else(rolling_deaths >= MINOR_CONFLICT, "conflict", "non-conflict"))

# Save full conflict grid as .rds file in case we want to use it for future analysis
rio::export(Conflict_Grid.df,"./02_data/processed/Conflict_Grid.rds")


# Select only the variable that records conflict status, and save as processed data
Conflict_Status.df <- Conflict_Grid.df %>%
  select(geocode, date, conflict, intensity) %>%
  mutate(date = as.Date(date))

# Save conflict status as .rds file to allow other scripts to run independently if need be
rio::export(Conflict_Status.df,"./02_data/processed/Conflict_Status.rds")


