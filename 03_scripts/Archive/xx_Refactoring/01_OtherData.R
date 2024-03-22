##### ----- Cleaning and Processing Other Datasets that are used to construct the final GTI file

### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                countrycode,
                rio,
                zoo)
                
GED_KEEP = c("country","date_start","best")
MINOR_CONFLICT = 25
WAR = 1000

CONF_START = as.Date(paste0(TT_FIRST_YEAR - 1, "-01-01"))
CONF_LAST = as.Date(paste0(GTI_LATEST_YEAR + 1, "-01-01")) - 1
DATE_SEQ = seq.Date(CONF_START, CONF_LAST, by = "day")

# Eventually this should happen via IEPG
IEP_NAMES.df <- rio::import("./02_data/gti-countrynames.xlsx") 
GEOCODES = unique(IEP_NAMES.df$geocode)


# Use the function to download and combine the data
conflict_1.df <- f_DownloadGED()
conflict_2.df <- f_DownloadCandidate()

rolling_conflict.df <- bind_rows(conflict_1.df,conflict_2.df) %>%
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
daily_deaths.df <- rolling_conflict.df %>%
  group_by(geocode, date) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE))

# Grid of all countries and days to track conflict for
Conflict_Grid.df <- expand.grid(geocode = GEOCODES, date = DATE_SEQ) %>%
  left_join(daily_deaths.df, by = c("geocode", "date")) %>%
  mutate(total_deaths = if_else(is.na(total_deaths), 0, total_deaths)) %>%
  arrange(geocode, date) %>%
  group_by(geocode) %>%
  mutate(lagged_deaths = lag(total_deaths, n = 1, default = 0),
         rolling_deaths = rollsumr(lagged_deaths, 365, fill = NA, align = "right")) %>%
  filter(date >= as.Date(CONF_START + 365)) %>%
  mutate(conflict = case_when(
    rolling_deaths >= WAR ~ "war",
    rolling_deaths >= MINOR_CONFLICT & rolling_deaths < WAR ~ "minor conflict",
    rolling_deaths < MINOR_CONFLICT ~ "non-conflict",
    TRUE ~ NA_character_))

Conflict_Status.df <- Conflict_Grid.df %>%
  select(geocode, date, conflict) %>%
  mutate(date = as.Date(date))
