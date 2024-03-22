##### ----- Refactored Index Calculation

### --- Libraries

library(dplyr)
library(tidyverse)
library(lubridate)
library(iepsqlite)

### --- Functions, Variables etc.

# Event weights
weight_incidents <- 1
weight_deaths <- 3
weight_injuries <- 0.5
weight_hostages <- 0.5

# Year weights
yr_1 <- .52
yr_2 <- .26
yr_3 <- .13
yr_4 <- .06
yr_5 <- .03

# Bands
BANDED_MAX = 20000
LOG_BASE_SCORE = (BANDED_MAX + 1)^(1/10)

# Years can be  updated here for new iterations of the index 
CURRENT_YEAR = 2023 #???
BASE_YEAR = CURRENT_YEAR-12 #???
MIN_YEAR=BASE_YEAR-4 # Should extract this from the actual terrorism tracker dataset
WEIGHT_YEAR = CURRENT_YEAR-5 #???

### --- Import Raw Data

# Import saved terrorism data that has been cleaned after database pull, select only necessary columns, remove duplicates
raw.df = rio::import("./02_data/raw/cleaned-condensed-tt-data.rds") %>%
  dplyr::select(iep_geocode, iep_country, iep_region, event_id, year, deaths_total, hostages_total, injured_total) %>%
  rename(country = iep_country,
         geocode = iep_geocode) %>%
  na.omit() %>%
  distinct() %>% 
  dplyr::filter(year<= CURRENT_YEAR)

# Create dataframe of year/country combinations for all IEP countries and all GTI years
gti_full.df = rio::import("./02_data/gti-countrynames.xlsx")
gti_full.df <- gti_full.df %>%
  expand(nesting(geocode, country), year = MIN_YEAR:(CURRENT_YEAR))

### --- Calculate the Index

# Add Incident Counter
raw.df$incidents_total = 1

# Calculate the Value of each attack
raw.df$attack_value =  (raw.df$deaths_total * weight_deaths) + 
                       (raw.df$injured_total * weight_injuries) +
                       (raw.df$hostages_total * weight_hostages) +
                       (raw.df$incidents_total * weight_incidents)

# Sum values for country/year
raw_totals.df = raw.df %>%
  distinct(event_id, geocode, country, year, deaths_total, hostages_total, injured_total, incidents_total, attack_value) %>%
  group_by(geocode, country, year) %>%
  summarise(incidents_total = sum(incidents_total),
            deaths_total = sum(deaths_total),
            hostages_total = sum(hostages_total),
            injured_total = sum(injured_total),
            attack_total = sum(attack_value)) %>%
  ungroup()

# Join with full dataframe
gti_full.df <- gti_full.df %>%
  left_join(raw_totals.df, by = c("geocode","country", "year")) %>%
  mutate(across(incidents_total:attack_total, ~replace_na(.x, 0)))


# calculate the weighted five year score
gti_weighted.df <- gti_full.df %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(weighted_yr1 = attack_total * yr_1,
         weighted_yr2 = dplyr::lag(attack_total, n = 1) * yr_2,
         weighted_yr3 = dplyr::lag(attack_total, n = 2) * yr_3,
         weighted_yr4 = dplyr::lag(attack_total, n = 3) * yr_4,
         weighted_yr5 = dplyr::lag(attack_total, n = 4) * yr_5) %>%
  mutate(weighted_total = (weighted_yr1 + weighted_yr2 + weighted_yr3 + weighted_yr4 + weighted_yr5)) %>%
  mutate(banded_score = log(weighted_total + 1, LOG_BASE_SCORE)) %>%
  mutate(banded_score = case_when(
    banded_score > 10 ~ 10, 
    TRUE ~ banded_score
  ))


gti_banded = gti_weighted.df %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(banded_score)))

#add regions

gti_banded = gti_banded %>% rename(ID_0=geocode) %>% iepsqlite::add_region()
gti_banded = gti_banded %>% dplyr::select(ID_0, country, year, incidents_total, 
                                          deaths_total, hostages_total, injured_total, 
                                          annual_score, weighted_score, banded_score, rank, region)


rio::export(gti_banded, "./02_data/processed/gti_banded_national.rds")









