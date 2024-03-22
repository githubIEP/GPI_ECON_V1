library(dplyr)
library(tidyverse)
library(lubridate)
library(iepsqlite)

DF = rio::import("./02_data/raw/cleaned-condensed-tt-data.rds")
DF = DF %>% dplyr::select(geocode, iep_country, iep_region, event_id, year, deaths_total, hostages_total, injured_total)
DF = DF %>% rename(event_country = iep_country)
DF = DF %>% na.omit()


##############

# You also need to ensure that "gpi_countries.rda" is in the current working directory
# This is a file containing a list fo country names and geocodes for the ppi/gpi/gti etc

# Set parameters
# Event weights
weight_incidents <- 1
weight_deaths <- 3
weight_injuries <- 0.5
weight_hostages <- 0.5

# Year weights
yr_1 <- 16
yr_2 <- 8
yr_3 <- 4
yr_4 <- 2
yr_5 <- 1

# Years can be  updated here for new iterations of the index 
CURRENT_YEAR = 2023 
BASE_YEAR = CURRENT_YEAR-12
MIN_YEAR=BASE_YEAR-4
WEIGHT_YEAR = CURRENT_YEAR-5

gti_df = DF %>% distinct() %>% dplyr::filter(year<= GTI_YEAR)

# Calculate total incidents, deaths, hostages and injuries for a country and year
gti_df = gti_df %>%
  distinct(event_id, geocode, country = event_country, year, deaths_total, hostages_total, injured_total) %>%
  group_by(geocode, country, year) %>%
  summarise(incidents_total = n(),
            deaths_total = sum(deaths_total),
            hostages_total = sum(hostages_total),
            injured_total = sum(injured_total)) %>%
  ungroup()

#  Create a dataframe of GTI country/year pairs, join to the gti_df dataframe with the annual scores

gti_full = rio::import("./02_data/gti-countrynames.xlsx")
gti_full <- gti_full %>%
  expand(nesting(geocode, country), year = MIN_YEAR:(CURRENT_YEAR)) %>%
  left_join(gti_df) %>%
  mutate(across(incidents_total:injured_total, ~replace_na(.x, 0)))

# calculate the weighted five year average
gti_weighted <- gti_full %>%
  group_by(country, geocode) %>%
  rowwise() %>%
  mutate(annual_score = sum(incidents_total * weight_incidents,
                            deaths_total * weight_deaths,
                            injured_total * weight_injuries,
                            hostages_total * weight_hostages)) %>%
  group_by(country, geocode) %>%
  mutate(weighted_lag = annual_score * yr_1,
         weighted_lag_1 = lag(annual_score, n = 1) * yr_2,
         weighted_lag_2 = lag(annual_score, n = 2) * yr_3,
         weighted_lag_3 = lag(annual_score, n = 3) * yr_4,
         weighted_lag_4 = lag(annual_score, n = 4) * yr_5) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(weighted_score = mean(c(weighted_lag,
                                 weighted_lag_1,
                                 weighted_lag_2,
                                 weighted_lag_3,
                                 weighted_lag_4))) %>%
  dplyr::select(geocode:annual_score, weighted_score)

# calculate annual GTI score (banded)
max_score <- max(gti_weighted %>% dplyr::filter(year >= BASE_YEAR, year<=CURRENT_YEAR) %>% pull(weighted_score))


# set that max score to 10 and find the log base that makes this max score equal 10
log_base <- (max_score + 1)^(1/10)

# use this log base to calculate the GTI score
gti_banded <- gti_weighted %>%
  mutate(banded_score = log(weighted_score + 1, log_base))


# add the ranking to the banded score
gti_banded = gti_banded %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(banded_score)))

#add regions

gti_banded = gti_banded %>% rename(ID_0=geocode) %>% iepsqlite::add_region()
gti_banded = gti_banded %>% dplyr::select(ID_0, country, year, incidents_total, 
                                   deaths_total, hostages_total, injured_total, 
                                   annual_score, weighted_score, banded_score, rank, region)


rio::export(gti_banded, "./02_data/processed/gti_banded_national.rds")


