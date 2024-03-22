##### ----- Calculate the Index

### --- Libraries

f_LibraryLoader(tidyverse,
                lubridate,
                iepg)

### --- Import Cleaned TT Data

# Import saved terrorism data that has been cleaned after database pull, select only necessary columns, remove duplicates
tt_raw.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  select(geocode, country, event_id, year, deaths_total, hostages_total, injured_total) %>%
  na.omit() %>%
  distinct() %>% 
  filter(year<= GTI_YEAR)

# Remove incidents which have been manually classified as not terrorism by IEP
#tt_exclude.df <- rio::import(GTI_EXCLUDE) %>%
  #EXCLUDE_LIST

# Create dataframe of year/country combinations for all IEP countries and all GTI years
gti_full.df = rio::import(IEP_NAMES) %>%
  select(geocode,country) %>%
  expand(nesting(geocode, country), year = TT_FIRST_YEAR:(GTI_YEAR))

### --- Calculate the Index

# Add Incident Counter
tt_raw.df$incidents_total = 1

tt_raw.df <- tt_raw.df %>% 
  mutate(attack_value =
    (deaths_total * e_WEIGHTS["DEATHS"]) + 
    (injured_total * e_WEIGHTS["INJURIES"]) +
    (hostages_total * e_WEIGHTS["HOSTAGES"]) +
    (incidents_total * e_WEIGHTS["INCIDENTS"])
  )

# Sum values for country/year
raw_totals.df = tt_raw.df %>%
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

gti_weighted.df <- gti_full.df %>%
  group_by(country) %>%
  arrange(year) %>%
  mutate(weighted_yr1 = attack_total * y_WEIGHTS["Y1"],
         weighted_yr2 = dplyr::lag(attack_total, n = 1) * y_WEIGHTS["Y2"],
         weighted_yr3 = dplyr::lag(attack_total, n = 2) * y_WEIGHTS["Y3"],
         weighted_yr4 = dplyr::lag(attack_total, n = 3) * y_WEIGHTS["Y4"],
         weighted_yr5 = dplyr::lag(attack_total, n = 4) * y_WEIGHTS["Y5"]) %>%
  mutate(weighted_total = (weighted_yr1 + weighted_yr2 + weighted_yr3 + weighted_yr4 + weighted_yr5)) %>%
  mutate(banded_score = log(weighted_total + 1, LOG_BASE_SCORE)) %>%
  mutate(banded_score = round(banded_score, 3)) %>%
  mutate(banded_score = case_when(
    banded_score > 10 ~ 10, 
    TRUE ~ banded_score
  )) %>%
  rename_with(~ str_replace(., "_total", ""), ends_with("_total")) %>%
  select(-starts_with("weighted"), -attack) %>%
  ungroup()
  

# Add Regions
regions.df <- rio::import(IEP_NAMES) %>%
  select(geocode, region)

gti_weighted.df <- gti_weighted.df %>%
  left_join(regions.df, by = "geocode")

# Add Ranks
gti_weighted.df <- gti_weighted.df %>%
group_by(year) %>%
  mutate(rank = rank(desc(banded_score), ties.method = "min")) %>%
  ungroup()

# Save finalized Index
rio::export(gti_weighted.df,"02_data/processed/GTI_BandedNational.rds")









