##### ----- Get TT data from the database and tidy

### --- Packages, Variables, Functions

# function that only loads libraries if they haven't already been loaded
f_LibraryLoader(tidyverse,
                #iepg,
                tidygeocoder,
                splitstackshape,
                countrycode,
                rio)

# list of TT variables to drop
TT_DROP =c("population","pop_year","start_time","start_time_lookup","end_time","end_time_lookup",
           "area","deaths","hostages","injured","tnt_equivalent_in_kg","selivery_method","author",
           "updated_at","created_at","deleted_at","attributions","updated_by_iep","source_names",
           "end_date")

### --- Clean and Tidy the Terrorism Tracker Data

# IEP Reference Dataset
the_west.df <- IEP_NAMES.df %>%
  rename(the_west = `The West`) %>%
  select(-c(region,country))

# Getting latest TT data from the database
## tt_raw.df <- iepg::iepg_tt() 

# temp fix
tt_raw.df <- rio::import(paste0(ONEDRIVE,"/data/tt_raw_df.csv"))

# Split list variables, remove perpetrator country, rename perpetrator variables
tt_clean.df <- tt_raw.df   %>%
  select(-all_of(TT_DROP)) %>% # Drop unnecessary variables
  rename(date = start_date) %>%
  mutate(date = as.Date(date)) %>%
  cSplit(c("perpetrators","weapons", "targets", "source_urls"), ";", "wide")   %>% # split columns
  cSplit(c("perpetrators_01", "perpetrators_02", "perpetrators_03"), ":", "wide") %>% # further split perpetrator data
  select(-matches("perpetrators_0[1-3]_1")) %>% # remove country variables from perpetrator set
  rename_with(~ sub("_2$", "_name", .x), matches("perpetrators_0[1-3]_2")) %>% # rename _2 to name
  rename_with(~ sub("_3$", "_type", .x), matches("perpetrators_0[1-3]_3")) %>% # rename _3 to type
  select(-matches("^(perpetrators|targets|weapons|source_urls)_0[3-9]$|^(perpetrators|targets|weapons|source_urls)_[1-9][0-9]$")) # remove perpetrators, targets, and weapons beyond the first two

# Further tidying - Remove hoxes and add the_west variable
tt_clean.df <- tt_clean.df %>%
  filter(event_type != "Hoax") %>%  # Remove Hoaxes
  mutate(geocode = case_when(
      country == "Kosovo" ~ "XKO",
      country == "Somaliland" ~ "SOM",
      TRUE ~ geocode
    ),
    country = if_else(geocode == "CZE", "Czechia", country)
  ) %>%
  left_join(the_west.df, by = "geocode") %>%
  rename(event_id = id, deaths_total = total_deaths,                      
         hostages_total = total_hostages, injured_total = total_injured)

# Add conflict variable
tt_clean.df <- tt_clean.df %>%
  left_join(Conflict_Status.df, by = c("geocode", "date")) %>%
  mutate(conflict = if_else(is.na(conflict), "non-conflict", conflict))

# Add admin_1 names

# x = tt_clean.df %>% 
#   dplyr::filter(is.na(latitude)) %>%
#   tidygeocoder::geocode(address = area, method = 'arcgis', verbose = TRUE) %>%
#   mutate(latitude = lat,
#          longitude = long)
# 
# no_regions = x %>% dplyr::filter(is.na(latitude)) %>% mutate(ID = "",
#                                                              NAME = "",
#                                                              admin_level = "") %>%
#   dplyr::select(-lat, -long) %>% relocate(iep_geocode, ID, NAME, admin_level)
# 
# 
# x = x %>% dplyr::select(-lat, -long) %>% iepsqlite::latlong2shp("level1") 
# 
# 
# tmp = tt_clean.df %>% dplyr::filter(!is.na(latitude)) %>%
#   iepsqlite::latlong2shp("level1")
# 
# tt_clean.df = rbind(tmp, no_regions, x) %>% mutate(iep_geocode = substr(ID, 1, 3)) %>%
#   relocate(iep_geocode)
# 
# tt_clean.df = inner_join(gti_country_names, tt_clean.df, by="iep_geocode")
# 
# tt_clean.df = tt_clean.df %>% rename(admin_name = NAME, iep_country = iep_country.x, iep_region = iep_region.x,
#                      `The West` = `The West.x`) %>%
#   mutate(deaths_total = ifelse(is.na(deaths_total), 0, deaths_total))
#   

rio::export(tt_clean.df, "./02_data/raw/clean_TT.rds")
