##### ----- Get TT data from the database and tidy

### --- Packages, Variables, Functions

# function that only loads libraries if they haven't already been loaded
f_LibraryLoader(tidyverse,
                iepg,
                tidygeocoder,
                splitstackshape,
                countrycode,
                rio)

# list of TT variables to drop
TT_DROP =c("population","pop_year","start_time","start_time_lookup","end_time","end_time_lookup",
           "area","deaths","hostages","injured","tnt_equivalent_in_kg","selivery_method","author",
           "updated_at","created_at","deleted_at","attributions","updated_by_iep","source_names",
           "end_date","perpetrators")

### --- Clean and Tidy the Terrorism Tracker Data

# Getting latest TT data from the database
dbcreds$host = "192.168.0.98" # Only need to run this if connecting to the database from the VPN. May be fixed eventually
tt_raw.df <- iepg_tt() 

# Split list variables, remove perpetrator country, rename perpetrator variables
tt_clean.df <- tt_raw.df   %>%
  select(-all_of(TT_DROP)) %>% # Drop unnecessary variables
  rename(date = start_date) %>%
  mutate(date = as.Date(date)) %>%
  cSplit(c("weapons", "targets"), ";", "wide") %>% 
  select(-matches("^(targets|weapons)_[2-9]$")) %>% # remove perpetrators, targets, and weapons beyond the first two
  rename(targets = targets_1, weapons = weapons_1) # rename targets and weapons

# Further tidying - Remove hoxes and add the_west variable
tt_clean.df <- tt_clean.df %>%
  filter(event_type != "Hoax") %>%  # Remove Hoaxes
  mutate(geocode = case_when(       # Fix iso special cases, include Somaliland as Somalia
      country == "Kosovo" ~ "XKO",
      country == "Somaliland" ~ "SOM",
      TRUE ~ geocode
    ),
    country = if_else(geocode == "CZE", "Czechia", country)
  ) %>%
  mutate(the_west = case_when(
    geocode %in% THE_WEST ~ "West", # Add the_west category
    TRUE ~ "non-West"
  )) %>%
  rename(event_id = id, deaths_total = total_deaths,                      
         hostages_total = total_hostages, injured_total = total_injured)

# Add conflict variable
tt_clean.df <- tt_clean.df %>%
  left_join(Conflict_Status.df, by = c("geocode", "date")) 

# Shorten names
tt_clean.df$targets <- sapply(tt_clean.df$targets, f_ShortenName, dict = dict_Targets)
tt_clean.df$perpetrator_name <- sapply(tt_clean.df$perpetrator_name, f_ShortenName, dict = dict_Groups)
tt_clean.df$ideology <- sapply(tt_clean.df$perpetrator_type, f_ShortenName, dict = dict_Ideology)


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

# Save and export to allow other scripts to run independently
rio::export(tt_clean.df, "02_data/processed/clean_TT.rds")
