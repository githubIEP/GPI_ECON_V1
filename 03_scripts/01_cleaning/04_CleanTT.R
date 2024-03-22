##### ----- Get TT data from the database and tidy

### --- Packages, Variables, Functions

# function that only loads libraries if they haven't already been loaded
f_LibraryLoader(tidyverse,
                iepg,
                tidygeocoder,
                splitstackshape,
                countrycode,
                rio,
                sf,
                scales)

# list of TT variables to drop
TT_DROP =c("population","pop_year","start_time","start_time_lookup","end_time","end_time_lookup",
           "area","deaths","hostages","injured","tnt_equivalent_in_kg","selivery_method","author",
           "updated_at","created_at","deleted_at","attributions","source_names",
           "end_date","perpetrators","date_added_by_iep")

### --- Load the data after the exclusion list has been applied
tt_raw.df <- rio::import("02_data/processed/tt_raw.rds")

# Load the Conflict status data
Conflict_Status.df <- rio::import("02_data/processed/Conflict_Status.rds")

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
    country = if_else(geocode == "CZE", "Czechia", country),
    country = if_else(country == "Somaliland", "Somalia", country)
  ) %>%
  mutate(the_west = case_when(
                     geocode %in% THE_WEST ~ "West", # Add the_west category and the Sahel category
                     TRUE ~ "non-West"),
    the_sahel = case_when(
      geocode %in% THE_SAHEL ~ "Sahel",
      TRUE ~ "non-Sahel")
    ) %>%
  rename(event_id = id, 
         deaths_total = total_deaths,                      
         hostages_total = total_hostages, 
         injured_total = total_injured)

# Drop events for non-GPI countries if desired
gti_list.df = rio::import(IEP_NAMES) %>%
  select(geocode)

tt_clean.df <- tt_clean.df %>%
  filter(EVENT_INCLUDE == "GPI" & geocode %in% gti_list.df$geocode)

# Add conflict variable
tt_clean.df <- tt_clean.df %>%
  left_join(Conflict_Status.df, by = c("geocode", "date")) 

# Shorten names
tt_clean.df$targets <- sapply(tt_clean.df$targets, f_ShortenName, dict = dict_Targets)
tt_clean.df$perpetrator_name <- sapply(tt_clean.df$perpetrator_name, f_ShortenName, dict = dict_Groups)
tt_clean.df$ideology <- sapply(tt_clean.df$perpetrator_type, f_ShortenName, dict = dict_Ideology)

# Aggregated Groups (for example, all IS affilliates would be listed as "Islamic State")
tt_clean.df <- tt_clean.df %>%
  mutate(perpetrator_aggregated = case_when(
    is.na(perpetrator_name) ~ "Unknown or Undefined",
    grepl("^Islamic State", perpetrator_name) ~ "Islamic State",
    grepl("undetermined|undefined|unknown|Unknown", perpetrator_name, ignore.case = TRUE) ~ "Unknown or Undefined",
    TRUE ~ as.character(perpetrator_name)))

# Add Admin_1 data
tt_clean.df <- f_LatLongShape(tt_clean.df,"level1")

# Save and export to allow other scripts to run independently
rio::export(tt_clean.df, "02_data/processed/clean_TT.rds")
