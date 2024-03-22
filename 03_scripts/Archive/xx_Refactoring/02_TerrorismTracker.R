##### ----- Get TT data from the database and tidy

### --- Packages, Variables, Functions
library(tidyverse)
library(splitstackshape)
# library(iepsqlite)
library(countrycode)
library(iepg)
library(tidygeocoder)

### --- Get Terrorism Tracker Data from Database

tt_raw.df <- iepg::iepg_tt()

### --- Clean and Tidy TT Data

## -- Splitting lists into multiple variables

# Expand List Columns
tt_clean.df <- tt_raw.df %>% 
  cSplit("perpetrators", ";") %>%
  cSplit("weapons", "; ") %>%
  cSplit("targets", "; ") %>%
  cSplit("source_urls", "; ") %>%

# Expand perpetrators further
tt_clean.df <- tt_clean.df %>% 
  cSplit("perpetrators_1", ":") %>%
  cSplit(all, "perpetrators_2", ":") %>%
  cSplit(all, "perpetrators_3", ":")

# remove the perpetrator country
tt_clean.df <- tt_clean.df %>% dplyr::select(-c("perpetrators_1_1",
                               "perpetrators_2_1",
                               "perpetrators_3_1"))

# rename perpetrator columns for clarity #REFACTORS WITH LOOP?? ****
all = all %>% rename(perpetrators_1_name =perpetrators_1_2,
                     perpetrators_1_type =perpetrators_1_3,
                     perpetrators_2_name =perpetrators_2_2,
                     perpetrators_2_type =perpetrators_2_3,
                     perpetrators_3_name =perpetrators_3_2,
                     perpetrators_3_type =perpetrators_3_3)

## -- Remove Unnecessary Source Columns

# Select unnecessary columns and extract names
tt_remove = tt_clean.df %>% dplyr::select(starts_with("source_urls")) %>%
  dplyr::select(-c(ends_with("01"),ends_with("02")))

tt_remove = colnames(tt_remove)

# Remove unnecessary columns
tt_clean.df <- tt_clean.df %>% dplyr::select(-all_of(tt_remove))


## -- Tidying Data

# Add a year variables
tt_clean.df$year = as.integer(substr(all$start_date,1,4))

# all$geocode = countrycode::countrycode(all$country, "country.name", "iso3c")

# Adding and adjusting country codes, **** Function here?
tt_clean.df <- tt_clean.df %>% 
  mutate(geocode = ifelse(country == "Kosovo", "XKO", geocode),
         country = ifelse(geocode == "CZE", "Czechia", country))


gti_country_names = rio::import("./02_data/gti-countrynames.xlsx")
gti_country_names = gti_country_names %>% rename(iep_country = country, 
                                                 iep_geocode = geocode,
                                                 iep_region = region)

# check for differences in geocodes 
# The only differences should be MDV, FJI, MLT, COM and HKG (SO is Somaliland - leave for now)
setdiff(unique(all$geocode),unique(gti_country_names$iep_geocode))
setdiff(unique(gti_country_names$iep_geocode),unique(all$geocode))


# Left join the terrorism tracker TO the gti countries to ensure you have all countries there
all = left_join(gti_country_names,all, by=c("iep_geocode" = "geocode"))
# all = all %>% rename(geocode = iep_geocode)

# Create list of hoax events and export, then remove hoaxes from main dataset
tt_hoaxes <- tt_clean.df %>%
  dplyr::filter(event_type == "Hoax")
write.csv(tt_hoaxes,"04_outputs/tables/hoax_data.csv")

tt_clean.df <- tt_clean.df %>% 
  dplyr::filter(event_type!= "Hoax")

# Rename Variables

tt_clean.df <- tt_clean.df %>%
  rename(event_id = id,
         deaths_total = total_deaths, 
         hostages_total = total_hostages, 
         injured_total = total_injured
  )

# Join with Conflict Data to add conflict status and intensity
tt_clean.df = tt_clean.df %>% left_join(conflict)

all$intensity[is.na(all$intensity)] <- "non-conflict"
all$conflict[is.na(all$conflict)] <- "non-conflict"

# Add admin_1 names # ***** FROM IEPSQLITE OR NEW PACKAGE?

x = all %>% dplyr::filter(is.na(latitude)) %>%
  tidygeocoder::geocode(address = area, method = 'arcgis', verbose = TRUE) %>%
  mutate(latitude = lat,
         longitude = long)

no_regions = x %>% dplyr::filter(is.na(latitude)) %>% mutate(ID = "",
                                                             NAME = "",
                                                             admin_level = "") %>%
  dplyr::select(-lat, -long) %>% relocate(iep_geocode, ID, NAME, admin_level)


x = x %>% dplyr::select(-lat, -long) %>% iepsqlite::latlong2shp("level1") 


tmp = all %>% dplyr::filter(!is.na(latitude)) %>%
  iepsqlite::latlong2shp("level1")

all = rbind(tmp, no_regions, x) %>% mutate(iep_geocode = substr(ID, 1, 3)) %>%
  relocate(iep_geocode)

all = inner_join(gti_country_names, all, by="iep_geocode")

all = all %>% rename(admin_name = NAME, iep_country = iep_country.x, iep_region = iep_region.x,
                     `The West` = `The West.x`) %>%
  mutate(deaths_total = ifelse(is.na(deaths_total), 0, deaths_total)) %>%
  dplyr::select(iep_country, 
                iep_geocode,
                ID,
                event_id,
                iep_region,
                "The West",
                year,
                start_date,
                end_date,
                latitude,
                longitude,
                admin_name,
                event_type,
                perpetrators_1_name,
                perpetrators_1_type,
                perpetrators_2_name,
                perpetrators_2_type,
                perpetrators_3_name,
                perpetrators_3_type,
                source_urls_01,
                source_urls_02,
                suicide,
                specific_target,
                targets_1,
                targets_2,
                targets_3,
                targets_4,
                targets_5,
                targets_6,
                targets_7,
                deaths_total,
                hostages_total,
                injured_total,
                terrorists_killed,
                weapons_1,
                weapons_2,
                weapons_3,
                weapons_4,
                summary, 
                intensity, 
                conflict)

### ADD SAHEL VARIABLE

# Export Final Cleaned Dataset as rds
rio::export(all, "./02_data/raw/cleaned-condensed-tt-data.rds")

# Export truncated version as
