#Packages
library(tidyverse)
library(splitstackshape)
library(iepsqlite)
library(countrycode)

# Get a list of CSV's in the directory and rbind them together
df =list.files("./responses/",pattern=".csv")
df = paste0("./responses/",df)
df  = lapply(df, read.csv, header = TRUE) %>% bind_rows

all = df %>% 
  dplyr::select(id, 
         country_code, 
         country, 
         region, 
         start_date, 
         end_date, 
         latitude, 
         longitude,
         area,
         perpetrators, 
         source_urls, 
         suicide, 
         event_type,
         weapons,
         targets, 
         specific_target,
         total_deaths, 
         total_hostages,
         total_injured,
         terrorists_killed,
         summary)

################################################################################
# split the columns
################################################################################

# Start to expand some of the columns 
all  =cSplit(all, "perpetrators", ";")
all  =cSplit(all, "weapons", "; ")
all  =cSplit(all, "targets", "; ")
all  =cSplit(all, "source_urls", "; ")

# Splitting perpetrators again
# Removes unnecessary country names from perpetrators column
all  =cSplit(all, "perpetrators_1", ":")
all  =cSplit(all, "perpetrators_2", ":")
all  =cSplit(all, "perpetrators_3", ":")

# remove the perpetrator country
all = all %>% dplyr::select(-c("perpetrators_1_1",
                      "perpetrators_2_1",
                      "perpetrators_3_1"))

# rename perpetrator columns for clarity
all = all %>% rename(perpetrators_1_name =perpetrators_1_2,
                   perpetrators_1_type =perpetrators_1_3,
                   perpetrators_2_name =perpetrators_2_2,
                   perpetrators_2_type =perpetrators_2_3,
                   perpetrators_3_name =perpetrators_3_2,
                   perpetrators_3_type =perpetrators_3_3)

################################################################################
# Remove unncesssary source columns (from 3 to 33)
################################################################################

unnecesary = all %>% dplyr::select(starts_with("source_urls"))
unnecesary = unnecesary %>% dplyr::select(-c(ends_with("01"),ends_with("02")))
unnecesary = colnames(unnecesary)
all = all %>% dplyr::select(-all_of(unnecesary))


################################################################################
# Add necessary columns
################################################################################

all$year = as.integer(substr(all$start_date,1,4))

all$geocode = countrycode::countrycode(all$country, "country.name", "iso3c")

all <- all %>% 
  mutate(geocode = ifelse(country == "Kosovo", "XKO", geocode),
         country = ifelse(geocode== "CZE", "Czechia", country))
         # geocode = ifelse(country == "Somaliland", "SOM", geocode))

gti_country_names = rio::import("./02_data/gti-countrynames.xlsx")
gti_country_names = gti_country_names %>% rename(iep_country = country, 
                                                 iep_geocode = geocode,
                                                 iep_region = region)

# check for differences in geocodes 
# The only differences should be MDV, FJI, MLT, COM and HKG (NA is Somaliland - leave for now)
setdiff(unique(all$geocode),unique(gti_country_names$iep_geocode))
setdiff(unique(gti_country_names$iep_geocode),unique(all$geocode))


# Left join the terrorism tracker TO the gti countries to ensure you have all countries there
all = left_join(gti_country_names,all, by=c("iep_geocode" = "geocode"))
all = all %>% rename(geocode = iep_geocode)

################################################################################
# Remove hoaxes
################################################################################

all = all %>% dplyr::filter(event_type!= "Hoax")


################################################################################
# Rename some of the columns
################################################################################

all = all %>%
  rename(event_id = id,
         deaths_total = total_deaths, 
         hostages_total = total_hostages, 
         injured_total = total_injured
  )

# select the desired columns in the specified order 
all = all %>%
  dplyr::select(iep_country, 
         geocode,
         event_id,
         iep_region,
         "The West",
         year,
         start_date,
         end_date,
         latitude,
         longitude,
         area,
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
         summary)


####Add conflict status (Note: Check this is the latest version of UCDP https://ucdp.uu.se/downloads/, save it in shared drive)
file <- file.path(IEP_USERPATH, "Global Terrorism Index/2024 GTI/Data/GEDEvent_v23_1.rds")

conflict <- readRDS(file)

conflict = conflict %>% dplyr::select(country, year, best) %>%
  mutate(geocode = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  mutate(geocode = ifelse(country == "Yemen (North Yemen)", "YEM", geocode)) %>%
  group_by(geocode, year) %>%
  summarise(deaths = sum(best))

con_2023 = conflict %>%
  dplyr::filter(year == 2022) %>%
  distinct(geocode, .keep_all = TRUE)
con_2023 <- con_2023 %>%
  mutate(year = 2023)

conflict <- bind_rows(con_2023, conflict)

conflict = conflict %>% 
  dplyr::filter (year >= 2007) %>%
  mutate(intensity = case_when(deaths >1000 ~ "war",
                               deaths >=25 & deaths <1000 ~ "minor",
                               deaths <25 ~ "non-conflict")) %>%
  mutate(conflict = if_else(deaths >=25, "conflict", "non-conflict")) %>%
  dplyr::select(geocode, year, conflict, intensity)

all = all %>% left_join(conflict)

all$intensity[is.na(all$intensity)] <- "non-conflict"
all$conflict[is.na(all$conflict)] <- "non-conflict"

###Add admin_1 names

x = all %>% dplyr::filter(is.na(latitude)) %>%
  tidygeocoder::geocode(address = area, method = 'arcgis', verbose = TRUE) %>%
  mutate(latitude = lat,
         longitude = long)

no_regions = x %>% dplyr::filter(is.na(latitude)) %>% mutate(ID = "",
                                                             NAME = "",
                                                             admin_level = "") %>%
  dplyr::select(-lat, -long) %>% relocate(geocode, ID, NAME, admin_level)
  

x = x %>% dplyr::select(-lat, -long) %>% iepsqlite::latlong2shp("level1") %>% mutate(geocode = substr(ID, 1, 3)) %>% 
  relocate(geocode)

tmp = all %>% dplyr::filter(!is.na(latitude)) %>%
  iepsqlite::latlong2shp("level1") %>% mutate(geocode = substr(ID, 1, 3)) %>% 
  relocate(geocode)

all = rbind(tmp, no_regions, x)

all = inner_join(gti_country_names, all, by=c("iep_geocode" = "geocode"))
all = all %>% rename(geocode = iep_geocode)

all = all %>% rename(admin_name = NAME, iep_country = iep_country.x, iep_region = iep_region.x,
                    `The West` = `The West.x`) %>%
  mutate(deaths_total = ifelse(is.na(deaths_total), 0, deaths_total)) %>%
   dplyr::select(iep_country, 
         geocode,
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
         intensity, conflict)




rio::export(all, "./02_data/raw/cleaned-condensed-tt-data.rds")
