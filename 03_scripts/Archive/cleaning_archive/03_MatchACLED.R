##### ----- Match TT and ACLED
#' This script allows you to match Terrorism Tracker and ACLED events based on:
#' identical event date, identical country, nearest geographical distance and nearest number of deaths
#' You can alter the threshold for maximum match distance by changing the filter in the final step
#' Countries to check are set in the CONFLICT_CHECK variable in ProjectControl.R
#' Events identified in this script will need to be manually checked, and added to a master Exclusion list if 
#' they do not meet IEP's terrorism definition.

### --- Libraries, Variables, Functions

f_LibraryLoader(tidyverse,
                iepg,
                geosphere)

# Event distance match (in metres)
MATCH_DISTANCE = 5000


### --- Load the data

# Load ACLED
acled.raw <- iepg_acled()
match_acled.df <- acled.raw %>%
  filter(geocode %in% CONFLICT_CHECK & year >= CONFLICT_CHECK_START) %>%
  select(event_date, geocode, acled_latitude=latitude, acled_longitude=longitude,
                                 acled_event_type=event_type, acled_actor1=actor1, acled_actor2=actor2, 
                                 acled_notes=notes, acled_fatalities=fatalities, acled_event_id_cnty=event_id_cnty)

# Load TT
match_tt.df <- rio::import("02_data/processed/clean_TT.rds")%>%
  select(event_date = date, geocode, country, year, 
                    tt_latitude=latitude, tt_longitude = longitude,
                    tt_summary = summary, tt_event_type = event_type, tt_total_deaths = deaths_total, 
                    tt_total_injured = injured_total, tt_perpetrators = perpetrators_01_name, 
                    tt_specific_target = specific_target, tt_id = event_id) %>%
  filter(geocode %in% CONFLICT_CHECK & year >= CONFLICT_CHECK_START)


# Ungroup the dfs and match on event date and geocode
matched.df <- match_acled.df %>% 
  ungroup() %>% 
  left_join(match_tt.df %>% ungroup()) %>%
  na.omit(country) %>%
  rowwise() %>%
  mutate(dist=distHaversine(c(acled_longitude, acled_latitude), c(tt_longitude, tt_latitude))) %>%
  mutate(deathdiff=abs(tt_total_deaths-acled_fatalities))

# BY TT: Choose the most likely ACLED match for each TT event
checkset.df <- matched.df %>%
  arrange(tt_id,event_date,geocode,country,dist,deathdiff) %>%
  group_by(tt_id) %>% slice(1) %>% 
  dplyr::filter(dist <= MATCH_DISTANCE) 

# Export the data that needs to be checked
write.csv(checkset.df, paste0(ONEDRIVE,"/Data/conflict_check.csv"))

#-------------------------------------------------------------------------------
# # BY ACLED: Choose the most likely TT match for each ACLED event
# fivekm <- match_dist %>%
#   arrange(acled_event_id_cnty,event_date,geocode,country,dist,deathdiff) %>%
#   group_by(acled_event_id_cnty) %>% slice(1) %>% 
#   filter(dist <= MATCH_DISTANCE)
