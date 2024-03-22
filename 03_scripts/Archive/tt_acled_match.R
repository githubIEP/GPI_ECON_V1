library(iepg)
library(dplyr)
library(geosphere)

# This script allows you to match Terrorism Tracker and ACLED events based on:
# identical event date, identical country, nearest geographical distance and nearest number of deaths

# You can alter the threshold for maximum match distance by changing the filter in the final step



# Load TT
tt <- iepg_tt()
# Unique names for non-matching vars
tt <- tt %>% dplyr::select(event_date=start_date, geocode, country, 
                    tt_latitude=latitude, tt_longitude=longitude,
                    tt_summary=summary, tt_event_type=event_type, tt_total_deaths=total_deaths, 
                    tt_total_injured=total_injured, tt_perpetrators=perpetrators, 
                    tt_specific_target=specific_target, tt_id=id)
# Load ACLED
acled<- iepg_acled()
# Unique names for non-matching vars
acled <- acled %>% dplyr::select(event_date, geocode, acled_latitude=latitude, acled_longitude=longitude,
                acled_event_type=event_type, acled_actor1=actor1, acled_actor2=actor2, 
                acled_notes=notes, acled_fatalities=fatalities, acled_event_id_cnty=event_id_cnty)

# Ungroup the dfs and match on event date and geocode
match<-acled %>% 
  ungroup() %>% 
  left_join(tt %>% ungroup()) %>%
  na.omit(country) # Remove non-matches

# Add distance column
match_dist<-match %>%
  rowwise() %>%
  mutate(dist=distHaversine(c(acled_longitude, acled_latitude), c(tt_longitude, tt_latitude))) %>%
  mutate(deathdiff=abs(tt_total_deaths-acled_fatalities))

# BY ACLED: Choose the most likely TT match for each ACLED event
fivekm <- match_dist %>%
  arrange(acled_event_id_cnty,event_date,geocode,country,dist,deathdiff) %>%
  group_by(acled_event_id_cnty) %>% slice(1) %>% 
  dplyr::filter(dist <= 5000) # distance in metres (5 km = 5000)

# BY TT: Choose the most likely ACLED match for each TT event
tt_match <- match_dist %>%
  arrange(tt_id,event_date,geocode,country,dist,deathdiff) %>%
  group_by(tt_id) %>% slice(1) %>% 
  dplyr::filter(dist <= 5000) # distance in metres (5 km = 5000)
