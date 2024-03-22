##### ----- Get TT data from the database and exclude variables

f_LibraryLoader(tidyverse,
                lubridate,
                iepg)

# Getting latest TT data from the database
tt_raw.df <- f_GetTT(CUTOFF_DATA,CUTOFF_PULL)

# Load the exclusion list
#' as of the 2024 GTI, the exclusion list looks at Myanmar data from 2021-2023,
#' focusing on the coup period and events by rebel groups that might otherwise
#' be considered military attacks. Events are first matched against ACLED in R,
#' then manually checked to see which ones will be dropped from our final dataset.
#' The exclusion list is then saved as a csv file and imported here.

tt_drop <- rio::import(paste0(ONEDRIVE,"/data/tt_drop.csv"))

tt_excluded.df <- tt_raw.df %>%
  filter(id %in% tt_drop$id)

rio::export(tt_excluded.df, paste0(ONEDRIVE,"/data/tt_excluded.csv"))

tt_raw.df <- tt_raw.df %>%
  filter(!id %in% tt_drop$id)  # Drop events in the exclusion list

# save and export dataset with excluded events
rio::export(tt_raw.df,"02_data/processed/tt_raw.rds")


