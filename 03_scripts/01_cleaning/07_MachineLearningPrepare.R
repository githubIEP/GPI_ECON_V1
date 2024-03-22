##### ----- Preparing Data for Machine Learning Model

### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                reticulate)

# Variables to remove
ML_KEEP = c("geocode","year","event_type","date","deaths_total","hostages_total","injured_total",
            "is_claimed","perpetrator_name","suicide","terrorists_killed","summary","latitude","longitude",
            "weapons","targets","the_west","conflict","intensity")

ML_REMOVE =c("summary","Unknown")

# Groups counted as unknown
ML_UNKNOWN = c("Unknown", "Jihadist (undetermined)")

# Date Variables
ML_DATE = c("date")

### --- Read in the Data

# Load datasert, add marker for Unknown groups
ml.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  select(all_of(ML_KEEP)) %>%
  rename(Group = perpetrator_name) %>%
  mutate(Unknown = if_else(Group %in% ML_UNKNOWN, 1, 0))

# Convert Date Variables to Numeric
for (date_var in ML_DATE) {
  ml.df[[date_var]] <- as.numeric(ml.df[[date_var]])
}

### --- Extracting Group Names using ChatGPT API

#source()

### --- Split Data into Test/Training and Final, export for Python ML

ml_unknown.df <- ml.df %>%
  dplyr::filter(Unknown == 1) %>%
  select(-all_of(ML_REMOVE)) %>%
  drop_na()

ml_known.df <- ml.df %>%
  dplyr::filter(Unknown == 0) %>%
  select(-all_of(ML_REMOVE)) %>%
  drop_na()

write.csv(ml_known.df,"02_data/processed/ml_testtraining.csv")
write.csv(ml_unknown.df,"02_data/processed/ml_final.csv")


