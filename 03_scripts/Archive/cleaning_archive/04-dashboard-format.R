library(dplyr)
library(tidyverse)
library(lubridate)

##DASHBOARD DATA (EXCLUDING ADVANCED TAB)
index = rio::import("./02_data/processed/gti_banded_national.rds")
index = index %>% rename(incidents = incidents_total, "GTI Overall Score" = banded_score, 
                         killed = deaths_total, hostages = hostages_total, wounded = injured_total, code = ID_0)
index = index <- gather(index, indicator, value, incidents, killed, hostages, wounded, 'GTI Overall Score', factor_key=TRUE)
index = index %>% dplyr::select(code, country, year, indicator, value, region)

##ADVANCED TAB
tt_data = rio::import("./02_data/raw/cleaned-condensed-tt-data.rds")
group_class = rio::import("./02_data/processed/TT_group_classifications.xlsx")
tt_data["Incidents"] <- 1

tt_data = tt_data %>% rename(Country = iep_country, Code = geocode, Region = iep_region, Year = year, Killed = deaths_total, 
                              "Target Type" = targets_1, "Specific Target" = specific_target, group = perpetrators_1_name) %>%
  mutate(group = if_else(is.na(group), "Unknown", group))
  
sahel = c("NER", "BFA", "NGA", "MLI", "TCD", "MRT",
          "GMB", "SEN", "GIN", "CMR")

tt_data = tt_data %>% mutate(Sahel = if_else(`Code` %in% sahel, "Yes", "No"))

tt_data <- merge(tt_data, group_class, by = "group", all.x = TRUE)
  
tt_data = tt_data %>% 
    dplyr::select(`Country`, `Code`, `Region`, `Sahel`, `Year`, `Incidents`, `Killed`, `Target Type`,
                               `Specific Target`, group, `The West`, conflict, intensity, `Ideology`, `sub-ideology`, `Affiliated`)


