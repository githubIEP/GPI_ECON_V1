library(tidyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(countrycode)

MIN_INDEX = 2011
CURRENT_YEAR = 2023

###---Public release Data

wb <- createWorkbook()

##Summary Sheet
index = rio::import("./02_data/processed/gti_banded_national.rds") %>%
  select(ID_0, country, year, banded_score, rank) %>%
  dplyr::filter(year>=MIN_INDEX)

summary<-dcast.data.table(data=setDT(index), 
                          formula=country ~ year, 
                          value.var=c("banded_score", "rank"))

summary = summary %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  mutate(iso3c = ifelse(country == "Kosovo", "XKO", iso3c)) %>%
  select(country, iso3c, starts_with("rank"), starts_with("banded"))

sheet_name <- "Summary"
addWorksheet(wb, sheetName = as.character(sheet_name))
writeData(wb, sheet = sheet_name, x = summary)

##rio::export(summary, "./02_data/processed/web_data.xlsx")


##Year Tabs
years = unique(index$year)
year_tables <- list()

index = rio::import("./02_data/processed/gti_banded_national.rds") %>%
  dplyr::filter(year>=MIN_INDEX) %>%
  rename(iso3c = ID_0, score = banded_score, incidents = incidents_total, 
         fatalities = deaths_total, injuries = injured_total, hostages = hostages_total)

for(i in seq_along(years)) {
  focus_year = years[i]
  
  year_data <- index %>%
    dplyr::filter(year == focus_year) %>%
    select(iso3c, country, rank, score, incidents, fatalities, injuries, hostages) %>%
    arrange(rank)
  
  sheet_name <- as.character(focus_year)
  addWorksheet(wb, sheetName = sheet_name)
  writeData(wb, sheet = sheet_name, x = year_data)
}

saveWorkbook(wb, file = paste0("./02_data/processed/GTI_public_release_data.xlsx"), overwrite = TRUE)
  

