##### ----- Data Export: Dashboard(Simple & Complex), Public Release, Web Data

### --- Libraries, Variables, Functions

f_LibraryLoader(tidyverse,
                rio,
                openxlsx)

# Set wb export locations
wb_Dashboard = loadWorkbook(GTI_DASHBOARD)
s_Sheet = "DataIndex" # Worksheet for Index Summary Data
c_Sheet = "DataIncidents" # Worksheet for Incidents Data for Advanced Analysis

### --- Simple Dashboard Data

# Load Data, transform into pivot table format, change to excel format #N/A
s_Dashboard.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  rename(`Overall Score` = banded_score,
         killed = deaths) %>%
  pivot_longer(cols = c("incidents","killed","injured","hostages","killed","Overall Score"), 
               names_to = "Indicator", 
               values_to = "Value") %>%
  filter(!is.na(Value))

## -- Export as .csv file
#write.csv(s_Dashboard.df, paste0(ONEDRIVE,"/Collateral/Dashboard_Latest.csv"), row.names = FALSE)

## -- Write Data Directly to the Dashboard

# Write new data to dashboard
writeData(wb_Dashboard, sheet = s_Sheet,
          x = s_Dashboard.df, 
          startRow = 1, startCol = 1, colNames = TRUE)

# Add Named Range
createNamedRegion(wb_Dashboard, sheet = s_Sheet, 
                  cols = 1:ncol(s_Dashboard.df), 
                  rows = 1:nrow(s_Dashboard.df),
                  name = "data.table", overwrite = TRUE)

### --- Complex Dashboard Data

# Variables to export to the dashboard
C_DASHBOARD_KEEP = c("geocode","country","year","date",
                     "region","the_west","Sahel",
                     "gov_type","income_group",
                     "conflict","intensity",
                     "group","ideology_1","ideology_2",
                     "target_type","specific_target","suicide","event_type",
                     "incidents","killed","summary")

# Need to redo this file
ideologies.df = rio::import(GTI_GROUPS) %>%
  rename(ideology_1 = Ideology, ideology_2 = `sub-ideology`)

# Prepare Dashboard Data
c_Dashboard.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  mutate(Sahel = if_else(geocode %in% THE_SAHEL, "Yes", "No"),
         incidents = 1) %>%
  rename(region = `gpi_region`,
         gov_type = `gpi_gov`,
         income_group = income,
         target_type = targets_01,
         killed = deaths_total,
         group = perpetrators_01_name
         ) %>%
  filter(group != "Unknown") %>%
  left_join(ideologies.df, by = "group") %>%
  select(all_of(C_DASHBOARD_KEEP)) %>%
  mutate(specific_target = case_when(target_type == "Undetermined" ~ NA,
                                     TRUE ~ specific_target))
  
# Write the incident data to the dashboard
writeData(wb_Dashboard, sheet = c_Sheet,
          x = c_Dashboard.df, 
          startRow = 1, startCol = 1, colNames = TRUE)

# Add Named Range
createNamedRegion(wb_Dashboard, sheet = s_Sheet, 
                  cols = 1:ncol(c_Dashboard.df), 
                  rows = 1:nrow(c_Dashboard.df),
                  name = "data.table2", overwrite = TRUE)

# Save workbook
saveWorkbook(wb_Dashboard, GTI_DASHBOARD, overwrite = TRUE)

### --- Web Data

# Indicator Bands
WEB_BANDS <- c(0, 5, 20, 50, 100, 500)

# Add Bands, Put in Web Format
gti_web.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  filter(year >= GTI_FIRST_YEAR) %>%
  rename(
    index_Incidents = incidents,
    index_Fatalities = deaths,
    index_Injuries = injured,
    index_Hostages = hostages,
    index_GTI_overall_score = banded_score,
    code = geocode,
    name_p = country,
    iep_region = region) %>%
  group_by(year) %>%
  mutate(rank = rank(desc(index_GTI_overall_score), ties.method = "min")) %>%
  mutate(
    band_GTI_overall_score = f_WebBands(index_GTI_overall_score, c(0, 2, 4, 6, 8)),
    band_Incidents = f_WebBands(index_Incidents, WEB_BANDS),
    band_Fatalities = f_WebBands(index_Fatalities, WEB_BANDS),
    band_Injuries = f_WebBands(index_Injuries, WEB_BANDS),
    band_Hostages = f_WebBands(index_Hostages, c(0, 2, 10, 25, 50, 100))
  ) %>%
  select(
    code, name_p, year, rank,
    index_Incidents, band_Incidents,
    index_Fatalities, band_Fatalities,
    index_Injuries, band_Injuries,
    index_Hostages, band_Hostages,
    index_GTI_overall_score, band_GTI_overall_score,
    iep_region
  )

# Export the data
rio::export(gti_web.df, paste0(ONEDRIVE,"/Collateral/GTI_WebData_",REPORT_YEAR,".csv"), row.names = FALSE)

### --- Public Release Data         

# Set the workbook

wb_Public = loadWorkbook(GTI_PUBLIC)
s_Overall = "Overall Scores"

# Import the data
pr_Data.df <- rio::import("02_data/processed/GTI_BandedNational.rds")

# Make the Overall Scores Dataframe
pr_Overall.df <- pr_Data.df %>%
  filter(year >= GTI_FIRST_YEAR) %>%
  select(country,geocode,year,banded_score) %>%
  group_by(year) %>%
  mutate(rank = rank(desc(banded_score), ties.method = "min")) %>%
  ungroup() %>%
  rename(score = banded_score, iso3c = geocode) %>%
  pivot_longer(cols = c(score,rank), names_to = "Indicator", values_to = "Value") %>%
  unite("year_Indicator", c("year", "Indicator"), sep = " ") %>%
  pivot_wider(names_from = year_Indicator, values_from = Value)

# Write it to the spreadsheet
writeData(wb_Public, sheet = s_Overall,
          x = pr_Overall.df, 
          startRow = 6, startCol = 1, colNames = TRUE)

# Make the format for the yearly data
pr_Yearly.df <- pr_Data.df %>%
  filter(year >= GTI_FIRST_YEAR) %>%
  mutate(Rank = rank(desc(banded_score), ties.method = "min")) %>%
  select(-region) %>%
  rename(Country = country, iso3c = geocode,
         Score = banded_score,
         Incidents = incidents,
         Fatalities = deaths,
         Injuries = injured,
         Hostages = hostages) %>%
  select(Country,iso3c,Rank,Score,everything()) %>%
  arrange(year,Rank)

# Use loop to write each of the years as separate sheets
for(wb_year in GTI_FIRST_YEAR:GTI_YEAR) {
  filter(pr_Yearly.df, year == wb_year) %>%
    select(-year) %>%
    writeData(wb_Public, sheet = as.character(wb_year),
              x = ., 
              startRow = 6, startCol = 1, colNames = TRUE)
}
  
# Save workbook
saveWorkbook(wb_Public, GTI_PUBLIC, overwrite = TRUE)         

  