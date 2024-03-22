##### ----- Data Export: Dashboard(Simple & Complex), Public Release, Web Data

### --- Libraries, Variables, Functions

f_LibraryLoader(tidyverse,
                rio,
                openxlsx)

### --- Simple Dashboard Data

# Load Data, transform into long format for dashboard
s_Dashboard.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  rename(`Overall Score` = banded_score,
         killed = deaths) %>%
  pivot_longer(cols = c("incidents","killed","injured","hostages","rank","Overall Score"), 
               names_to = "Indicator", 
               values_to = "Value") %>%
  filter(!is.na(Value))

# Export as .csv file
write.csv(s_Dashboard.df, paste0(ONEDRIVE,"/Collateral/Dashboard_Latest.csv"), row.names = FALSE)

### --- Complex Dashboard Data

# Variables to export to the dashboard
C_DASHBOARD_KEEP = c("geocode","country","year",
                     "region","the_west","Sahel", "gov_type",
                     "ideology","group",
                     "conflict","intensity",
                     "target_type","event_type", "suicide",
                     "incidents","killed")

# Prepare Dashboard Data
c_Dashboard.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  mutate(Sahel = if_else(geocode %in% THE_SAHEL, "Yes", "No"),
         incidents = 1) %>%
  rename(region = `gpi_region`,
         gov_type = `gpi_gov`,
         income_group = income,
         target_type = targets,
         injured = injured_total,
         killed = deaths_total,
         hostages = hostages_total,
         group = perpetrator_name
         ) %>%
  mutate(unknown = if_else(group == "Unknown",1,0)) %>%
  select(all_of(C_DASHBOARD_KEEP))

write.csv(c_Dashboard.df, paste0(ONEDRIVE,"/Collateral/DashboardEvents_Latest.csv"), row.names = FALSE)

### --- Web Data

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
    band_GTI_overall_score = f_ScoringBands(index_GTI_overall_score, SCORING_BANDS),
    band_Incidents = f_ScoringBands(index_Incidents, WEB_BANDS),
    band_Fatalities = f_ScoringBands(index_Fatalities, WEB_BANDS),
    band_Injuries = f_ScoringBands(index_Injuries, WEB_BANDS),
    band_Hostages = f_ScoringBands(index_Hostages, HOSTAGE_BANDS)
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

# # Set the workbook

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
  select(-c(region,region2)) %>%
  rename(Country = country, iso3c = geocode,
         Score = banded_score,
         Incidents = incidents,
         Fatalities = deaths,
         Injuries = injured,
         Hostages = hostages) %>%
  select(Country,iso3c,rank,Score,everything()) %>%
  arrange(year,Rank) %>%
  select(-Rank)

# Use loop to write each of the years as separate sheets
for(wb_year in GTI_FIRST_YEAR:GTI_YEAR) {
  filter(pr_Yearly.df, year == wb_year) %>%
    select(-year) %>%
    writeData(wb_Public, sheet = as.character(wb_year),
              x = .,
              startRow = 6, startCol = 2, colNames = TRUE)
}

# Save workbook
saveWorkbook(wb_Public, GTI_PUBLIC, overwrite = TRUE)

  