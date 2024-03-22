##### ----- Code Archive
#' For code that has been deprecated or removed because the data from the database
#' etc. changed structure. Not needed, but might be handy as a reference

# Code to split perpretrator, target, and weapons columns. Useful if we want group 2 etc.
%>%
  cSplit(c("perpetrators","weapons", "targets", "source_urls"), ";", "wide")   %>% # split columns
  cSplit(c("perpetrators_01", "perpetrators_02", "perpetrators_03"), ":", "wide") %>% # further split perpetrator data
  select(-matches("perpetrators_0[1-3]_1")) %>% # remove country variables from perpetrator set
  rename_with(~ sub("_2$", "_name", .x), matches("perpetrators_0[1-3]_2")) %>% # rename _2 to name
  rename_with(~ sub("_3$", "_type", .x), matches("perpetrators_0[1-3]_3")) %>% # rename _3 to type
  select(-matches("^(perpetrators|targets|weapons|source_urls)_0[3-9]$|^(perpetrators|targets|weapons|source_urls)_[1-9][0-9]$")) # remove perpetrators, targets, and weapons beyond the first two

cSplit(c("weapons", "targets"), ";", "wide")  %>% # split columns
  select(-matches("^(targets|weapons)_0[3-9]$|^(targets|weapons)_[1-9][0-9]$")) %>% # remove perpetrators, targets, and weapons beyond the first two
  rename(targets = targets_01, weapons = weapons_01) # rename targets and weapons
  
#--------------------------------------
# Code to export data directly to dashboard. This was ruining the dashboard formatting.

# Set wb export locations
wb_Dashboard = loadWorkbook(GTI_DASHBOARD)
s_Sheet = "DataIndex" # Worksheet for Index Summary Data
c_Sheet = "DataIncidents" # Worksheet for Incidents Data for Advanced Analysis

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