##### ----- EXPORT CHARTS, MAPS AND TABLES
#' Note that for the export to work, the standardoutputs and analysis scripts have to 
#' be run first, in order to create the chart information, plot, and df objects

### --- Libraries and Variables

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)

### --- Section 1 --------------------------------------------------------------
#' Section 1 Charts: Note that country profile charts and maps are not stored in this file
#' Country profiles are exported using a loop in that specific .R file

# Create Workbook
wbCHARTS_SECTION1 <- createWorkbook()

# List
SECTION1_EXPORT <- c(
  "MAP_IndexMap","TABLE_IndexTable","MAP_AttacksMap", "TABLE_AttacksTable",
  "CHART_5Countries","CHART_wfDeaths","CHART_DeathDecreases","CHART_DeathIncreases",
  "CHART_TerroristGroups","TABLE_Top10Impacted")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("1", wbCHARTS_SECTION1, CHARTBOOK_1, SECTION1_EXPORT)

# Export Country Profile Chart Files
source("03_scripts/02_standardOutputs/02_Section1_CountryProfiles.R")

### --- Section 2 --------------------------------------------------------------

# Create Workbook
wbCHARTS_SECTION2 <- createWorkbook()

# Section 2 Charts: Note that Regional tables are not in this list and are added using a special function
SECTION2_EXPORT <- c(
  "CHART_TrendSummary","MAP_SignificantChanges","CHART_WestIdeology","CHART_DeathsDistribution",
  "CHART_DeathsConflict", "TABLE_RegionSummary","CHART_RegionTotals","CHART_Top3Regions")

# Regional Tables (Generated Dynamically)
SECTION2_DYNAMIC <- c("TABLE_RegionTables")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("2", wbCHARTS_SECTION2, CHARTBOOK_2, SECTION2_EXPORT)

# Dynamic Loops for Region Charts
for (active_region in REGIONS) {
  ActiveRegion <- active_region

  # Dynamically Update Chart_Information and DF
  TABLE_RegionTables["title"] <- paste0(ActiveRegion, " GTI Score, Rank and Change in Score, ", GTI_YEAR - 10, "-", GTI_YEAR)
  TABLE_RegionTables["sheet"] <- paste0("RegionTables_", ActiveRegion)
  #TABLE_RegionTables$counter <- as.character(RegionTableCounter)
  TABLE_RegionTables.df <- f_RegionalTable(RegionTables.df, active_region)
  
  # Export the chart or table
  f_ProjectExport("2",wbCHARTS_SECTION2, CHARTBOOK_2, SECTION2_DYNAMIC)
  
}

### --- Section 3 --------------------------------------------------------------

# Create Workbook
wbCHARTS_SECTION3 <- createWorkbook()


# Section 3 Charts: Properties of Terrorism
SECTION3_EXPORT <- c("CHART_CompDeaths","CHART_IsraelPoll", "CHART_ParetoEvents", "CHART_PowerPlot",
                      "CHART_PowerPlotCompare", "CHART_ParetoGroups", "CHART_GroupSurvival", "CHART_ActiveGroups",
                      "CHART_MLDeaths", "CHART_GPIscatter", "TABLE_GPIcorrelates", "CHART_PPIscatter",
                      "TABLE_PPIcorrelates")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("3", wbCHARTS_SECTION3, CHARTBOOK_3, SECTION3_EXPORT)

### --- Section 4 --------------------------------------------------------------

# Create Workbook
wbCHARTS_SECTION4 <- createWorkbook()

# Section 4 Charts: Organised Crime
SECTION4_EXPORT <- c(
  "CHART_OCScatter","MAP_OCTerror","DIAGRAM_TerrorTransition","CHART_TerrorismSahel",
  "CHART_SahelGold","CHART_SahelKidnappings","MAP_UNWithdrawal")

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("4", wbCHARTS_SECTION4, CHARTBOOK_4, SECTION4_EXPORT)

### --- Appendices -------------------------------------------------------------

# Create Workbook
wbCHARTS_APPENDICES <- createWorkbook()

# Section 4 Charts: Organised Crime
APPENDICES_EXPORT <- c(
  "TABLE_AppendixA", "TABLE_AppendixB"
)

# Reset Counters
figure_count = 0
table_count = 0

# Export Data
f_ProjectExport("Aopendices", wbCHARTS_APPENDICES, CHARTBOOK_APPENDICES, APPENDICES_EXPORT)
