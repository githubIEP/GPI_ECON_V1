##### ----- EXPORT CHARTS, MAPS AND TABLES

### --- List of charts to include

# Section 1 Charts: Note that country profile charts and maps are not stored in this file
SECTION1_FIGURES <- c(
  "MAP_IndexMap","TABLE_IndexTable","MAP_AttacksMap", "TABLE_AttacksTable",
  "CHART_5Countries","CHART_wfDeaths","CHART_DeathDecreases","CHART_DeathIncreases",
  "CHART_TerroristGroups","TABLE_Top10Impacted")

# Section 2 Charts: Note that Regional tables are not in this list and are added using a special funciton
SECTION2_FIGURES <- c(
  "CHART_TrendSummary","MAP_SignificantChanges","CHART_WestIdeology","CHART_DeathsDistribution",
  "CHART_DeathsConflict", "TABLE_RegionSummary","CHART_RegionTotals")
  
# Section 3 Charts: Properties of Terrorism
SECTION3_FIGURES <- c("CHART_CompDeaths","CHART_IsraelPoll", "CHART_ParetoEvents", "CHART_PowerPlot",
                      "CHART_PowerPlotCompare", "CHART_ParetoGroups", "CHART_GroupSurvival", "CHART_ActiveGroups",
                      "CHART_MLDeaths", "CHART_GPIscatter", "TABLE_GPIcorrelates", "CHART_PPIscatter",
                      "TABLE_PPIcorrelates")

# Section 4 Charts: Organised Crime
SECTION4_FIGURES <- c(
  "CHART_OCScatter","MAP_OCTerror","DIAGRAM_TerrorTransition","CHART_TerrorismSahel",
  "CHART_SahelGold","CHART_SahelKidnappings","MAP_UNWithdrawal")


CHARTS_SECTION1 <- createWorkbook()
f_ChartExport(CHARTS_SECTION1,CHARTBOOK_1, SECTION1_FIGURES)

CHARTS_SECTION2 <- createWorkbook()
f_ChartExport(CHARTS_SECTION2,CHARTBOOK_2, SECTION2_FIGURES)

CHARTS_SECTION3 <- createWorkbook()
f_ChartExport(CHARTS_SECTION3,CHARTBOOK_3, SECTION3_FIGURES)

CHARTS_SECTION4 <- createWorkbook()
f_ChartExport(CHARTS_SECTION4,CHARTBOOK_4, SECTION4_FIGURES)

SECTION_TEST <-c("DIAGRAM_TerrorTransition")
CHARTS_SECTION_TEST <- createWorkbook()
f_ChartExport(CHARTS_SECTION_TEST,CHARTBOOK_TEST, SECTION_TEST)



f_GTISavePlots(DIAGRAM_TerrorTransition,pDIAGRAM_TerrorTransition)