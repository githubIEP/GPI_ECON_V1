##### ----- GLOBAL TERRORISM INDEX 2024: VARIABLE FILE
#' The purpose of this script is to have a single location to quickly change project settings
#' Project scripts are run from the ProjectControl.R file
##### -----

### --- Libraries---------------------------------------------------------------
#' Libraries will be loaded in each script using the f_LibraryLoader function.
#' This function checks to see if a package has already been loaded before loading it.

# Package Conflict Preferences
CONFLICT_PRIORITY = c(filter = "dplyr", select = "dplyr", lag = "dplyr")

### --- Filepaths --------------------------------------------------------------

# Key Locations
ONEDRIVE = paste0(IEP_USERPATH,"/Global Terrorism Index/",REPORT_YEAR," GTI") # Location of one drive report folder
IEP_NAMES = "02_data/gti-countrynames.xlsx" # Location of finalized list of country name spellings and associated data
GTI_DASHBOARD = paste0(ONEDRIVE,"/Collateral/GTI_Dashboard_",REPORT_YEAR,".xlsx") # Dashboard excel location
GTI_GROUPS = "02_data/processed/TT_group_classifications.xlsx" # GTI Group affiliations file
GTI_PUBLIC = paste0(ONEDRIVE,"/Collateral/GTI_PublicReleaseData_",REPORT_YEAR,".xlsx") # Public Data file

# Chartbooks
CHARTBOOK_1 = paste0(ONEDRIVE,"/Layout/GTI_Chartbook_Section1_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists
CHARTBOOK_2 = paste0(ONEDRIVE,"/Layout/GTI_Chartbook_Section2_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists
CHARTBOOK_3 = paste0(ONEDRIVE,"/Layout/GTI_Chartbook_Section3_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists
CHARTBOOK_4 = paste0(ONEDRIVE,"/Layout/GTI_Chartbook_Section4_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists
CHARTBOOK_APPENDICES = paste0(ONEDRIVE,"/Layout/GTI_Chartbook_Appendices_",REPORT_YEAR,".xlsx") # Data for recreating charts or sharing with journalists

# Output Locations
o_MAPS = ("04_outputs/maps/")
o_CHARTS = ("04_outputs/charts/")
o_TABLES = ("04_outputs/tables/")
CHART_FILES = paste0(ONEDRIVE,"/Layout/Charts")
IMAGE_FILES = paste0(ONEDRIVE,"/Layout/Images")
TABLE_FILES = paste0(ONEDRIVE,"/Layout/Tables")
MAP_FILES = paste0(ONEDRIVE,"/Layout/Maps")
ANALYSIS_FILES = paste0(ONEDRIVE,"/Analysis")


### --- Index Calculation ------------------------------------------------------

# Terrorism Tracker Cutoff dates for TT updates and TT pulls (so that the data won't have variables change after the cutoff date)
CUTOFF_DATA = as.Date('2023-12-31') # cutoff for events
CUTOFF_PULL = as.Date('2024-01-12') # cutoff for updates

# Years
GTI_YEAR = 2023                      # Latest year of GTI data (UPDATE EACH YEAR)
REPORT_YEAR = GTI_YEAR + 1           # Year that will be on the report cover
TT_FIRST_YEAR = 2007                 # First year of data in terrorism tracker (UPDATE IF NECESSARY)
GTI_FIRST_YEAR = TT_FIRST_YEAR + 4  # First year for which it is possible to calculate the GTI

# Event weights
e_WEIGHTS <- c(INCIDENTS = 1, DEATHS = 3, INJURIES = 0.5, HOSTAGES = 0.5)

# Year weights
y_WEIGHTS <- c(Y1 = .52, Y2 = .26, Y3 = .13, Y4 = .06, Y5 = .03)

# Scoring Bands
BANDED_MAX = 20000
LOG_BASE_SCORE = (BANDED_MAX + 1)^(1/10)

## -- Identity Variables
THE_SAHEL = c("NER", "BFA", "NGA", "MLI", "TCD", "MRT",
              "GMB", "SEN", "GIN", "CMR")

THE_WEST = c("AUS","AUT","BEL","CAN","DNK","FIN","FRA","DEU","ISL","IRL",
             "ITA","NLD","NZL","NOR","PRT","SVN","ESP","SWE","CHE","GBR",
             "USA")

THE_OECD = c("AUS","AUT","BEL","CAN","CHL", "COL", "CRI", "CZE", 
         "DNK", "EST","FIN","FRA","DEU", "GRC", "ISL","IRL", "ISR", "ITA","JPN", 
         "KOR", "LVA", "LTU", "LUX", "MEX", "NLD","NZL","NOR", "POL", "PRT", 
         "SVK", "SVN","ESP","SWE","CHE", "TUR ", "GBR", "USA")

# Variable to include non-GPI countries in event dataset. If set to "GPI", these countries will be removed
EVENT_INCLUDE = "GPI"

# Countries to check Terrorism and Conflict Overlap
CONFLICT_CHECK = c("MMR")
CONFLICT_CHECK_START = 2021

### --- Dictionaries for renaming variables

# Group Dictionary
dict_Groups <- c(
  "Al-Qaeda in the Islamic Maghreb (AQIM)" = "AQIM",
  "Revolutionary Armed Forces of Colombia (FARC)" = "FARC",
  "Communist Party of Nepal-Maoist (CPN-M)" = "CPN-M",
  "Communist Party of India-Marxist-Leninist (CPI-ML)" = "CPI-ML",
  "Al-Qaeda in the Arabian Peninsula (AQAP)" = "AQAP",
  "Jamaat Nusrat Al-Islam wal Muslimeen (JNIM)" = "JNIM"
)

# Target Dictionary - Add to as needed
dict_Targets <-c(
  "Government/Political" = "Government",
  "Civilians/General Public" = "Civilians",
  "Government/Political Parties" = "Political Party",
  "Law Enforcement/Internal Security Forces" = "Police"
)

dict_Ideology <- c("Far Left/Revolutionary" = "Political",
                   "Far Right/Extreme Right" = "Political",
                   "Nationalist/Separatist" = "Nationalist",
                   "Other Religious Extremist/Cultist" = "Religious",
                   "Reactionary/Pro-Regime" = "Nationalist",
                   "National Islamist" = "Nationalist/Religious",
                   "Global Islamist" = "Religious"
                   )

## -- Conflict Variables
VER_GED = 231        # Current version of GED, eg. 23.1 = 231
VER_CANDIDATE = 23   # Current version of UCDP candidate event dataset


## -- Output Variables ---------------------------------------------------------

# Chart and Map Colours
GTI_COLOURS <- c("#770A1E", "#F37053", "#ED1D24", "#FDC16E","#678696", "#F9B298", "#B7C9D3",
                 "#2D4F5E", "#D1D3D4", "#A6A593")

GTI_COLOURS_SCALE <- c("#D1D3D4","#B2DED1", "#f6e38d", "#FEC779", "#F37053", "#ed1c24", 
                       "#770A1E")

# Chart Sizes
GTI_CHARTS <- list(
  small = c(width = 8.45, height = 10),
  medium = c(width = 12, height = 10),
  large = c(width = 17.6, height = 10)
)

GTI_MAPS <- list(
  small = c(width = 12, height = 8),
  medium = c(width = 14, height = 10),
  large = c(width = 28, height = 14)
)

CHART_UNIT = "cm"

# Chart Fonts
HEAVY_FONT = "Helvetica LT Pro" 
LIGHT_FONT = "Helvetica LT Pro Light" 

# Commonly used sources
SOURCE_TT = "Terrorism Tracker, IEP Calculations"

# Overall Score Bands
SCORING_BANDS <- c(0, 2, 4, 6, 8)

# Indicator Bands
WEB_BANDS <- c(0, 5, 20, 50, 100, 500) # All indicators other than hostages
HOSTAGE_BANDS <- c(0, 2, 10, 25, 50, 100) # Bands specific to the hostage indicator


