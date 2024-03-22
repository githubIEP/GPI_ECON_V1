##### ----- GLOBAL TERRORISM INDEX 2024: CONTROL FILE
#' The purpose of this script is to allow for the basic functions of the GTI
#' (TT data cleaning, standard charts and tables etc.) to be created from a single location
##### -----

### --- Variables and Filepaths
GTI_PROJECT_FOLDER = paste0(IEP_USERPATH,"/Global Terrorism Index/2024 GTI")
MAP_PATH = ("04_outputs/maps/")
CHARTS_PATH = ("04_outputs/charts/")
TABLES_PATH = ("04_outputs/tables/")

GTI_LATEST_YEAR = 2023
GTI_PREVIOUS_YEAR = GTI_LATEST_YEAR - 1
GTI_FIRST_YEAR = 2007
GTI_BASE_YEAR = GTI_FIRST_YEAR + 4

GTI_COUNTRYLIST = "02_data/gti-countrynames.xlsx"

### --- Chart Settings

GTI_COLOURS <- c("#770A1E", "#F37053", "#ED1D24", "#FDC16E", "#F9B298", "#B7C9D3", "#678696", 
                 "#2D4F5E", "#D1D3D4", "#A6A593")

GTI_COLOURSCALE <- c("#B2DED1", "#f6e38d", "#FEC779", "#F37053", "#ed1c24", 
                       "#770A1E")

CHART_HEIGHT = 12
CHART_WIDTH = 16

### --- Run the Project

# Get the Data, Calculate the Index
source("03_scripts/cleaning/00_DatabasePull.R")    # Download the latest data from the database
source("03_scripts/cleaning/01_CleanTidy.R")       # Clean and Tidy The Data
source("03_scripts/cleaning/02_CalculateIndex.R")  # Calculate the Index Scores & Export Data

# Create the standard outputs that are in the report ever year: Maps, tables, charts
source("03_scripts/")




