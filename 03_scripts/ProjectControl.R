##### ----- GLOBAL TERRORISM INDEX 2024: CONTROL FILE
#' The purpose of this script is to allow the entire index to be run from a single script
##### -----

### --- Run the Project

# - 00 - Setting Variables and Functions ---------------------------------------
source("03_scripts/ProjectVariables.R")
source("03_scripts/ProjectFunctions.R")

# - 01 - Cleaning and Calculating ----------------------------------------------
source("03_scripts/01_cleaning/01_CleanConflict.R") # Download conflict data and create conflict variable
source("03_scripts/01_cleaning/02_MatchACLED.R") # Check Terrorism Tracker data against ACLED for conflict overlap

# At this point, check the ACLED matches to see if any events need to be removed

source("03_scripts/01_cleaning/03_GetTT.R") # Download Terrorism Tracker and Remove excluded data
source("03_scripts/01_cleaning/04_CleanTT.R") # Clean and Tidy Terrorism Tracker Data
source("03_scripts/01_cleaning/05_CalculateIndex.R") # Calculate the Index
source("03_scripts/01_cleaning/06_DataExport.R") # Export dashboard, web, and public release data
source("03_scripts/01_cleaning/07_MachineLearningPrepare.R") # Prepare data for ML model
# Code for Running Machine Learning Script

# - 02 - Standard Outputs ------------------------------------------------------
source("03_scripts/02_standardOutputs/00_ResultsMaps.R") # Standard Maps
source("03_scripts/02_standardOutputs/01_Section1_ChartsTables.R") # Tables and Charts from Section 1
source("03_scripts/02_standardOutputs/02_Section1_CountryProfiles.R") # Country Profiles from Section 1
source("03_scripts/02_standardOutputs/03_Section2_ChartsTables.R") # Tables and Charts from Section 2
source("03_scripts/02_standardOutputs/04_Appendices.R") # Tables and Charts from Section 2

# - 03 - Report Analysis - Note that this section will change each year --------
source("03_scripts/03_analysis/01_Section3_PropertiesofTerrorism.R") # Section 3 Charts and Tables
source("03_scripts/03_analysis/02_Section4_OrganizedCrime.R") # Section 3 Charts and Tables

# - 04 - Export Charts and Analysis --------------------------------------------
source("03_scripts/ProjectExport.R") # Export the analysis and charts for the report


