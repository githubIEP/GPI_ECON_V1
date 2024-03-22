### --- Libraries
library(iepsqlite)
library(tidyverse)
library(ggplot2)
library(forcats)
library(scales)
library(ggpubr)
library(patchwork)
library(writexl)
library(openxlsx)
library(countrycode)
library(biscale)
library(cowplot)
library(gridGraphics)
library(readxl)
library(raster)
library(Rfast)
library(iepsqlite)
library(waterfalls)
library(stringr)
library(iepg)
library(WDI)

### --- Variables
MAP_PATH = ("04_outputs/maps/")
CHARTS_PATH = ("04_outputs/charts/")
TABLES_PATH = ("04_outputs/tables/")


MIN_YEAR=2007
CURRENT_YEAR = 2023
BASE_YEAR=CURRENT_YEAR-10
PREVIOUS_YEAR = CURRENT_YEAR - 1 
HEIGHT = 6
WIDTH = 8


gti_national <- rio::import("./02_data/processed/gti_banded_national.rds")
terrorism_df <- rio::import("./02_data/raw/cleaned-condensed-tt-data.rds")

acled_df <- iepg::iepg_acled()

# acled_df <- rio::import(paste0(IEP_USERPATH, "/Research/Data/ACLED/raw-acled-full-data.csv")) %>%
#   mutate(geocode = countrycode::countrycode(country, "country.name", "iso3c")) %>% 
#   mutate(geocode = ifelse(country == "Kosovo", "XKO", geocode))


gti_colours <- c("#770A1E", "#F37053", "#ED1D24", "#FDC16E", "#F9B298", "#B7C9D3", "#678696", 
                 "#2D4F5E", "#D1D3D4", "#A6A593")

gti_colours_scale <- c("#B2DED1", "#f6e38d", "#FEC779", "#F37053", "#ed1c24", 
                       "#770A1E")


theme_gti <- function() {
  
  theme_minimal() + 
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#888686", size = 12),
          axis.text = element_text(colour = "#888686", size = 12),
          axis.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()) 
  
}

label_formatter <- function(x) {
  ifelse(x < 0, paste0("-", abs(x)), as.character(x))
}
