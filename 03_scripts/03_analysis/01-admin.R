
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, WDI, readr, openxlsx, readxl, 
                DataCombine, countrycode, ggplot2)

options (scipen = 999)
#devtools::install_github("david-hammond/tidyindexR") #only need to be run once

#_________________________________________________________ADMIN SO CODES WORK_________________________________________________________________________________________________


# GPI data ----------------------------------------------------------------

#gpidata <- read_excel("Data/2021 GPI data.xlsx") %>% 
gpidata <- rio::import("Data/2023_data/Econ-costing-data-2023.xlsx") %>% 
  rename(iso3c=geocode, indicator = element)

gpidata <- gpidata %>% mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "Homicides per 100,000" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))

# function to get only GPI countries from WDI -------------------------------------------

pos <- unique(gpidata$iso3c)


# function to get data from WDI -------------------------------------------
get.wdi <- function(country,variable, start, end){
  data <- WDI::WDI(country=country,indicator = variable,start=start, end=end)
  data[,'country'] <- ifelse(data$country=="Eswatini","Swaziland",data$country)
  data[,'country'] <- ifelse(data$country=="Turkiye","Turkey",data$country)
  data[,'iso3c'] <- countrycode::countrycode(data[,'country'],"country.name", "iso3c")
  data <- subset(data,select=-iso2c)
  data <- dplyr::select(data, iso3c, country, year, everything())
  data <- subset(data, !is.na(iso3c))
  data <- data[data$iso3c %in% pos, ]
  #data <- subset(data, GPI!=0)
  #data <- subset(data,select=-GPI)
}



# iep.time.series.impute = function(index.data, extrapolate = F)
# {
#   require(zoo)
#   temp = group_by(index.data, iso3c, variablename)
#   temp = as.data.frame(summarise(temp, min.year = as.numeric(min(year)),
#                                  max.year = as.numeric(max(year)),
#                                  years = max.year - min.year))
#   #   temp = ddply(index.data, c("iso3c", "variablename"), summarise,
#   #                min.year = as.numeric(min(year)),
#   #                max.year = as.numeric(max(year)),
#   #                years = max.year - min.year)
#   temp = subset(temp, years > 0)
#   temp$id = paste(temp$iso3c, temp$variablename)
#   FilledData <- TimeFill(temp, GroupVar = 'id',
#                          StartVar = 'min.year', EndVar = 'max.year')
#   if (!extrapolate)
#   {
#     FilledData = subset(FilledData, TimeFilled == 1)
#   }
#   temp = merge(temp, FilledData)
#   temp = subset(temp, select=-c(id, min.year, max.year, years, TimeFilled))
#   names(temp) = c("iso3c", "variablename", "year")
#   temp = merge(temp, index.data, all.x = T)
#   temp = arrange(temp, variablename, year, country)
#   temp = group_by(temp, iso3c,variablename)
#   temp = as.data.frame(mutate(temp, time.imputed = na.approx(value, rule = 2)))
#   #   temp = ddply(temp, c("iso3c", "variablename"), transform,
#   #                time.imputed = na.approx(value, rule = 2))
#   temp$imputation = "time"
#   pos = which(temp$value == temp$time.imputed)
#   temp$imputation[pos] = "none"
#   temp = subset(temp, select=-value)
#   index.data$imputation = "none"
#   pos = which(names(temp)=="time.imputed")
#   names(temp)[pos] = "value"
#   index.data = rbind(temp[,names(index.data)], index.data)
#   pos = duplicated(index.data)
#   index.data = index.data[!pos,]
#   index.data$country = countrycode::countrycode(index.data$iso3c,"iso3c","country.name")
#   ##FILL IN BLANKS
#   index.data = index.data[with(index.data, order(iso3c, year)),]
#   vars = names(index.data)[!(names(index.data) %in% iep.standard.columns())]
#   for (i in vars)
#   {
#     index.data = FillDown(index.data, i)
#   }
#   index.data
# }



# iep.standard.columns = function()
# {
#   c("iso3c", "country", "year", "variablecode", "variablename", "value")
# }



# iep.expand.time.series = function(raw.data)
# {
#   require(zoo)
#   require(lubridate)
#   require(DataCombine)
#   #this calculates whether there is a time series or not
#   temp = raw.data %>% group_by(iso3c, variablename) %>%
#     summarise(min.year = as.numeric(min(year)),
#               max.year = as.numeric(max(year)),
#               years = max.year - min.year)
#   #temp = subset(temp, years > 0)
#   temp$id = paste(temp$iso3c, temp$variablename)
#   temp = as.data.frame(temp)
#   FilledData <- TimeFill(temp, GroupVar = 'id',
#                          StartVar = 'min.year', EndVar = 'max.year')
#   #FilledData = subset(FilledData, Time>2000) #may want to chnage this
#   temp = merge(temp, FilledData)
#   
#   temp$avgYears = with(temp, ifelse(Time <= min.year, min.year,
#                                     ifelse(Time >= max.year, max.year, (Time + 0.5))))
#   
#   temp = subset(temp, select=-c(id, min.year, max.year, years, TimeFilled))
#   names(temp) = c("iso3c", "variablename", "year", "avgYears")
#   temp = merge(temp, raw.data, all.x = T)
#   temp = arrange(temp, variablename, year, country)
#   temp$imputation = NA
#   temp$original.year = NA
#   temp$original.value = temp$value
#   temp$imputed = NA
#   num.values = temp %>% group_by(iso3c, variablename) %>% summarise(num = sum(!is.na(value)))
#   no.value = subset(num.values, num == 0)
#   add.ons = NULL
#   if(nrow(no.value)>0)
#   {
#     for (i in 1:nrow(no.value))
#     {
#       pos = with(temp,iso3c == no.value$iso3c[i] &
#                    variablename == no.value$variablename[i] )
#       add.ons = rbind(add.ons, temp[pos,])
#       temp = temp[!pos,]
#     }
#   }
#   
#   one.value = subset(num.values, num == 1)
#   if(nrow(one.value)>0)
#   {
#     for (i in 1:nrow(one.value))
#     {
#       #this fills in static data
#       pos = with(temp,iso3c == one.value$iso3c[i] &
#                    variablename == one.value$variablename[i] )
#       x = temp[pos,]
#       pos2 = !is.na(x$value)
#       x$value = x$value[pos2]
#       x$original.year = x$year[pos2]
#       x$imputation[pos2] = "none"
#       x$imputation[!pos2] = "static"
#       temp[pos,] = x
#     }
#   }
#   
#   temp = temp %>% group_by(iso3c, variablename) %>%
#     arrange(year) %>% mutate(imputed = na.approx(value, rule = 2))
#   pos = is.na(temp$imputation)
#   temp$imputation[pos] = "time"
#   pos = which(temp$value == temp$imputed & temp$imputation == "time")
#   temp$imputation[pos] = "none"
#   #fill in orignal years
#   pos = temp$imputation == "none"
#   temp$original.year[pos] = temp$year[pos]
#   pos = temp$imputation == "time"
#   
#   temp$original.year[pos] = temp$avgYears[pos]
#   
#   if (length(add.ons)>0)
#   {
#     add.ons$imputed = NA
#     temp = rbind(temp, add.ons[,names(temp)])####need to combine these
#   }
#   
#   pos = duplicated(temp)
#   temp = temp[!pos,]
#   temp$country = countrycode::countrycode(temp$iso3c, "iso3c","country.name")
#   temp$iso3c[temp$country=="Kosovo, Republic of"] <- "KSV"
#   temp$country[temp$iso3c=="KSV"] <- "Kosovo, Republic of"
#   temp = temp[!is.na(temp$country),]
#   temp = temp[temp$country != "NA",]
#   index.data  = as.data.frame(temp)
#   return(index.data)
# }


# update for 2022
# GPI grid ----------------------------------------------------------------
#gpi.country <- read_csv("Data/gpi.country.csv")
gpi.country <- read_csv("Data/2023_data/gpi.country.csv")
#gpi.grid <- expand.grid(year=c(2007:2021), iso3c=unique(gpi.country$iso3c))
gpi.grid <- expand.grid(year=c(2008:2022), iso3c=unique(gpi.country$iso3c))
gpi.grid <- subset(gpi.grid,!(iso3c=="PSE" & year<2014))
gpi.grid <- subset(gpi.grid,!(iso3c=="SSD" & year<2009))

# function to get data from WEO -------------------------------------------

# weovarselect <- function(data,subject, unit ){
#   data <- subset(data, Subject.Descriptor==subject & Units==unit)
#   data <- data[,c(2,4,10:52)]
#   data <- gather(data, year, value,-c(iso3c, country))
#   
#   data[,'value'] <- as.numeric(as.character(gsub(",","", data[,'value'])))
#   
#   data[,'year']<- as.numeric(as.character(gsub("X","", data[,'year'])))
#   
#   data <- subset(data, year>2006 & year<2018)
#   
#   data <- data[data$iso3c %in% pos, ]
#   
#   return(data)
# }


# see if any missing

# mis <- function(data){
#   pos[which(!(pos %in% data$iso3c ))] 
# }



# table to check missing data ---------------------------------------------

tab <- function(data){
  t <- with(data, table(iso3c, is.na(value)))
  View(t)
}

# lubridate::floor_date(lubridate::today(), "year") - lubridate::days(1)   # if you want to manually set max year for report each year; e.g., 2022 for this year's report


# Impute Function ===============================================================

index_data_pad<-function (df) 
{
  df = df %>% dplyr::mutate(date = as.Date(paste0(.data$year, 
                                                  "-01-01"), format("%Y-%m-%d")))
  df = df %>% dplyr::group_by(.data$geocode, .data$variablename) %>% 
    dplyr::arrange(.data$year) %>% padr::pad(interval = "year", 
                                             start_val = min(df$date), end_val = max(df$date)) %>% 
    dplyr::mutate(n = sum(!is.na(.data$value))) %>% dplyr::ungroup()
  pos = df$n == 1
  df1 = df[pos, ] %>% dplyr::group_by(.data$geocode, .data$variablename) %>% 
    dplyr::mutate(imputed = .data$value[!is.na(.data$value)][1]) %>% 
    dplyr::ungroup()
  df2 = df[!pos, ] %>% dplyr::group_by(.data$geocode, .data$variablename) %>% 
    dplyr::mutate(imputed = imputeTS::na_interpolation(.data$value)) %>% 
    dplyr::ungroup()
  df = rbind(df1, df2)
  df$year = lubridate::year(df$date)
  df$imputation_type = "Interpolated"
  df$imputation_type[df$value == df$imputed] = "Original Data"
  df = df %>% dplyr::select(.data$geocode, .data$year, .data$variablename, 
                            .data$value, .data$imputed, .data$imputation_type)
  return(df)
}



# Function to find missing data in a dataset ###################################

missing <- function(df){
  left_join(gpi.grid,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}

################ Region and Peace level ####################
# update for new GPI data
#based off score

#Peace_and_region <- read_excel("Data/Peace and region.xlsx", 
Peace_and_region <- read_excel("Data/2023_data/Peace and region.xlsx",                               
                               col_types = c("text", "text", "numeric"))

Peace_and_region <- Peace_and_region %>%  rename(`GPI overall score`=`2020 Peace level`)
#Peace_and_region <- Peace_and_region %>%  rename(`GPI overall score`=`2021 Peace level`)

Peace_and_region$peace_level = ifelse(Peace_and_region$`GPI overall score` <= 1.45, "Very High Peace", 
                                      ifelse(Peace_and_region$`GPI overall score` >= 1.45 & Peace_and_region$`GPI overall score` <= 1.9, "High Peace", 
                                             ifelse(Peace_and_region$`GPI overall score` >= 1.9 & Peace_and_region$`GPI overall score` <= 2.4, "Medium Peace",
                                                    ifelse(Peace_and_region$`GPI overall score` >= 2.4 & Peace_and_region$`GPI overall score` <= 2.9, "Low Peace",
                                                           ifelse(Peace_and_region$`GPI overall score` >= 2.9, "Very Low Peace", NA)))))
Peace_and_region <- Peace_and_region %>% select(-`GPI overall score`)



######################################################## POPULATION  ############################################################

pop <- gpidata %>% filter (indicator == "population") %>% rename (population = value) %>% select (-indicator)

pop <- pop %>% mutate (variablename = "population") %>% rename(value = population, geocode = iso3c)

pop <- index_data_pad(pop)

pop %<>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, population = imputed)



pop <- gpi.grid %>% left_join(pop)