
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, WDI, readr, openxlsx, readxl, 
               DataCombine, countrycode, ggplot2)

options (scipen = 999)
#devtools::install_github("david-hammond/tidyindexR") #only need to be run once

#_________________________________________________________ADMIN SO CODES WORK_________________________________________________________________________________________________


# GPI data ----------------------------------------------------------------

#gpidata <- read_excel("Data/2021 GPI data.xlsx") %>% 
gpidata <- rio::import("02_data/processed/Econ-costing-data-2023.xlsx") %>% 
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


# GPI grid ----------------------------------------------------------------
#gpi.country <- read_csv("Data/gpi.country.csv")
gpi.country <- read_csv("02_data/processed/gpi.country.csv")
#gpi.grid <- expand.grid(year=c(2007:2021), iso3c=unique(gpi.country$iso3c))
gpi.grid <- expand.grid(year=c(2008:2022), iso3c=unique(gpi.country$iso3c))
gpi.grid <- subset(gpi.grid,!(iso3c=="PSE" & year<2014))
gpi.grid <- subset(gpi.grid,!(iso3c=="SSD" & year<2009))



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
#based off score

Peace_and_region <- read_excel("02_data/processed/Peace and region.xlsx",                               
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




# Getting GDP per cap PPP Constant, GDP current, GDP Constant, GDP PPP Constant, GDP Deflator from WDI

wdi.all <- get.wdi("all",c("NY.GDP.PCAP.PP.KD", "PA.NUS.PPPC.RF", "NY.GDP.DEFL.ZS",
                           "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD"),
                   2007,2022) %>% mutate(year = year+1)

wdi.data <- wdi.all %>% 
  rename('GDP per cap PPP Constant 2017' = NY.GDP.PCAP.PP.KD,  
         'PPP Conversion Factor' = PA.NUS.PPPC.RF, 'GDP Deflator' = NY.GDP.DEFL.ZS, 
         'GDP per cap Current' = NY.GDP.PCAP.CD,'GDP per cap Constant' = NY.GDP.PCAP.KD)




wdi.data <- gpi.grid %>% left_join(wdi.data, by = c("iso3c", "year"))

wdi.data$country[wdi.data$iso3c == "KSV"] <- "Kosovo"
wdi.data$country[wdi.data$iso3c =="TWN"] <- "Taiwan"

wdi.data <- wdi.data %>% group_by (iso3c, country, year) %>% pivot_longer(cols = c(4:8)) %>%
  rename (variablename = name, geocode = iso3c) %>% ungroup()



# Imputation

wdi.data.impute <- wdi.data %>% select (geocode, year, variablename, value) %>%
  filter(complete.cases(value))

wdi.grid <- wdi.data %>% select (geocode, year, variablename)

wdi.data.impute <- wdi.data.impute %>%
  index_data_pad() %>% 
  right_join(wdi.grid, by = c("geocode", "year", "variablename")) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    variablename,
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    variablename, 
    geocode) %>%
  # # mutate(imputed2 = imputeTS::na_interpolation(imputed)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))


wdi.data <- wdi.data.impute

rm(wdi.data.impute, wdi.grid)

# =======================================================================================================================================

## GDP Values

gdp.wdi <- wdi.data %>% filter (variablename %in% c("GDP per cap Current", "GDP per cap Constant", "GDP per cap PPP Constant 2017"))

gdp.wdi <- gdp.wdi %>% rename (iso3c = geocode) %>% left_join (pop, by = c("iso3c", "year"))

gdp.wdi <- gdp.wdi %>% mutate (value = value * population) %>% select (-population)

gdp.wdi <- gdp.wdi %>% group_by (iso3c, year ) %>% pivot_wider(names_from = variablename) %>%
  rename (gdp = 'GDP per cap Current', gdpcons = 'GDP per cap Constant', gdpconsppp = 'GDP per cap PPP Constant 2017') %>%
  select (iso3c, year, gdp, gdpcons, gdpconsppp)

gdp.wdi <- gdp.wdi %>% mutate (gdpcons = 1 / .87 * gdpcons) # turning WDI constant 2015 to constant 2022




# =====================================================================================================================================

## GDP per cap Constant

gdp.pc.constant <- wdi.data %>% filter (variablename == "GDP per cap Constant")

gdp.pc.constant <- gdp.pc.constant %>% select (geocode, year, value) %>% rename (iso3c = geocode, gdp.pc.cons = value)

gdp.pc.constant <- gdp.pc.constant %>% mutate (gdp.pc.cons = 1 / .87 * gdp.pc.cons) # turning constant 2015 to constant 2022

# ==================================================================================================================================


## GDP Deflator

deflator <- wdi.data %>% filter (variablename == "GDP Deflator")

deflator <- deflator %>% select (geocode, year, value) %>% rename (iso3c = geocode)


deflator_2022 <- deflator %>% filter (year == 2022)

deflator <- deflator %>% left_join(deflator_2022, by = "iso3c")

deflator <- deflator %>% select (iso3c, year.x, value.x, value.y) %>%
  mutate (value.x = value.x / value.y) %>%
  rename (value = value.x, year = year.x) %>%
  select (iso3c, year, value)

deflator <- deflator %>% rename (deflator = value)

deflator <- deflator %>% group_by(year) %>% summarize (deflator = median(deflator)) # using median deflator for all countries

rm(deflator_2022)

# ==============================================================================================================================

## PPP Conversion Factor
ppp.conv <- wdi.data %>% filter (variablename == "PPP Conversion Factor")

ppp.conv  <- ppp.conv  %>% select (geocode, year, value) %>% rename (iso3c = geocode)

# ========================================================================================================================

## PPP US Ratio

wdi.gdpc.ppp <- wdi.data %>% filter (variablename == "GDP per cap PPP Constant 2017")

wdi.gdpc.ppp  <- wdi.gdpc.ppp  %>% select (geocode, year, value) %>% rename (iso3c = geocode)



wdi.gdpc.ppp <- wdi.gdpc.ppp %>% rename(gdpc.ppp.con = value)

ppp_us <- wdi.gdpc.ppp %>% filter (iso3c == "USA")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% left_join(ppp_us, by="year")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% mutate (scale = gdpc.ppp.con.x/gdpc.ppp.con.y)

ppp <- wdi.gdpc.ppp %>% select (c(1, 2, 6)) %>% rename(iso3c = iso3c.x)

rm(wdi.gdpc.ppp, ppp_us, wdi.data, wdi.all)

# =========================================================================================================

# UNIT COST

unitcost <- read_excel("02_data/processed/unit costs for gpi2022.xlsx", 
                       sheet = "unit costs r")
unitcost <- within(unitcost, indicator <- paste(Indicator, type,sep='.'))
unitcost <- unitcost[,c("indicator", "unitcost")]

unitcost <- spread(unitcost, indicator, unitcost)


unitcost2 <- cbind(ppp, unitcost)

unitcost.scaled <- mutate_at(unitcost2,vars(3:15), funs(.*scale) )

unitcost.scaled <- unitcost.scaled[,c(2,1,4:15)]


############# Automate Vet affairs and interest

################## https://www.whitehouse.gov/omb/historical-tables/  ####################

temp = tempfile(fileext = ".xlsx")
#dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2021/05/hist05z1_fy22.xlsx"   ## UPDATE GPI 2022   Update the link to Table 5.1—Budget Authority by Function and Subfunction: 1976–2025
dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2022/03/hist05z1_fy2023.xlsx" 
download.file(dataURL, destfile=temp, mode='wb')

test <- readxl::read_excel(temp, skip =2)
test <-test %>%  subset(`Function and Subfunction`=="Total, Veterans Benefits and Services") %>%
  rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`, `2026`=`2026 estimate`, `2027`=`2027 estimate`) %>% 
  gather(year, value, -c(`Function and Subfunction`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6)



#Update the link to Table 6.1—Composition of Outlays: 1940–2027
temp = tempfile(fileext = ".xlsx")
#dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2021/05/hist06z1_fy22.xlsx"      ## UPDATE GPI 2022 Update the link to Table 6.1—Composition of Outlays: 1940–2025
dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2022/03/hist06z1_fy2023.xlsx"
download.file(dataURL, destfile=temp, mode='wb')


test_interest <- readxl::read_excel(temp, skip =1)
test_interest <- test_interest[9,]

test_interest <- test_interest %>% rename(`2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`,`2026`=`2026 estimate`, `2027`=`2027 estimate`, ) %>% 
  gather(year, value, -c(`Category`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6) %>%
  mutate(interest=value*0.2) %>% select(year, interest)


vet_tmp <- left_join(test,test_interest)
vet_tmp <- vet_tmp %>%subset(year>2006) %>% mutate(value=value+interest) %>% select(year, value) %>%subset(year<2023)
vet_tmp$iso3c = "USA"


vet_tmp <- vet_tmp %>% full_join(gpi.grid, by=c("iso3c","year")) %>% 
  mutate(value=ifelse(is.na(value),0,value))



vet_tmp <- subset(vet_tmp,!(iso3c=="PSE" & year<2015))
vet_tmp <- subset(vet_tmp,!(iso3c=="SSD" & year<2010))

vet_tmp <- vet_tmp %>%  rename(vet.int=value)

vet = vet_tmp

vet <- gpi.grid %>% left_join(vet, by = c("year", "iso3c"))

vet <- vet %>% rename (geocode = iso3c, value = vet.int) %>% mutate (variablename = "Veteran Costs")

vet <- index_data_pad(vet)

vet <- vet %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, vet.int = imputed)

vet <- gpi.grid %>% left_join(vet)

#_________________________________________________________SUICIDE RATE_________________________________________________________________________________________________

########### WORLD BANK SUICIDE RATE ################


#suicide <- get.wdi("all","SH.STA.SUIC.P5",2007,2021) %>% 
suicide <- get.wdi("all","SH.STA.SUIC.P5",2006,2022) %>% 
  mutate (year = year + 1) %>%
  rename(rate=SH.STA.SUIC.P5) %>% rename(value=rate) %>% mutate(variablename="suicide_rate") %>%
  subset(!iso3c=="PSE") %>% subset(!iso3c=="KSV") %>% subset(!iso3c=="TWN")


suicide <- left_join(gpi.grid, suicide)

# Checking what countries missing all country-year values 

suicide %>% group_by(iso3c) %>% summarize(count = sum(is.na(value))) %>% arrange(desc(count))

# Palestine Kosovo and Taiwan missing all country-year values

suicide <- suicide %>% rename(geocode=iso3c) %>% mutate(variablename = "suicide rate") %>% 
  subset(!geocode %in% c("KSV", "PSE", "TWN"))


suicide <- index_data_pad(suicide)

suicide <- suicide %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)


suicide <- gpi.grid %>% left_join(suicide) %>% mutate (variablename = "suicide rate")



# Using regional average for Kosovo Taiwan & Palestine

suicide.region.average <- suicide %>%  left_join(Peace_and_region) %>% select (-c (6))
suicide.region.average <- suicide.region.average %>% group_by(region,year) %>%
  summarise(average=mean(value, na.rm=T))



suicide <- suicide %>%  left_join(Peace_and_region, by = "iso3c")

suicide <- suicide   %>% left_join(suicide.region.average, by = c("region", "year"))

suicide <- suicide %>% mutate (value = coalesce(value, average)) %>% select (c(1:3))


rm(suicide.region.average, suicide_GBD)



suicide <- subset(suicide,!(iso3c=="PSE" & year<2014))
suicide <- subset(suicide,!(iso3c=="SSD" & year<2009))

suicide <- suicide %>% left_join(pop) %>%  distinct() %>% mutate(value=(population/100000*value)) %>% select(-population) 
suicide <- suicide %>% mutate(variablename="suicide_count") %>% select(iso3c,value, year) %>%  rename(suicidevalue=value)


#######################      fear    ####################### 

# Per capita data 

fear <- gpidata %>%
  subset(indicator=="perceptions of criminality") %>%   rename_all(tolower) %>%
  select("iso3c", "year", "value")  %>%
  subset(!year==2023) %>%
  mutate(variablename="fear")

fear <- gpi.grid %>% left_join(fear) %>% rename (geocode = iso3c)

fear <- index_data_pad(fear)


fear <- fear %>% select (geocode, year, imputed ) %>% 
  rename (iso3c = geocode, fear = imputed) %>% 
  right_join(gpi.grid)


# Estimating for each country

fear <- fear %>% subset(select=c("iso3c", "year", "fear")) %>% subset(!is.na(fear)) %>%
  merge(pop[,c("iso3c","year","population")], by=c("iso3c", "year")) %>% mutate(fearvalue= fear*population)%>% 
  subset(select=c("iso3c",  "year", "fearvalue")) %>% rename(fear=fearvalue)

rm(fear.region.average)
rm(fear.peace.level)

########### UNHCR funding #########

########### Prior to 2018 #########

unhcr<- read_csv("02_data/processed/Refugees and IDP costs unhcr 2020.csv") %>%
  gather(year,unhcr,-iso3c) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year))))


unhcr<- unhcr %>% mutate(unhcr=as.numeric(as.character(unhcr)))

UNHCR_2021 <- read_excel("02_data/processed/Global Funding Overview 31 December 2021-converted.xlsx", 
                         sheet = "data", col_types = c("text", 
                                                       "numeric"))

UNHCR_2021$iso3c = countrycode::countrycode(UNHCR_2021$country, "country.name", "iso3c")
UNHCR_2021$iso3c[UNHCR_2021$country=="Kosovo"] <- "KSV"



UNHCR_2021 <- na.omit(UNHCR_2021)
UNHCR_2021 <- UNHCR_2021[UNHCR_2021$iso3c %in% pos,]
UNHCR_2021 <- UNHCR_2021 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2021$year = 2021
UNHCR_2021 <- rename(UNHCR_2021, unhcr=total)
UNHCR_2021 <- UNHCR_2021 %>% select(iso3c,year,unhcr)

unhcr <- unhcr %>% rbind(UNHCR_2021) 

unhcr <- unhcr[unhcr$iso3c %in% pos,]
unhcr <- unhcr %>% na.omit() #%>% mutate(year=year-1)
unhcr <- gpi.grid %>% left_join(unhcr) 


unhcr <- unhcr %>% rename (geocode = iso3c, value = unhcr) %>% mutate (variablename = "unhcr")

unhcr <-  index_data_pad(unhcr)

unhcr <- unhcr %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, unhcr = imputed)

unhcr <- gpi.grid %>% left_join(unhcr)


#################################### Violent assault ###################################

# violent.assault <- read.csv("02_data/processed/assault_data.xlsx") %>% 
violent.assault <- rio::import("02_data/processed/assault_data.xlsx") %>%
  rename(country=Country, year=Year, value=Rate) %>%
  select(country, year, value) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% subset(year>2006)  %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)")

violent.assault = violent.assault %>% mutate(value = round(value,2))




violent.assault$iso3c <- countrycode::countrycode(violent.assault$country, "country.name","iso3c")
violent.assault$iso3c[violent.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
violent.assault$country[violent.assault$iso3c=="KSV"] <- "Kosovo"
violent.assault$variablename <- "violent assault"
violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]

#work out who is missing data
violent.assault <- gpi.grid %>% left_join(violent.assault)
tmp <- missing(violent.assault)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))

# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==8) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==15) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

violent.assault <- violent.assault %>% left_join(count_of_missing)

violent.assault <- violent.assault %>% subset(!new=="delete")

# We can now impute given what remains has at least one value
violent.assault <- violent.assault %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="violent assault")

violent.assault <- index_data_pad(violent.assault)

violent.assault <- violent.assault %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

## Average by peace and region

violent.assault.average <- violent.assault %>%  left_join(Peace_and_region)
violent.assault.average <- violent.assault.average %>% group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
violent.assault.peace.level <- violent.assault %>%  left_join(Peace_and_region)
violent.assault.peace.level <- violent.assault.peace.level %>% select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))

##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

violent.assault <- gpi.grid %>% left_join(violent.assault)

##### From who is missing allocate the average rate by peace level and region

tmp <- missing(violent.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)

tmp <- tmp %>% left_join(violent.assault.average) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="violent assault") %>% na.omit() 

#ADD to main DF

violent.assault <- violent.assault %>% na.omit() %>% rbind(tmp) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
violent.assault <- gpi.grid %>% left_join(violent.assault)

tmp <- missing(violent.assault)


#If data still does not have a match so I will take the average of the peace level #

tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% left_join(violent.assault.peace.level) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="violent assault")


# The 'additional' missing data needs to be added in 

violent.assault <- violent.assault %>% na.omit() %>% rbind(tmp)

violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]


rm(violent.assault.average)
rm(violent.assault.peace.level)


#################### Sexual violence #############################
# Not in GPI data 


#sexual.assault <- read_csv("Data/sexual_violence.csv") %>% 
sexual.assault <- rio::import("02_data/processed/sexual_violence.xlsx") %>% 
  rename(country=Country, year=Year, value="Measure Values", rate="Measure Names") %>% 
  select(country, year, value, rate) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% 
  subset(year>2006)  %>%
  #subset(year>2007)  %>% 
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)") %>%
  subset(!rate=="Count")

sexual.assault = sexual.assault %>% mutate(value = round(value,2))

sexual.assault$iso3c <- countrycode::countrycode(sexual.assault$country, "country.name","iso3c")
sexual.assault$iso3c[sexual.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
sexual.assault$country[sexual.assault$iso3c=="KSV"] <- "Kosovo"
sexual.assault$variablename <- "sexual assault"
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]


#work out who is missing data
sexual.assault <- gpi.grid %>% left_join(sexual.assault)
tmp <- missing(sexual.assault)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))

# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==9) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==15) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

sexual.assault <- sexual.assault %>% left_join(count_of_missing)

#delete out all that dont have at least one variable!  
sexual.assault <- sexual.assault %>% subset(!new=="delete")

# We can now impute given what remains has at least one value
sexual.assault <- sexual.assault %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="sexual assault")

sexual.assault <- index_data_pad(sexual.assault)

sexual.assault <- sexual.assault %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

## Average by peace and region

sexual.assault.average <- sexual.assault %>%  left_join(Peace_and_region)
sexual.assault.average <- sexual.assault.average %>% group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
sexual.assault.peace.level <- sexual.assault %>%  left_join(Peace_and_region)
sexual.assault.peace.level <- sexual.assault.peace.level %>% select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

sexual.assault <- gpi.grid %>% left_join(sexual.assault)

##### From who is missing allocate the average rate by peace level and region

tmp <- missing(sexual.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)

tmp <- tmp %>% left_join(sexual.assault.average) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="sexual assault") %>% na.omit() 

#ADD to main DF

sexual.assault <- sexual.assault %>% na.omit() %>% rbind(tmp) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
sexual.assault <- gpi.grid %>% left_join(sexual.assault)

tmp <- missing(sexual.assault)


#If data still does not have a match so I will take the average of the peace level #

tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% left_join(sexual.assault.peace.level) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="sexual assault")


# The 'additional' missing data needs to be added in 

sexual.assault <- sexual.assault %>% na.omit() %>% rbind(tmp)

sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]


rm(sexual.assault.average)
rm(sexual.assault.peace.level)


# sexual.assault <- rbind(sexual.assault, sexual.assault_missing)
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]
sexual.assault <- subset(sexual.assault,!(iso3c=="PSE" & year<2014))
sexual.assault <- subset(sexual.assault,!(iso3c=="SSD" & year<2009))



##### TURN RATE TO COUNT FOR BOTH SEXUAL AND VIOLENT ASSAULT
# Violent and sexual assault together -------------------------------------
crime <- as.data.frame(rbind(violent.assault, sexual.assault)) 

crime <- crime%>% left_join(pop)

crime <- crime %>% mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value, variablename)



crime = crime %>% mutate(value = round(value,3))

crime2 <- crime %>% group_by(iso3c, year,variablename) %>% 
  summarise(value=sum(value)) %>% ungroup()


rm(crime, violent.assault.tmp, violent.assault_missing, violent.assault_missing_tmp, violent.assault_regional_average, sexual.assault.tmp, sexual.assault_missing,
   sexual.assault_missing_tmp, sexual.assault_regional_average, tmp, fear.region.average)


small.arms <- read_excel("02_data/processed/Small arms survey data 2020.xlsx", sheet = "clean") %>%  
  mutate(value = value * 1000000)


small.arms <- small.arms %>% mutate(year=as.numeric(as.character(gsub("X","", year)))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) %>%
  mutate (year = year + 2)



small.arms <- gpi.grid %>% left_join(small.arms) %>% select (-Country) 

small.arms <- small.arms %>% rename (geocode = iso3c) %>% filter (complete.cases(value)) 

country_year <- expand.grid(geocode = unique(small.arms$geocode), year = c(2008, 2009), value = NA) # adding back year 2008 and 2009

small.arms <- small.arms %>% rbind (country_year) %>% 
  mutate (variablename = "small arms")

rm (country_year)


small.arms <- index_data_pad(small.arms)

small.arms <- small.arms %>% select (geocode, year, imputed) %>% 
  rename (iso3c = geocode, sarms = imputed) %>%
  right_join(gpi.grid) %>%
  mutate (sarms = case_when(is.na(sarms) ~ 0,
                            TRUE ~ sarms ))



# Security Agency

secu.agnecy <- read.csv("02_data/processed/security agency costs 2018gpi.csv", stringsAsFactors = FALSE) %>%
  mutate(X2007=X2008, X2019=X2018,X2020=X2018,X2021=X2018, X2022=X2018)%>%
  # mutate( X2019=X2018,X2020=X2018,X2021=X2018, X2022=X2018)%>% 
  mutate(iso3c=countrycode(country,"country.name","iso3c")) %>%
  gather(year, secu.agnecy, -c(country, iso3c))%>% 
  mutate(year=as.numeric(as.character(gsub("X","",year))))

secu.agnecy$iso3c[secu.agnecy$country=="Kosovo"] <- "KSV"

secu.agnecy <- secu.agnecy[secu.agnecy$iso3c %in% pos, ]    


secu.agnecy <- subset(secu.agnecy,select=c(iso3c,year,secu.agnecy))

secu.agnecy <- gpi.grid %>% left_join(secu.agnecy)
secu.agnecy <- subset(secu.agnecy,!(iso3c=="PSE" & year<2014))
secu.agnecy <- subset(secu.agnecy,!(iso3c=="SSD" & year<2009))


# using regional average for South Sudan

secu.agnecy.region.average <- secu.agnecy %>%  left_join(Peace_and_region) %>% select (-c (5))
secu.agnecy.region.average <- secu.agnecy.region.average %>% group_by(region,year) %>%
  summarise(average=mean(secu.agnecy, na.rm=T))



secu.agnecy <- secu.agnecy %>%  left_join(Peace_and_region, by = "iso3c")

secu.agnecy <- secu.agnecy   %>% left_join(secu.agnecy.region.average, by = c("region", "year"))

secu.agnecy <- secu.agnecy %>% mutate (secu.agnecy = coalesce(secu.agnecy, average)) %>% select (c(1:3))


pb <- read.csv("02_data/processed/peacebuilding 2022.csv") %>%   
  mutate(iso3c=countrycode(Recipient,"country.name","iso3c")) %>%  mutate(iso3c=ifelse(Recipient=="Kosovo","KSV",iso3c))

pb <- pb[pb$iso3c %in% pos,]


# Check that that coutnrys are the ones we actually want
country.list <- as.data.frame(table(pb$Recipient))

rm(country.list)

pb.countries <- read_excel("02_data/processed/Peacebuilding Fund - Projects by Country.xlsx")

pb.countries <- pb.countries %>%  mutate(iso3c=countrycode(Countries,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Countries=="Kosovo","KSV",iso3c)) %>%
  distinct(iso3c) %>%
  mutate(country=countrycode(iso3c,"iso3c", "country.name")) %>% mutate(country=ifelse(iso3c=="KSV","Kosovo",country))

pb <- left_join(pb.countries,pb, by="iso3c" )



pb <- pb %>% subset(Donor=="Official Donors, Total") %>% 
  subset(Channel=="All Channels") %>% 
  subset(Flow=="Official Development Assistance") %>% 
  subset(Type.of.aid=="All Types, Total") %>% 
  subset(Value>0) %>% 
  subset(Amount.type=="Current Prices") %>% 
  subset(SECTOR=c("15210","15240","15250","15261","15230","15220","15130","15152","15113","15150","15153","15160","15170","15110","15111","15112","15151")) %>% 
  subset(Flow.type=="Gross Disbursements") 


pb <- pb %>% mutate(Year = Year+2) %>% group_by(Year, iso3c, Recipient) %>% 
  summarise(value=sum(Value, na.rm = TRUE)) %>% mutate(variablename = "peacebuilding") %>%
  rename(country=Recipient) %>%
  rename(year = "Year") 


pb <- subset(pb,!(iso3c=="PSE" & year<2015))
pb <- subset(pb,!(iso3c=="SSD" & year<2010))


pb$value <- pb$value*10^6

peacebuilding <- pb %>% select(iso3c,year,value) %>% rename(peacebuilding=value) %>% 
  subset(year > 2006)
rm(pb.countries)
peacebuilding <- gpi.grid %>% left_join(peacebuilding) 
peacebuilding <- peacebuilding %>% mutate (peacebuilding = case_when(is.na(peacebuilding) ~ 0,
                                                                     TRUE ~ peacebuilding ))


#_________________________________________________________ # private security _________________________________________________________________________________________________

priv.secu <- read_csv("02_data/processed/private security numbers updated 2021.csv") %>% 
  select(-`...1`) %>%
  gather(year,value, -c("country","iso3c")) %>% select(-country)



priv.secu <-   priv.secu %>% mutate(value=as.numeric(as.character(gsub(",","",value)))) %>%
  mutate(year=as.numeric(as.character(gsub(",","",year)))) 


priv.secu <- left_join(gpi.grid,priv.secu) %>% na.omit()


#Add in population
tmp <- pop %>% select(iso3c, year, population)

priv.secu <- left_join(priv.secu, tmp) %>% mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value) 

tmp <- priv.secu %>%  merge(unitcost.scaled[,c("iso3c","year", "privtsecurity.direct")], by=c("iso3c","year")) %>%
  mutate(privsecu.cost=privtsecurity.direct*value) %>%
  subset(select=c(iso3c,year, privsecu.cost)) %>%
  full_join(gpi.grid, by=c("iso3c","year")) %>%
  mutate(privsecu.cost=ifelse(is.na(privsecu.cost),0,privsecu.cost))


priv.secu = tmp

# Internal security  ------------------------------------------------------

pos.exp <- read_excel("02_data/processed/expenditure on public order and safety IMF.xlsx")


pos.exp <- pos.exp %>%
  rename(variable.name="COFOG Function", country="Country")


pos.exp <- pos.exp %>% gather(year, value, -c(country, variable.name)) %>% mutate(value=as.numeric(as.character(value))) %>% 
  mutate(year=as.numeric(as.character(year))) %>%  mutate(year=year+1) 



pos.exp <- pos.exp%>%  mutate(iso3c=countrycode(country,"country.name","iso3c"))
pos.exp$iso3c[pos.exp$country=="Kosovo, Republic of"] <- "KSV"
pos.exp$iso3c[pos.exp$country=="Eswatini, Kingdom of"] <- "SWZ"
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]


pos.exp <- pos.exp %>% select(iso3c, year, value, country) %>%
  mutate(variablename="public order and safety") %>%
  subset(!(year<2005))
pos.exp <- pos.exp %>% mutate(value=round(value,1))



gpi.grid.tmp <- gpi.grid %>% mutate(count=1) %>% spread(year,count) %>% 
  gather(year, count, -iso3c) %>% select(year,iso3c) %>% mutate(year=as.numeric(as.character(year)))

gpi.grid.tmp <- subset(gpi.grid.tmp,!(iso3c=="PSE" & year<2014))
gpi.grid.tmp <- subset(gpi.grid.tmp,!(iso3c=="SSD" & year<2009))

#work out who is missing data
pos.exp <- gpi.grid.tmp %>% left_join(pos.exp)



missing2 <- function(df){
  left_join(gpi.grid.tmp,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}
### MISSING IS USING WRONG GRID
tmp <- missing2(pos.exp)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))



for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==9) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==15) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

pos.exp <- pos.exp %>% left_join(count_of_missing)

#delete out all that dont have at least one variable!  
pos.exp <- pos.exp %>% subset(!new=="delete")

# We can now impute given what remains has at least one value
pos.exp <- pos.exp %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="public order and safety")

pos.exp <- index_data_pad(pos.exp)

pos.exp <- pos.exp %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

#pos.exp <- pos.exp[pos.exp$year>2005,]
pos.exp <- pos.exp[pos.exp$year>2006,]
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]
pos.exp <- subset(pos.exp,!(iso3c=="PSE" & year<2014))
pos.exp <- subset(pos.exp,!(iso3c=="SSD" & year<2009))
pos.exp <- subset(pos.exp,!(year<2006))
#pos.exp <- subset(pos.exp,!(year<2007))

pos.exp <- pos.exp %>% 
  merge(gdp.wdi[,c("iso3c","year","gdp")],by=c("iso3c","year")) %>% 
  mutate(value=value/100) %>% mutate(intsecu=value*gdp) %>% distinct()

pos.exp <- subset(pos.exp, select = c(iso3c,year,intsecu))

pos.exp1 <- gpi.grid %>% left_join(pos.exp)
pos.exp1 <- pos.exp1 %>% subset(is.na(intsecu))

pos.intsec <- unique(pos.exp1$iso3c)

#########################################################   FOR MISSING Data    ################################################
#fill in the police rate
#gpidata <- read.csv("Data/dashboard data.csv") %>%
gpidata <- rio::import("02_data/processed/GPI_2023_FINAL.xlsx") %>%
  select (c(1:8)) %>% 
  mutate (year = year - 1) %>%
  subset(type=="raw") %>%  
  rename(iso3c=geocode, indicator = variablename, govt = government) %>%
  subset(select=-type) %>% 
  select(-region) %>%select(-govt) %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  subset(!year==2022)


# GPI countries 
police <- subset(gpidata, indicator=="police rate" ) 
police$value <- as.numeric(as.character(police$value))
police <- police %>%  select(country, iso3c, year, value)

tmp <- pop %>% select(year,iso3c,population)

police <- police %>% full_join(tmp, by=c("iso3c", "year")) %>%
  rename(pop=population) %>% 
  mutate(pop100k=pop/10^5) %>% mutate(value=value*pop100k) %>% 
  merge(unitcost.scaled[,c("iso3c", "year", "police.direct")], by=c("iso3c", "year"))%>%
  mutate(police.cost=value*police.direct)%>% select(iso3c, country, year, police.cost)


police <- police[police$iso3c %in%pos.intsec, ]

police <- subset(police, select = c(iso3c,year,police.cost))

police <- rename(police,intsecu =police.cost)

pos.exp1 <- pos.exp1  %>%  select(year, iso3c)%>% left_join(police) %>%  select(year, iso3c, intsecu)


pos.exp <- pos.exp %>% rbind(pos.exp1)

pos.exp <- gpi.grid %>% left_join(pos.exp)

pos.exp <- subset(pos.exp,!(iso3c=="PSE" & year<2014))
pos.exp <- subset(pos.exp,!(iso3c=="SSD" & year<2009))


pos.exp3 <- rename(pos.exp,value=intsecu)

pos.exp3 <- distinct(pos.exp3)

count <- pos.exp3 %>% group_by(iso3c) %>% tally()


# imputation

pos.exp <- pos.exp %>% rename (geocode = iso3c, value = intsecu) %>% mutate (variablename = "internal security")

pos.exp <- index_data_pad(pos.exp)


pos.exp <- pos.exp %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, intsecu = imputed)

pos.exp <- gpi.grid %>% left_join(pos.exp)

#============================================================= GTI ================================================

gti.sum <- rio::import ("02_data/processed/terrorism_tracker_data_FINAL.xlsx", sheet = "cleaned-condensed-tt-data")

gti.sum <- gti.sum %>% select (geocode, year, deaths_total, injured_total)

gti.sum <- gti.sum %>% group_by (geocode, year) %>% summarize (killed = sum(deaths_total), wounded = sum (injured_total)) %>%
  rename (iso3c = geocode)

gti.sum <- gpi.grid %>% left_join(gti.sum)

gti.sum <- gti.sum %>% mutate (killed = case_when (is.na(killed) ~ 0,
                                                   TRUE ~ killed)) %>%
  mutate (wounded = case_when (is.na(wounded) ~ 0,
                               TRUE ~ wounded))


# ========================================== MILEX =====================================================================
# Data from 2008 to 2021 from Military Balance

gpidata <- rio::import("02_data/processed/Econ-costing-data-2023.xlsx") %>% 
  rename(iso3c=geocode, indicator = element)

gpidata <- gpidata %>% mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "Homicides per 100,000" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))


milex <- gpidata %>% 
  subset(indicator=="military expenditure (% gdp)") %>% 
  mutate(value = value /100) %>%
  filter (year < 2022)


# Data for 2022 from SIPRI

milex2022 <- read_excel("02_data/processed/SIPRI-Milex-data-1949-2022.xlsx", sheet = "Share of GDP", skip = 5) %>% select(-Notes) %>%
  gather(year, value, -Country) %>% subset(year>2005) %>%
  mutate (year = as.numeric(year))%>%
  rename(country=Country) %>%
  mutate(country=ifelse(country=="eSwatini","Swaziland",country)) %>%
  mutate(country=ifelse(country=="Norh Macedonia","Macedonia",country)) %>%
  mutate( iso3c=  countrycode(country, "country.name","iso3c")) %>%
  mutate(iso3c=ifelse(country=="Kosovo","KSV",iso3c)) %>%
  filter (!country == "USSR")%>%
  select (iso3c, year, value) %>%
  filter (year > 2021) %>%
  right_join(gpi.grid) %>%
  filter(year > 2021) %>%
  mutate (value = as.numeric(value)) %>%
  mutate (indicator = "military expenditure (% gdp)")

# ===================================================================================

milex <- milex %>% rbind (milex2022)

rm(milex2022)


milex <- gpi.grid %>% left_join(milex)

milex <- milex %>% rename (geocode = iso3c, variablename = indicator)

milex <- index_data_pad(milex)

milex <- milex %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, milex = imputed)

milex <- gpi.grid %>% left_join(milex)


milex <- milex %>% left_join(gdp.wdi)

milex <- milex %>% mutate (milex = milex * gdp) %>% select (iso3c, year, milex)





# Homicide ----------------------------------------------------------------

gpidata <- rio::import("02_data/processed/Econ-costing-data-2023.xlsx") %>% 
  rename(iso3c=geocode, indicator = element)

gpidata <- gpidata %>% mutate (indicator = case_when (indicator == "Terrorism deaths" ~ "killed",
                                                      indicator == "Military expenditure % GDP" ~ "military expenditure (% gdp)",
                                                      indicator == "Fear % population" ~ "perceptions of criminality",
                                                      indicator == "Homicides per 100,000" ~ "homicide rate",
                                                      indicator == "Refugees and IDPs" ~ "refugees and idps",
                                                      indicator == "Incarceration rate per 100,000" ~ "incarceration rate",
                                                      indicator == "Peacekeeping" ~ "un peacekeeping funding",
                                                      TRUE ~ indicator))


homicide <- gpidata %>% 
  subset(indicator=="homicide rate") %>% 
  merge(pop[,c("iso3c","year","population")], by=c("iso3c", "year")) %>% 
  rename(pop=population) %>% 
  mutate(poprate=pop/100000) %>% 
  mutate(value=poprate*value) %>% 
  subset(select=c('iso3c','year','indicator','value'))


homicide <- gpi.grid %>% left_join(homicide)

homicide <- homicide %>% rename (geocode = iso3c, variablename = indicator) %>% 
  mutate (variablename = "homicide rate") 

homicide <- index_data_pad(homicide)

homicide <- homicide %>% select (1, 2, 3, 5) %>% rename (iso3c = geocode, value = imputed, indicator = variablename)

homicide <- gpi.grid %>% left_join(homicide)

# incarceration rate ------------------------------------------------------

incar <- gpidata %>% 
  subset(indicator=="incarceration rate") %>%  
  merge(pop[,c("iso3c","year","population")], by=c("iso3c", "year")) %>% 
  rename(pop=population) %>% 
  mutate(poprate=pop/100000) %>% 
  mutate(value=poprate*value) %>% 
  subset(select=c(iso3c,year,indicator,value))


incar <- gpi.grid %>% left_join(incar, by = c("iso3c", "year"))

incar <- incar %>% rename (geocode = iso3c, variablename = indicator) %>% 
  mutate (variablename = "incarceration rate") 


incar <- index_data_pad(incar)

incar <- incar %>% select (1, 2, 3, 5) %>% rename (iso3c = geocode, value = imputed, indicator = variablename)

incar <- gpi.grid %>% left_join(incar, by = c("iso3c", "year"))


# conflcit deaths -------------------------------------------------------------

conflict <- gpidata %>% 
  subset(indicator== "Battle deaths")

conflict <- gpi.grid %>% left_join(conflict)


conflict <- conflict  %>% rename (geocode = iso3c, variablename = indicator) 

conflict <- index_data_pad(conflict)

conflict <- conflict %>% select (c(1,2, 5)) %>% rename (iso3c = geocode, battle_deaths = imputed)

conflict <- gpi.grid %>% left_join(conflict)


# refugees and IDPs -------------------------------------------------------
pop3 <- pop %>% rename(pop=population)


refugidp <- gpidata %>% 
  subset(indicator=="refugees and idps" ) %>% 
  merge(pop3[,c("iso3c","year","pop")], by=c("iso3c", "year")) %>%  
  subset(select=c(iso3c,year,indicator,value)) %>% 
  rename(refug=value)

refugidp <- merge(refugidp, gdp.pc.constant[,c("iso3c","year","gdp.pc.cons")], by=c("iso3c","year"), all=TRUE)

refugidp$refugeidp <- refugidp[, "refug"]*refugidp[,"gdp.pc.cons"]*(1-0.1)

refugidp <- refugidp %>%   subset(select=c(iso3c,year,refugeidp)) 


# Imputation

refugidp <- refugidp %>% rename (geocode = iso3c, value = refugeidp) %>% 
  mutate (variablename = "refugee")  


refugidp <- index_data_pad(refugidp)

refugidp <- refugidp %>% select (1, 2, 5) %>% rename (iso3c = geocode, refugeidp = imputed)

refugidp <- gpi.grid %>% left_join(refugidp)


# GDP losses for countries with >1000 deaths ------------------------------


gdplosses <- conflict %>% 
  mutate(conflict=ifelse(battle_deaths>999,1,0)) %>%
  merge(gdp.wdi[,c("iso3c","year","gdpcons")], by=c("iso3c","year"), all=TRUE) %>%
  mutate(gdplosses=ifelse(conflict==1,gdpcons*0.022,0)) %>% 
  subset(select=c(iso3c,year,gdplosses))


gdplosses <- subset(gdplosses,!(iso3c=="PSE" & year<2015))
gdplosses <- subset(gdplosses,!(iso3c=="SSD" & year<2010))
gdplosses <- subset(gdplosses,!(year<2007))
# gdplosses <- subset(gdplosses,!(year<2008))
gdplosses <- subset(gdplosses,!(year>2022))
# gdplosses <- subset(gdplosses,!(year>2023))


# imputation

gdplosses <- gdplosses %>% rename (geocode = iso3c, value = gdplosses) %>% mutate (variablename = "gdp losses")

gdplosses <- index_data_pad(gdplosses)


gdplosses <- gdplosses %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, gdplosses = imputed)


gdplosses <- gpi.grid %>% left_join(gdplosses)


# ================================ Peacekeeping =============================================

peacekeeping <- gpidata %>% filter (indicator == "un peacekeeping funding") %>%
  rename (peacekeep = value) %>% select (iso3c, year, peacekeep)
peacekeeping <- gpi.grid %>% left_join(peacekeeping)


peacekeeping <- peacekeeping  %>% mutate (variablename = "peacekeeping") %>% rename (geocode = iso3c, value = peacekeep) 

peacekeeping <- index_data_pad(peacekeeping)

peacekeeping <- peacekeeping %>% select (c(1,2, 5)) %>% rename (iso3c = geocode, peacekeep = imputed)

peacekeeping <- gpi.grid %>% left_join(peacekeeping)






