# this script adds battle death homicide incarceration conflict death! refugees 
# GPI data ----------------------------------------------------------------
# rm(gpidata)
# tmp <- read.xlsx("Data/GPIcostingdata.xlsx")
# # tmp <- read.xlsx("Data/2023_data/GPIcostingdata.xlsx")
# 
# battledeaths.GPI.21 <- read.xlsx("Data/GPIcostingdata.xlsx") %>% 
# #battledeaths.GPI.22 <- read.xlsx("Data/2023_data/GPIcostingdata.xlsx") %>% 
#   select(code,Deaths.from.Internal.Conflict) %>% 
#   rename(`2020`=Deaths.from.Internal.Conflict, Code=code)
# # rename(`2021`=Deaths.from.Internal.Conflict, Code=code)
# 
# #battle deaths are needed to alter the costing 
# battledeaths <- read_excel("Data/battle deaths.xlsx") %>%
# # battledeaths <- read_excel("Data/2023_data/battle deaths.xlsx") %>%  
#   left_join(battledeaths.GPI.21) %>% 
# # left_join(battledeaths.GPI.22) %>%   
#   gather(year, value, -c("Country", "Code")) %>% mutate(indicator="deaths from internal conflict") %>% 
#   rename(country=Country, iso3c=Code) %>% mutate(year=as.numeric(as.character(year))) %>% 
#   mutate(value=as.numeric(as.character(value))) %>% mutate(year=year+1)
# 
# 
# 
# 
# # gpidata.tmp <- read.csv("Data/dashboard data_2021.csv") # another df with same name in use; _2021 is added!
#   gpidata.tmp <- rio::import("Data/2023_data/GPI_2023_FINAL.xlsx") %>%
#   select (c(1:8)) %>% 
#     mutate (year = year - 1) %>%
#     subset(type=="raw") %>%  
#     rename(iso3c=geocode, indicator = variablename, govt = government)
# 
# #remove internal conflict from GI data as each year it for 2 years
# # gpidata <- read.csv("Data/dashboard data_2021.csv") %>% # another df with same name in use; _2021 is added!
#  gpidata <- rio::import("Data/2023_data/GPI_2023_FINAL.xlsx") %>%
#    select (c(1:8)) %>% 
#    mutate (year = year - 1) %>%
#    subset(type=="raw") %>%  
#    rename(iso3c=geocode, indicator = variablename, govt = government) %>%
#   subset(select=-type) %>% select(-region) %>%select(-govt) %>% 
#   mutate(value=as.numeric(as.character(value))) 
#  # subset(!indicator=="deaths from internal conflict")
# 
# 
# gpidata <- rbind(gpidata, battledeaths)



# Homicide ----------------------------------------------------------------

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


# I multiply this my 0.6 as 40% of the refugee population are children and therefore not really contributing to GDP pc
# Note we have opted to a 10% resettlement rate.

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
