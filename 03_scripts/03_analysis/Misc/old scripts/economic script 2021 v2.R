####################################  FORUMULA MISSING   ###################################

#This will provide a df of the countries missing or missing years for a country 
missing <- function(df){
  left_join(gpi.grid,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}


####################################  EXCHANGE RATE   ###################################
## UPDATE LATEST YEAR (NOTE 2021 DATA MAY NOT BE AVAILABLE SO PLUS ONE TO EACH YEAR)

#SHOW WHERE THE CODE COMES FROM PA.NUS.FCRF

# prepare exchage from UK to US pesos
exchange <- WDI::WDI(country = "all", indicator ="PA.NUS.FCRF", start = 2000, end = 2020)%>%
  subset(iso2c=="US" | iso2c=="GB") %>%
  select(-iso2c) %>%  spread(country,PA.NUS.FCRF) %>% mutate(year=year+1)


####################################  CONSUMER PRICE INDEX FOR UNIT COSTS   ###################################
# Consumer price index (2010 = 100) -> FP.CPI.TOTL   For unit costs

## UPDATE LATEST YEAR
CPI <- WDI::WDI(country = "all", indicator ="FP.CPI.TOTL", start = 2000, end = 2020)%>%
  subset(iso2c=="US" | iso2c=="GB") %>% 
  select(-iso2c) %>% 
  spread(country,FP.CPI.TOTL)

# write.csv(CPI, "data/CPI.csv")

## GET FACTOR
CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United States`, 1)/`United States`)
CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United Kingdom`, 1)/`United Kingdom`)
CPI <-  CPI %>% mutate(ratio_US = `United Kingdom`/`United States`)


# change year (ADDS A YEAR SO THAT WE HAVE 2021)
CPI[,1] <- CPI[,1] + 1


#exchange rate from current to constant (real terms)
# Must apply inflation

CPI1 <- CPI %>% select(c("year","ratio_US"))

exchange <- left_join(exchange, CPI1, by="year")
exchange <- exchange %>% mutate(real_UK_US = (`United States`/`United Kingdom`)*ratio_US)

rm(CPI1)

####################################  SET WORKING DIRECTORY   ###################################

#set up
setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')

## MANUALLY DOANLOAD THIS FROM THE IMF. GET LATEST DATA
WEO_all <- read_excel("IMF WEO OCT 2020.xlsx")

setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')


############################################# GDP current USD ############################################# 

gdp__weo <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Gross domestic product, current prices") %>% subset(Units=="U.S. dollars") %>% 
  select("country", "iso3c","2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) %>% 
  mutate(value=value*10^9)

gdp__weo$iso3c[gdp__weo$country=="Kosovo"] <- 'KSV'
gdp__weo <- gdp__weo[gdp__weo$iso3c %in% pos,]
gdp__weo <- subset(gdp__weo,!(iso3c=="PSE" & year<2014))
gdp__weo <- subset(gdp__weo,!(iso3c=="SSD" & year<2009))

gdp__weo <- left_join(gpi.grid, gdp__weo)
tmp <- missing(gdp__weo)


gdp.wdi <- get.wdi("all","NY.GDP.MKTP.CD",2006,2019)%>% 
  rename(value=NY.GDP.MKTP.CD) %>% mutate(year = year + 1) %>% 
  mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(value)))

tmp <- tmp %>% select(iso3c,year) %>% left_join(gdp.wdi)
tmp2 <- tmp %>%  na.omit()
gdp__weo <- gdp__weo %>%  na.omit() %>%  rbind(tmp2)
rm(tmp2)
gdp__weo <- left_join(gpi.grid, gdp__weo)
tmp <- missing(gdp__weo)
# USE this to write the missing data
# write.csv(tmp, "missing gdp current US v2.csv")
gdp.wdi.missing <- read_csv("Data/Missing data/missing GDP (current US$) 2021.csv") %>% select(iso3c, year, value)
gdp__weo <- gdp__weo %>%  na.omit() %>% select(iso3c,year,value)
gdp.wdi <-  rbind(gdp__weo, gdp.wdi.missing)
gdp.wdi <- left_join(gpi.grid, gdp.wdi)
gdp.wdi <- gdp.wdi %>%  rename(geocode=iso3c) %>% mutate(variablename="GDP US")
gdp.wdi <- gdp.wdi %>% distinct()
gdp.wdi <- index_data_pad(gdp.wdi)
gdp.wdi <- gdp.wdi %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>% rename(iso3c=geocode, value=imputed)
#If tmp is zero we on money
tmp <- gpi.grid %>% left_join(gdp.wdi)
tmp <- missing(tmp)
gdp.wdi[,"variablename"] <- "gdp current US"

rm(tmp)


############################################# PPP Conversion Scale  ############################################# 


ppp.conv <- get.wdi("all","PA.NUS.PPPC.RF",2006,2020) %>% 
  rename(value=PA.NUS.PPPC.RF) %>% mutate(year=year+1)

ppp.conv <- left_join(gpi.grid, ppp.conv, by=c("year","iso3c"))
ppp.conv <- ppp.conv[ppp.conv$iso3c %in% pos,]


tmp <-missing(ppp.conv)



ppp.conv$value[ppp.conv$iso3c=="CUB"| ppp.conv$iso3c=="PRK" |  ppp.conv$iso3c=="KSV" |   ppp.conv$iso3c=="PSE"]=1 
ppp.conv$value[ppp.conv$iso3c=="SOM"| ppp.conv$iso3c=="SYR" |  ppp.conv$iso3c=="TWN"]=1 


ppp.conv$variablename <- "ppp convt"

ppp.conv <- ppp.conv %>%  rename(geocode=iso3c) %>% mutate(variablename="GDP US")
ppp.conv <- ppp.conv %>% distinct()
ppp.conv <- index_data_pad(ppp.conv)

ppp.conv <- ppp.conv %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>% rename(iso3c=geocode, value=imputed)

ppp.conv <- left_join(gpi.grid, ppp.conv, by=c("year","iso3c"))
ppp.conv <- ppp.conv[ppp.conv$iso3c %in% pos,]


write.csv(ppp.conv, "Results/Economic output/PPP convert.csv")



############################################# Inflation and deflator ############################################# 


# deflator

gdpdefl_weo <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Gross domestic product, deflator") %>% 
  select("country", "iso3c","2003","2004","2005", "2006", "2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) 

gdpdefl_weo$iso3c[gdpdefl_weo$country=="Kosovo"] <- 'KSV'
gdpdefl_weo <- gdpdefl_weo[gdpdefl_weo$iso3c %in% pos,]


def.wdi2 <- gdpdefl_weo %>% 
  subset(year==2020) %>% rename(value.2020=value) %>% 
  subset(select=-year) %>% na.omit(value.2020) %>% 
  mutate(value.2020=as.numeric(as.character(value.2020)))%>% 
  full_join(gdpdefl_weo, by="iso3c") %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  mutate(deflator=value/value.2020) %>% na.omit(value.2020) %>% 
  group_by(year) %>%  summarise(def.median=median(deflator))







# Inflation
gdp_cpi_weo <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Inflation, average consumer prices") %>% subset(Units=="Index") %>% 
  select("country", "iso3c", "2003","2004","2005", "2006", "2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) 

gdp_cpi_weo$iso3c[gdp_cpi_weo$country=="Kosovo"] <- 'KSV'
gdp_cpi_weo <- gdp_cpi_weo[gdp_cpi_weo$iso3c %in% pos,]


CPI.wdi <- gdp_cpi_weo %>% 
  subset(year==2020) %>% rename(value.2020=value) %>% 
  subset(select=-year) %>% na.omit(value.2020) %>% 
  mutate(value.2020=as.numeric(as.character(value.2020)))%>% 
  full_join(gdp_cpi_weo, by="iso3c") %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  mutate(inflation=value/value.2020) %>% na.omit(value.2020) %>% 
  group_by(year) %>%  summarise(def.median=median(inflation))


############################################# Convert GDP USD current to constant! ############################################# 

### CONVERT TO CONSTANT
gdp.wdi <- gdp.wdi %>% mutate(value=as.numeric(as.character(value)))
gdp.wdi <- gdp.wdi %>% mutate(year=as.numeric(as.character(year)))
def.wdi2 <- def.wdi2 %>% mutate(year=as.numeric(as.character(year)))


gdp.wdi <- gdp.wdi  %>% full_join(def.wdi2, by="year") %>% 
  mutate(gdpcons=value/def.median) %>% 
  full_join(ppp.conv, by=c("iso3c","year")) 



gdp.wdi <- gdp.wdi %>% 
  mutate(gdpconsppp=gdpcons/value.y) %>% 
  subset(select=c(iso3c,year,value.x,gdpcons,gdpconsppp))
gdp.wdi <- subset(gdp.wdi,!(iso3c=="PSE" & year<2014))
gdp.wdi <- subset(gdp.wdi,!(iso3c=="SSD" & year<2009))

gdp.wdi <- rename(gdp.wdi, gdp='value.x')


write.csv(gdp.wdi, "Results/Economic output/GDP completed PPP USD and Constant.csv")



######################################################## POPULATION  ############################################################



pop <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Population") %>%
  select("country", "iso3c", "2003","2004","2005", "2006", "2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) %>% 
  mutate(value=value*10^6)

pop$iso3c[pop$country=="Kosovo"] <- 'KSV'
pop <- pop[pop$iso3c %in% pos,]
pop <- subset(pop,!(iso3c=="PSE" & year<2014))
pop <- subset(pop,!(iso3c=="SSD" & year<2009))

pop <- left_join(gpi.grid, pop)

tmp <- missing(pop)


pop_wb <- get.wdi("all","SP.POP.TOTL",2006,2019)%>% 
  rename(value=SP.POP.TOTL) %>% mutate(year = year + 1) %>% 
  mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(value)))

tmp <- tmp %>% select(iso3c,year) %>% left_join(pop_wb)


pop <- pop %>%  na.omit()
pop_wb <- tmp %>%  na.omit()

pop <-  rbind(pop, pop_wb)


# This will show who is missing!!!!
pop <- left_join(gpi.grid, pop)
pop <- pop %>%  rename(population=value)

# write.csv(pop, "Results/Economic output/population.csv")

tmp <- missing(pop)

###########################                PPP SCALE                              ##################


#In the 2021 GPI I have changed to using the GDP PPP divided by the population to calculate the GDP PPP per capita
#This solves issues with the estimated data and the consequential discrepancies
####issues with the PPP scale

new_calc <- gdp.wdi %>%  left_join(pop, by=c("iso3c","year"))

# new_calc <- new_calc %>%  left_join(gdp.pc.ppp, by=c("iso3c","year","country"))

# write.csv(new_calc, "results/ppp calc.csv")

ppp <- new_calc %>% select(iso3c, year, gdpconsppp,population) %>% 
  mutate(gdp_pc_ppp=gdpconsppp/population) %>% select(iso3c,year,gdp_pc_ppp)

ppp_us <- ppp[ppp$iso3c=="USA",c("year", "gdp_pc_ppp")]
ppp <- merge(ppp, ppp_us, by="year")
ppp$scale <- ppp$gdp_pc_ppp.x/ppp$gdp_pc_ppp.y

# write.csv(ppp, "Results/Economic output/ppp scale calc.csv")

#WDI will update the data at some point, save as a csv to protect from changes to the data
# write.csv(gdp.pc.ppp, "Data/written economics data/gdp.pc.ppp.csv")







######################### GDP PER CAPITA USD CURRENT #########################
new_calc <- gdp.wdi %>%  left_join(pop, by=c("iso3c","year"))
names(new_calc)

gdp.pc.wb <- new_calc %>% mutate(gdp_pc_ppp=gdpconsppp/population, gdp_pc_cons=gdpcons/population, gdp_pc_current=gdp/population) %>%
  select(iso3c,year,gdp_pc_ppp,gdp_pc_cons,gdp_pc_current)

# write.csv(ppp, "Results/Economic output/ppp scale calc.csv")

#WDI will update the data at some point, save as a csv to protect from changes to the data
# write.csv(gdp.pc.ppp, "Data/written economics data/gdp.pc.ppp.csv")






###################################  END OF SCRIPT   ################################### 







