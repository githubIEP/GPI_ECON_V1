missing <- function(df){
  left_join(gpi.grid,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}


####################################################

# prepare exchage from UK to US pesos
exchange <- WDI::WDI(country = "all", indicator ="PA.NUS.FCRF", start = 2000, end = 2019)%>%
  subset(iso2c=="US" | iso2c=="GB") %>%
  select(-iso2c) %>%  spread(country,PA.NUS.FCRF) %>% mutate(year=year+1)


#_________________________________________________________CONSUMER PRICE INDEX____________________________________________________________________________________
# Consumer price index (2010 = 100) -> FP.CPI.TOTL   For unit costs


CPI <- WDI::WDI(country = "all", indicator ="FP.CPI.TOTL", start = 2000, end = 2019)%>%
  subset(iso2c=="US" | iso2c=="GB") %>% 
  select(-iso2c) %>% 
  spread(country,FP.CPI.TOTL)

# write.csv(CPI, "data/CPI.csv")

CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United States`, 1)/`United States`)
CPI <- CPI %>% mutate(CPI_factor_US = tail(CPI$`United Kingdom`, 1)/`United Kingdom`)
CPI <-  CPI %>% mutate(ratio_US = `United Kingdom`/`United States`)


# change year
CPI[,1] <- CPI[,1] + 1


#exchange to real terms
CPI1 <- CPI %>% select(c("year","ratio_US"))

exchange <- left_join(exchange, CPI1, by="year")
exchange <- exchange %>% mutate(real_UK_US = (`United States`/`United Kingdom`)*ratio_US)

rm(CPI1)



#set up
setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')

WEO_all <- read_excel("IMF WEO OCT 2020.xlsx")

setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')


############################################# GDP current USD ############################################# 

gdp__weo <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Gross domestic product, current prices") %>% subset(Units=="U.S. dollars") %>% 
  select("country", "iso3c","2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) 

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


############################################# GDP current PPP ############################################# 


#Note WEO conversion factor is 10^2 larger than the IMF  

PPP_conv_weo <- WEO_all  %>%
  rename(Subject.Descriptor="Subject Descriptor") %>%  rename(iso3c='ISO') %>% rename(country='Country') %>% 
  subset(Subject.Descriptor=="Implied PPP conversion rate") %>%
  select("country", "iso3c","2007", "2008", "2009","2010","2011","2012","2013","2014","2015", "2016", "2017","2018","2019","2020") %>% 
  gather(year, value,-c(iso3c, country)) %>% mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) %>% 
  mutate(value=value/100)


PPP_conv_weo$iso3c[PPP_conv_weo$country=="Kosovo"] <- 'KSV'
PPP_conv_weo <- PPP_conv_weo[PPP_conv_weo$iso3c %in% pos,]
PPP_conv_weo <- subset(PPP_conv_weo,!(iso3c=="PSE" & year<2014))
PPP_conv_weo <- subset(PPP_conv_weo,!(iso3c=="SSD" & year<2009))

PPP_conv_weo <- left_join(gpi.grid, PPP_conv_weo)


tmp <- missing(PPP_conv_weo)


PPP_conv_weo.wdi <- get.wdi("all","PA.NUS.PPPC.RF",2006,2019)%>% 
  rename(value=PA.NUS.PPPC.RF) %>% mutate(year = year + 1) %>% 
  mutate(year=as.numeric(as.character(year))) %>% 
  mutate(value=as.numeric(as.character(value)))

tmp <- tmp %>% select(iso3c,year) %>% left_join(PPP_conv_weo.wdi)

#new
tmp2 <- tmp %>%  na.omit()
PPP_conv_weo <- PPP_conv_weo %>%  na.omit() %>%  rbind(tmp2)
rm(tmp2)
PPP_conv_weo <- left_join(gpi.grid, PPP_conv_weo)
tmp <- missing(PPP_conv_weo)

# We are missing some
tmp$value[tmp$iso3c=="CUB"| tmp$iso3c=="PRK" ] = 1 



tmp <- tmp %>% select(iso3c,year,value)
PPP_conv_weo <- PPP_conv_weo %>%  na.omit() %>% select(iso3c,year,value)


PPP_conv_weo <-  rbind(PPP_conv_weo, tmp)
PPP_conv_weo <- left_join(gpi.grid, PPP_conv_weo)

PPP_conv_weo <- PPP_conv_weo %>%  rename(geocode=iso3c) %>% mutate(variablename="GDP US")
PPP_conv_weo <- PPP_conv_weo %>% distinct()
PPP_conv_weo <- index_data_pad(PPP_conv_weo)


ppp.conv <- PPP_conv_weo %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>% rename(iso3c=geocode, value=imputed)
#If tmp is zero we on money
tmp <- gpi.grid %>% left_join(ppp.conv)
tmp <- missing(tmp)


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



###########################                PPP SCALE                              ##################


#In the 2021 GPI I have changed to using the GDP PPP divided by the population to calculate the GDP PPP per capita
#This solves issues with the estimated data and the consequential indiscrepancies
####issues with the PPP scale

new_calc <- gdp.wdi %>%  left_join(pop, by=c("iso3c","year"))

new_calc <- new_calc %>%  left_join(gdp.pc.ppp, by=c("iso3c","year","country"))

write.csv(new_calc, "results/ppp calc.csv")

ppp <- new_calc %>% select(iso3c, year, gdpconsppp,population) %>% 
  mutate(gdp_pc_ppp=gdpconsppp/population) %>% select(iso3c,year,gdp_pc_ppp)

ppp_us <- ppp[ppp$iso3c=="USA",c("year", "gdp_pc_ppp")]
ppp <- merge(ppp, ppp_us, by="year")
ppp$scale <- ppp$gdp_pc_ppp.x/ppp$gdp_pc_ppp.y

write.csv(ppp, "results/ppp scale calc.csv")

#WDI will update the data at some point, save as a csv to protect from changes to the data
# write.csv(gdp.pc.ppp, "Data/written economics data/gdp.pc.ppp.csv")


###################################







