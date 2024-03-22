#_________________________________________________________ # private security _________________________________________________________________________________________________
# private security fix data clean     NOTE THIS IS A ONE OFF AND NOT NEEDED FOR THE FOLLOWING YEAR  -------------------------------------------------------
#This is for reference of how the data was cleaned

setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')



Small_Arms_Survey_2011_converted <- read_excel("Private security/Small-Arms-Survey-2011-converted.xlsx", 
                                               sheet = "2011 and 2013", col_types = c("text", 
                                                                                      "numeric", "numeric")) %>%
  gather(year,rate, -Country) %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) %>%
  mutate(year=as.numeric(year)) %>% 
  mutate(rate=as.numeric(rate)) %>% 
  select(iso3c, year, rate) %>% 
  left_join(pop) %>% mutate(value=rate*population) %>%
  mutate(value=value/(population/100000)) %>% 
  select(iso3c, year, value) %>% na.omit() %>% spread(year,value) %>% 
  mutate(`2012`=`2011`,`2014`=`2013`,`2015`=`2013`,`2016`=`2013`,`2017`=`2013`,`2018`=`2013`,`2019`=`2013`,`2020`=`2013`) %>% 
  gather(year,value, -iso3c) %>% mutate(year=as.numeric(year))%>% 
  mutate(value=as.numeric(value))


tmp <- Small_Arms_Survey_2011_converted %>% select(iso3c,year) %>% mutate(keep=1) %>% mutate(year=as.numeric(year))
tmp <- gpi.grid %>% mutate(year=as.numeric(year)) %>% left_join(tmp) %>% left_join(Small_Arms_Survey_2011_converted) %>% na.omit() 


private.security <- gpi.grid %>% left_join(tmp)%>% subset(is.na(keep))



Private_security_estimates <- read_excel("Private security/Private security estimates 2.0.xlsx", 
                                         sheet = "Where do private security outnu", 
                                         col_types = c("text", "numeric", "text", 
                                                       "numeric", "numeric")) %>% 
  select("Country","Private security workers per 100000") %>%
  rename(value="Private security workers per 100000") %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) 

Private_security_estimates$iso3c[Private_security_estimates$Country=="Kosovo"] <- "KSV"
Private_security_estimates <- Private_security_estimates %>% select(iso3c,value)


trial <- private.security %>% select(iso3c,year) %>% left_join(Private_security_estimates)
tmp <- tmp %>%  select(iso3c,year,value) %>% rbind(trial)


private.security <- tmp %>% na.omit()


tmp <- private.security %>% mutate(country=countrycode(iso3c,"iso3c", "country.name")) %>% spread(year, value)
tmp$iso3c[tmp$iso3c=="KSV"] <- "Kosovo"


write.csv(tmp, "Private security/private security numbers updated 2020.csv")


rm(tmp, tmp2, private.security,trial, Private_security_estimates,Small_Arms_Survey_2011_converted)

# private security  -------------------------------------------------------
setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')

priv.secu <- read_csv("Private security/private security numbers updated 2020.csv") %>% select(-X1) %>% 
  gather(year,value, -c("country","iso3c")) %>% select(-country)


setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')


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

