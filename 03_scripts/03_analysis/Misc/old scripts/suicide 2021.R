#_________________________________________________________SUICIDE RATE_________________________________________________________________________________________________
#Suicide mortality rate (per 100,000 population)

########### WORLD BANK SUICIDE RATE ################


suicide <- get.wdi("all","SH.STA.SUIC.P5",2007,2019) %>% 
  rename(rate=SH.STA.SUIC.P5) %>% rename(value=rate) %>% mutate(variablename="suicide_rate") %>%
  subset(!iso3c=="PSE") %>% subset(!iso3c=="KSV") %>% subset(!iso3c=="TWN")

tmp <- left_join(gpi.grid, suicide)
tmp <- missing(tmp)

##############################


suicide_GBD <- read_excel("Data/suicide GBD.xlsx", 
                          col_types = c("text", "text", "text", 
                                        "text", "text", "text", "numeric", 
                                        "numeric", "numeric", "numeric")) %>% 
  select(location,year,val) %>%
  rename(country=location) %>%
  rename(value=val) %>% 
  mutate(iso3c=countrycode(country,"country.name","iso3c"))


suicide_GBD$iso3c[suicide_GBD$country=="Kosovo"] <- 'KSV'


suicide_GBD <- suicide_GBD %>%  select(iso3c, year, value) 

suicide_GBD <- left_join(gpi.grid, suicide_GBD)



suicide_GBD <- suicide_GBD %>% rename(geocode=iso3c) %>% mutate(variablename = "suicide rate") %>% 
  subset(!geocode=="KSV")

suicide_GBD <- index_data_pad(suicide_GBD)

suicide <- suicide_GBD %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)


suicide <- subset(suicide,!(iso3c=="PSE" & year<2014))
suicide <- subset(suicide,!(iso3c=="SSD" & year<2009))

suicide <- suicide %>% left_join(pop) %>%  distinct() %>% mutate(value=(population/100000*value)) %>% select(-population) 
suicide <- suicide %>% mutate(variablename="suicide_count") %>% select(iso3c,value, year) %>%  rename(suicidevalue=value)


# finished
