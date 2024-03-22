#_________________________________________________________SUICIDE RATE_________________________________________________________________________________________________
#Suicide mortality rate (per 100,000 population)

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




# finished
