#######################      fear    ####################### 


# Per capita data 

fear <- gpidata %>%
  subset(indicator=="perceptions of criminality") %>%   rename_all(tolower) %>%
  select("iso3c", "year", "value")  %>%
#  mutate(year=year-1) %>%
 # rename(iso3c=code) %>% 
 # subset(!year==2022) %>%
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

