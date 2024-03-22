
small.arms <- read_excel("Data/2023_data/Small arms survey data 2020.xlsx", sheet = "clean") %>%  
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

