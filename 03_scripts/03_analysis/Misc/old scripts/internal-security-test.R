
# Using Police Rate to estimate internal security costs for all countries

gpidata <- rio::import("Data/2023_data/GPI_2023_FINAL.xlsx") %>%
  select (c(1:8)) %>% 
  mutate (year = year - 1) %>%
  subset(type=="raw") %>%  
  rename(iso3c=geocode, indicator = variablename, govt = government) %>%
  subset(select=-type) %>% 
  select(-region) %>%select(-govt) %>% 
  mutate(value=as.numeric(as.character(value)))



# GPI countries 
police <- subset(gpidata, indicator=="police rate" ) 
police$value <- as.numeric(as.character(police$value))
police <- police %>%  select(iso3c, year, value)

police <- gpi.grid %>% left_join(police)


police <- police %>% rename (geocode =iso3c) %>% mutate (variablename = "police rate")

police <- index_data_pad(police)

police <- police %>% select (geocode, year, imputed) %>% rename (iso3c = geocode, value = imputed) 

pos.exp <- police %>% left_join(pop) %>% right_join(gpi.grid) %>% 
                     left_join(unitcost.scaled, ) %>%
                     mutate (police.cost = (value * population / 10^5) * police.direct) %>%
                     rename (intsecu = police.cost) %>%
                     select (iso3c, year, intsecu)

