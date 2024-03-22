

peacekeeping <- gpidata %>% filter (indicator == "un peacekeeping funding") %>%
                            rename (peacekeep = value) %>% select (iso3c, year, peacekeep)
peacekeeping <- gpi.grid %>% left_join(peacekeeping)


peacekeeping <- peacekeeping  %>% mutate (variablename = "peacekeeping") %>% rename (geocode = iso3c, value = peacekeep) 

peacekeeping <- index_data_pad(peacekeeping)

peacekeeping <- peacekeeping %>% select (c(1,2, 5)) %>% rename (iso3c = geocode, peacekeep = imputed)

peacekeeping <- gpi.grid %>% left_join(peacekeeping)
