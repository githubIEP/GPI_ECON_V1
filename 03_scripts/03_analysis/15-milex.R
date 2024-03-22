# Data from 2008 to 2021 from Military Balance

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


milex <- gpidata %>% 
  subset(indicator=="military expenditure (% gdp)") %>% 
  mutate(value = value /100) %>%
  filter (year < 2022)


# Data for 2022 from SIPRI

milex2022 <- read_excel("Data/2023_data/SIPRI-Milex-data-1949-2022.xlsx", sheet = "Share of GDP", skip = 5) %>% select(-Notes) %>%
  gather(year, value, -Country) %>% subset(year>2005) %>%
  mutate (year = as.numeric(year))%>%
  rename(country=Country) %>%
  mutate(country=ifelse(country=="eSwatini","Swaziland",country)) %>%
  mutate(country=ifelse(country=="Norh Macedonia","Macedonia",country)) %>%
  mutate( iso3c=  countrycode(country, "country.name","iso3c")) %>%
  mutate(iso3c=ifelse(country=="Kosovo","KSV",iso3c)) %>%
  filter (!country == "USSR")%>%
  select (iso3c, year, value) %>%
  filter (year > 2021) %>%
  right_join(gpi.grid) %>%
  filter(year > 2021) %>%
  mutate (value = as.numeric(value)) %>%
  mutate (indicator = "military expenditure (% gdp)")

# ===================================================================================

milex <- milex %>% rbind (milex2022)

rm(milex2022)


milex <- gpi.grid %>% left_join(milex)

milex <- milex %>% rename (geocode = iso3c, variablename = indicator)

milex <- index_data_pad(milex)

milex <- milex %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, milex = imputed)

milex <- gpi.grid %>% left_join(milex)


milex <- milex %>% left_join(gdp.wdi)

milex <- milex %>% mutate (milex = milex * gdp) %>% select (iso3c, year, milex)
