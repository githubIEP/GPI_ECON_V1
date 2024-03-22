f_LibraryLoader(tidyverse,
                countrycode,
                scales)

CHART_CompDeaths = c(title = paste0("Deaths from terrorism, ",COMPVIOLENCE_START,"-", COMPVIOLENCE_END),
                     sheet = "Trend_Summary", source = "UCDP, UNODC, GBD, WPP, Terrorism Tracker, IEP Calculations", 
                     xtext = "", ytext = "DEATHS")

# Start and End Date of Comparisons
COMPVIOLENCE_START = 2008
COMPVIOLENCE_END = 2021

### --- Comparative levels of violence
#' Comparing total deaths from different forms of violence

# Set Active Chart
CHART_ACTIVE = CHART_CompDeaths

## -- Tidy Data

# Homicide
homicide.df <- iepg_get(100) %>% # GPI homicide data (raw rate)
  ungroup() %>%
  filter(year >= COMPVIOLENCE_START & year <= COMPVIOLENCE_END) %>%
  select(geocode,value,year) %>%
  mutate(indicator = "Homicide") 

# Suicide
suicide.df <- iepg_get(19717) %>%
  ungroup() %>%
  filter(year >= COMPVIOLENCE_START & year <= COMPVIOLENCE_END) %>%
  select(geocode,value,year) %>%
  mutate(indicator = "Suicide")

# Population data to convert rates into totals
pop.df <- iepg_get(14947) %>% 
  ungroup() %>%
  filter(year >= COMPVIOLENCE_START & year <= COMPVIOLENCE_END) %>%
  select(geocode, population = value, year)

# Join and convert rates to totals
homicide.df <- homicide.df %>%
  left_join(pop.df, by = c("geocode","year"))

suicide.df <- suicide.df %>%
  left_join(pop.df, by = c("geocode","year"))

cide.df <- homicide.df %>%
  rbind(suicide.df)

cide.df$total <- ceiling(cide.df$value * (cide.df$population/100000))

cide.df <- cide.df %>% 
  select(-c(value, population)) %>%
  rename(value = total)

# Conflict Data
conflict.df <- f_DownloadGED() %>%
  mutate(year = year(date_start)) %>%
  mutate(geocode = countrycode::countrycode(country, "country.name", "iso3c")) %>%
  select(geocode,value = best,year) %>%
  filter(year >= COMPVIOLENCE_START & year <= COMPVIOLENCE_END) %>%
  mutate(indicator = "Conflict")

# Terrorism Data
terrorism.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  select(geocode,year,value = deaths) %>%
  filter(year >= COMPVIOLENCE_START & year <= COMPVIOLENCE_END) %>%
  mutate(indicator = "Terrorism")

# Combined
deaths.df <- cide.df %>%
  rbind(conflict.df) %>%
  rbind(terrorism.df) %>%
  drop_na() %>%
  group_by(indicator) %>%
  summarise(total_deaths = sum(value)) %>%
  arrange(total_deaths)

# Base Plot
p <- ggplot(deaths.df, aes(x = reorder(indicator, total_deaths), y = total_deaths, fill = "darkred")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkred" = "darkred"))

# GTI Theme
pCHART_ACTIVE <- f_ThemeGTI(p,
                            chart_info = CHART_ACTIVE,
                            plottitle = "",
                            xaxis = "Include",
                            yaxis = "",
                            xgridline = "",
                            ygridline = "Include") +
  theme(legend.position = "none") +
  scale_y_continuous(expand = c(0,0), labels = comma)