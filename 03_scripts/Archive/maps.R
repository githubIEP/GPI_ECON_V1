##### ----- GTI MAPS
#' This script is for producing all the standard GTI Maps
#' (Overall Results, Top ten countries, 20 deadliest attacks etc)
##### -----

### --- Libraries

f_LibraryLoader(tidyverse,
                iepg,
                lubridate,
                rio,
                sf,
                zoo,
                extrafont)

## -- Index Results Map --------------------------------------------------------

MAP_IndexMap = c(title = paste0(GTI_YEAR," GLOBAL TERRORISM INDEX"), 
                   sheet = "IndexMap", source = "", xtext = "", ytext = "",
                   counter = "", type = "Map")

world.map <- iepg_get_gadm("level0") %>%
  filter(!is.na(income)) # Filter out small countries

GTI_map.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  select(geocode,incidents, deaths, injured, hostages, banded_score, year, rank) %>%
  mutate(map_bands = f_WebBands(banded_score, SCORING_BANDS))


GTI_COLOURS_SCALE <- c("#D1D3D4","#B2DED1", "#f6e38d", "#FEC779", "#F37053", "#ed1c24", "#770A1E")

world.map <- world.map %>%
  left_join(GTI_map.df %>% filter(year == GTI_YEAR)) %>%
  mutate(map_bands = ifelse(is.na(banded_score), 0, map_bands),
         map_bands = factor(map_bands, levels = c(0, 1, 2, 3, 4, 5, 6)))

pMAP_IndexMap <- world.map %>% 
  ggplot() + 
  geom_sf(aes(fill = map_bands), color="white") +
  theme_void() +
  theme(legend.position = "bottom") + 
  theme(panel.background = element_rect(fill='black', colour='black')) +
  theme(legend.position = c(0.102,0.1),
        legend.direction="horizontal",
        legend.key.height= unit(0.3, 'cm'),
        legend.key.width= unit(1, 'cm'),
        legend.text = element_text(color = c("white"))
  ) + 
  scale_fill_manual(values = GTI_COLOURS_SCALE,
                    breaks = c(0, 1, 2, 3, 4, 5, 6),
                    labels = c("", "", "", "", "", "", ""),
                    guide = guide_legend(
                      override.aes = list(fill = GTI_COLOURS_SCALE, color = GTI_COLOURS_SCALE),
                      keywidth = unit(0.04, "npc"),
                      keyheight = unit(0.5, "lines"),
                      label.position = "bottom",
                      title = "",
                      direction = "horizontal",
                      nrow = 1,
                      byrow = TRUE
                    )) +
  annotate("text", x = -174, y = 12, label = REPORT_YEAR, colour = "white", size=10, family="Arial", fontface = 2) +
  annotate("text", x = -165, y = 0, label = " GLOBAL", colour = "white", size=10,family="Arial", fontface = 2) + 
  annotate("text", x = -153, y = -12, label = "TERRORISM", colour = "white",  size=10,family="Arial", fontface = 2) +
  annotate("text", x = -169, y = -24, label = "INDEX", colour = "white",  size=10,family="Arial", fontface = 2) + 
  annotate("text", x = -160, y = -38, label = "THE IMPACT OF TERRORISM",colour = "white",  size=4,family="Arial", fontface = 2) 

## -- Index Results Table ------------------------------------------------------

TABLE_IndexTable = c(title = paste0(GTI_YEAR," GLOBAL TERRORISM INDEX"), 
                 sheet = "IndexTable", source = "", xtext = "", ytext = "",
                 counter = "", type = "Table")

TABLE_IndexTable.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  select(rank, geocode, country, year, banded_score) %>%
  filter(year %in% c(GTI_YEAR, GTI_YEAR - 1)) %>%
  pivot_wider(names_from = year, values_from = c(rank, banded_score)) %>%
  mutate(`rank change` = !!sym(paste0("rank_",GTI_YEAR - 1)) - !!sym(paste0("rank_",GTI_YEAR))) %>%
  select(country, rank = !!sym(paste0("rank_",GTI_YEAR)), score = !!sym(paste0("banded_score_",GTI_YEAR)),`rank change`) %>%
  mutate(scoring_bands = f_ScoringBands(score,SCORING_BANDS)) %>%
  select(rank,scoring_bands,country,score,`rank change`) %>%
  arrange(rank,country)

## --- Terrorist Incidents: Table ----------------------------------------------

TABLE_AttacksTable = c(title = paste0(GTI_YEAR," Terrorist Incidents"), 
                     sheet = "AttacksTable", source = SOURCE_TT, xtext = "", ytext = "",
                     counter = "", type = "Table")

TABLE_AttacksTable.df = rio::import("02_data/processed/clean_TT.rds")  %>% 
  filter(year == GTI_YEAR, deaths_total >= 0) %>% 
  select(event_id, country, date, admin_Name, perpetrator_name, deaths_total, summary) %>%
  distinct(event_id, country, date, admin_Name, perpetrator_name, deaths_total, summary) %>%
  arrange(desc(deaths_total)) %>% 
  slice(1:20)

## -- Terrorist Incidents: Map -------------------------------------------------

# Map Information
MAP_AttacksMap = c(title = paste0(GTI_YEAR," GLOBAL TERRORISM INDEX"), 
                 sheet = "AttacksMap", source = "", xtext = "", ytext = "",
                 counter = "", type = "Map")

# All Attacks
attacks.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  filter(year == GTI_YEAR,
                deaths_total >= 0) %>% 
  select(event_id, date, geocode, latitude, longitude, deaths_total) %>%
  distinct(event_id, date, geocode, latitude, longitude, deaths_total)

# Top 20 Attacks
top20attacks.df <- attacks.df %>%
  slice_max(deaths_total, n = 20, with_ties = FALSE) %>%
  select(event_id, date, geocode, latitude, longitude, deaths_total) %>%
  distinct(event_id, date, geocode, latitude, longitude, deaths_total) %>%
  rename(ID_0=geocode)

# Base Map
incidents.map <- iepg_get_gadm("level0") %>%
  filter(!is.na(gpi_region))

# Attacks Map
pMAP_AttacksMap <- ggplot(incidents.map) +
  geom_sf() +
  geom_point(data = attacks.df, 
             aes(x = longitude, y = latitude, size = deaths_total),
             alpha = 0.2,
             colour = "red") +
  geom_point(data = top20attacks.df,
             aes(x = longitude, y = latitude, size = deaths_total),
             shape = 1) + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_size_continuous(range = c(min(attacks.df$deaths_total+1), max(attacks.df$deaths_total) * 0.02))



