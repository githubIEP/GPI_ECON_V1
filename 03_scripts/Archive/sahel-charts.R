
f_LibraryLoader(tidyverse,
                iepg,
                patchwork,
                countrycode,
                tidygeocoder)

## -- CHART_TerrorismSahel -----------------------------------------------------


CHART_TerrorismSahel = c(title = paste0("Terrorist Incidents in the Sahel, ",TT_FIRST_YEAR," - ",GTI_YEAR),
                           sheet = "?", source = SOURCE_TT,
                           xtext = "", ytext = "TERRORIST ATTACKS")

CHART_ACTIVE = CHART_TerrorismSahel

SahelAttacks.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  filter(geocode %in% THE_SAHEL) %>%
  select(country,year,incidents) %>%
  mutate(country = if_else(!country %in% c("Burkina Faso","Mali","Niger"),"Other",country)) %>%
  group_by(country,year) %>%
  summarise(incidents = sum(incidents)) %>%
  ungroup()

# Make the chart
p <- ggplot(SahelAttacks.df, aes(x=year, y=incidents, fill=country)) + 
  geom_area() +
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2))) +
  scale_fill_manual(values = GTI_COLOURS) +
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0))

pCHART_ACTIVE <- f_ThemeGTI(
  p, 
  chart_info = CHART_ACTIVE,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") 






####--Kidnapping Changes - Map--####

sahel = c("NER", "BFA", "NGA", "MLI", "TCD", "MRT",
          "GMB", "SEN", "GIN", "CMR")


tmp = acled_df %>% mutate(geocode = countrycode(country, "country.name", "iso3c"),
                          event_date = as.Date(event_date, format = "%d/%m/%Y")) %>%
  dplyr::filter(sub_event_type == "Abduction/forced disappearance",
    year >= CURRENT_YEAR - 5,
    geocode %in% sahel,
    !is.na(longitude),
    !is.na(latitude)) %>%
  iepsqlite::latlong2shp("level1") %>%
  group_by(ID, year) %>%
  mutate(kidnap = n()) %>%
  dplyr::select(ID, year, kidnap) %>% ungroup() %>% unique()

all_years <- expand.grid(ID = unique(tmp$ID), year = seq(min(tmp$year), max(tmp$year)))

tmp <- all_years %>%
  left_join(tmp, by = c("ID", "year")) %>%
  mutate(kidnap = replace_na(kidnap, 0)) %>%
  group_by(ID) %>% 
  summarise(value = kidnap[year == max(year)] - kidnap[year == min(year)]) %>% rename(ID_1 = ID)

shp = iep_get_shapefile("level1") %>% 
  dplyr::filter(ID_0 %in% sahel) %>% left_join(tmp)

is.na(shp$value) <- 0


map = iep_get_shapefile("level1") %>%
  dplyr::filter(ID_0 %in% sahel) 

p <- ggplot() +
  geom_sf(data = shp,  fill="white", colour=NA, linewidth=0.9) +
  geom_sf(data = shp, aes(fill = value)) +
  theme(legend.position = "right") +
  geom_sf(data = shp, color="#232323", fill=NA, linewidth=0.5)+
  gpi_map_theme() +
  scale_fill_gradient(low = "#eddddc", high = "darkred", na.value = "white")


## -- RogersMatrix_OCGTI -------------------------------------------------------


# ggsave("./04_outputs/maps/Organised Crime-v-GTI.jpg", x, height = HEIGHT, width = WIDTH)

print(p)
addWorksheet(wb, "Organised Crime")
insertPlot(wb, "Organised Crime", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)
print(legend2)
insertPlot(wb, "Organised Crime", height = 4, width = 6, fileType = "jpg", startRow = 1,
           startCol = 1)



####--Map of subnational areas in Mali, Burkina Faso and Niger overlaying gold mines and terror attacks, 
#conflict activity using terrorism tracker, ACLED.--####

sahel = c("NER", "BFA", "NGA", "MLI", "TCD", "MRT",
          "GMB", "SEN", "GIN", "CMR")

focus_countries = c("MLI", "BFA", "NER")

full_path <- file.path(IEP_USERPATH, "Research/Data/Africa_GIS.gdb/a0000000b.gdbtable")

gold = sf::st_read(full_path) %>%  dplyr::filter(grepl("gold", tolower(DsgAttr01))) %>%
  mutate(incident = 1) %>% sf::st_zm() %>% fortify() %>% rename(longitude = Longitude, latitude = Latitude) %>%
  sf::st_drop_geometry() %>% iepsqlite::latlong2shp("level1") %>% mutate(geocode = substr(ID, 1, 3)) %>% 
  relocate(geocode) %>% dplyr::filter(geocode %in% focus_countries) %>% dplyr::select(geocode, ID, longitude, latitude) %>%
  mutate(type = "Gold")

tmp = terrorism_df %>%
  dplyr::filter(complete.cases(longitude)) %>% dplyr::filter(complete.cases(latitude)) %>%
  dplyr::filter(year >= CURRENT_YEAR, deaths_total > 0) %>%
  dplyr::filter(geocode %in% focus_countries) %>%
  iepsqlite::latlong2shp("level1") %>% mutate(geocode = substr(ID, 1, 3)) %>% 
  relocate(geocode) %>% dplyr::filter(geocode %in% focus_countries) %>% dplyr::select(geocode, year, ID, admin_level, longitude, latitude, deaths_total) %>%
  group_by(ID,year, admin_level) %>% summarise(deaths = sum(deaths_total), variablename = "Terrorist Deaths") %>%
  ungroup()
tmp$value = findInterval(tmp$deaths, c(0, 50, 100, 200, 300, 400))

acled = acled_df %>% 
  dplyr::filter(complete.cases(longitude)) %>% dplyr::filter(complete.cases(latitude)) %>%
  mutate(geocode = countrycode(country, "country.name", "iso3c")) %>%
  dplyr::filter(year >= CURRENT_YEAR, fatalities > 0) %>%
  dplyr::filter(geocode %in% focus_countries) %>%
  iepsqlite::latlong2shp("level1") %>% mutate(geocode = substr(ID, 1, 3)) %>% 
  relocate(geocode) %>% dplyr::filter(geocode %in% focus_countries) %>% dplyr::select(geocode, year, ID, admin_level, longitude, latitude, fatalities) %>%
  group_by(ID,year, admin_level) %>% summarise(deaths = sum(fatalities), variablename = "Conflict Deaths") %>%
  ungroup()
acled$value = findInterval(acled$deaths, c(0, 25, 100, 500, 1000, 3000))

acled = acled %>% rename(ID_1=ID) %>%
  dplyr::select(-deaths) %>%
  left_join(iepsqlite::iep_get_shapefile("level1"))

tmp = tmp %>% rename(ID_1=ID) %>%
  dplyr::select(-deaths) %>%
  left_join(iepsqlite::iep_get_shapefile("level1"))

map = iep_get_shapefile("level1") %>%
  dplyr::filter(ID_0 %in% focus_countries)


p = ggplot() +
  geom_sf(data = map, fill="lightgrey", colour=NA) +
  geom_sf(data = acled, aes(geometry = geom), colour=NA, fill="#d25601", alpha=0.7, show.legend = F) +
  geom_sf(data = tmp, aes(geometry = geom), colour="lightgrey", fill="#563787", alpha=0.5, show.legend = F) +
  geom_point(data = gold, aes(longitude, latitude, colour = type), size = 2) +
  geom_sf(data = map, color="#232323", fill=NA)+
  theme_minimal() +
  labs(fill="",size="Gold Mines") +
  theme(panel.background = element_blank(),
        legend.text = element_text(colour = "#232323", size = 20),
        legend.title = element_text(colour = "#232323", size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        legend.key.height=unit(1.4, "cm"),
        legend.position= c(1.2,0.65))

legend2 <- bi_legend(pal = "PurpleOr",
                     dim = 2,
                     xlab = "Conflict Deaths",
                     ylab = "Terrorism Deaths",
                     size = 14,
                     arrows = TRUE)

x = ggdraw() +
  draw_plot(p, -0.2, 0, 1, 1) +
  draw_plot(legend2, 0.55, 0.15, 0.3, 0.3)

# ggsave("./04_outputs/maps/Gold-map.jpg", x,  height = HEIGHT, width = WIDTH)


print(p)
addWorksheet(wb, "Gold")
insertPlot(wb, "Gold", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)
print(legend2)
insertPlot(wb, "Gold", height = 4, width = 6, fileType = "jpg", startRow = 1,
           startCol = 1)


####--Line graph or table of drug types and seizures in Sahel countries over time (UNODC data)--####
#Average drug quantity per seizure per type

options(scipen = 999)

path <- file.path(IEP_USERPATH, "Research/Data/unodc-drug-seizures/IDS-data-2011_2022-Nov23.xlsx")

drug_types = rio::import(path, which = 13) %>%
  rename(Drug = 2, drug_type = 1)

sheet_names <- excel_sheets(path)

sheet_names = sheet_names[-13]

data_list <- lapply(sheet_names, function(sheet) {
  df <- read_xlsx(path, sheet)
  # Convert "DateDDMMYYYY" to a consistent datetime format
  df$DateDDMMYYYY <- as.POSIXct(df$DateDDMMYYYY, format = "%Y-%m-%d %H:%M:%S")
  # Extract the year from the sheet name and create a "Year" column
  df <- df %>% mutate(Year = as.integer(str_extract(sheet, "\\d+")))
  return(df)
})

# Combine all data frames into a single data frame
unodc_all <- bind_rows(data_list)

drug_data = unodc_all %>% left_join(drug_types) %>%
  mutate(drug_type = case_when(
    drug_type %in% c("Cannabis-type (excluding synthetic cannabinoids)") ~ "Cannabis",
    drug_type %in% c("Cocaine-type") ~ "Cocaine",
    drug_type %in% c("Amphetamine-type stimulants (excluding ecstasy)") ~ "Amphetamines",
    drug_type %in% c("Sedatives and Tranquillizers") ~ "Sedatives and Tranquillisers",
    is.na(drug_type) | drug_type %in% c("Precursors", "Solvents and Inhalants", 
                                        "New psychoactive substances",
                                        "“Ecstasy”-type substances") ~ "Other drugs/substances",
    TRUE ~ as.character(drug_type)
  ))


tmp <- drug_data %>%
  mutate(Drugquantity = if_else(is.na(Drugquantity), 0, Drugquantity)) %>%
  dplyr::filter(ISO3code %in% sahel, Year >= CURRENT_YEAR-1) %>%
  group_by(Countryofseizure, drug_type) %>%
  summarise(total_quantity = sum(Drugquantity)) %>%
  ungroup() %>%
  group_by(Countryofseizure) %>%
  mutate(value = total_quantity / sum(total_quantity) * 100) %>%
  dplyr::select(Countryofseizure, drug_type, value)

tmp$drug_type = factor(tmp$drug_type, levels= c("Opioids", "Cannabis",
                                          "Sedatives and Tranquillisers",
                                          "Cocaine",
                                          "Amphetamines",
                                          "Other drugs/substances"))

# 
# tmp = tmp %>% 
#   ungroup() %>%
#   complete(drug_type, fill = list(value = 0))


colours <- c("Cocaine" = "#F37053", 
             "Cannabis"= "#ED1D24", 
             "Opioids" = "#678696", 
             "Amphetamines" = "#2D4F5E",
             "Sedatives and Tranquillisers" = "#D1D3D4", 
             "Other drugs/substances" = "#A6A593")

p = ggplot(tmp, aes(x = Countryofseizure, 
                    y = value,
                    fill = drug_type)) + 
  geom_bar(stat = "identity", position = "stack") +
  theme_gti() + 
  scale_color_manual(values= colours) + 
  scale_fill_manual(values= colours) + 
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y = "PERCENTAGE OF DRUGS SEIZED",
    caption = "Source: UNODC, IEP calculations") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "bottom",
        legend.title =element_blank())

print(p)
addWorksheet(wb, "DrugSeizures per unit")
writeDataTable(wb, sheet = "DrugSeizures per unit", x = tmp, startCol = 1, startRow = 1)
insertPlot(wb, "DrugSeizures per unit", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)


# ###--No. of Drug Seizures in Sahel in a year by drug type
# 
# tmp = drug_data %>%
#   dplyr::filter(ISO3code %in% sahel, Year>=CURRENT_YEAR-1) %>%
#   group_by(Countryofseizure, Year, drug_type) %>%
#   summarise(seiz = n()) %>%
#   ungroup() 
# 
# tmp = tmp %>% 
#     ungroup() %>%
#     drop_na() %>%
#     complete(drug_type, fill = list(avg_quantity = 0))
#   
# drug_data$drug_type = factor(drug_data$drug_type, levels= c("Cannabis",
#                                                               "Sedatives and Tranquillisers",
#                                                               "Opioids",
#                                                               "Cocaine",
#                                                               "Other drugs/substances"))
#   
#   
#   colours <- c("Cocaine" = "#F37053", 
#                "Cannabis"= "#ED1D24", 
#                "Opioids" = "#678696", 
#                "Sedatives and Tranquillisers" = "#D1D3D4", 
#                "Other drugs/substances" = "#A6A593")
#   
#   p = ggplot(tmp, aes(x = Countryofseizure, 
#                       y = seiz,
#                       fill = drug_type)) + 
#     geom_bar(stat = "identity", position = "stack") +
#     theme_gti() + 
#     scale_color_manual(values= colours) + 
#     scale_fill_manual(values= colours) + 
#     labs(
#       title = NULL, 
#       subtitle = NULL, 
#       x = NULL,
#       y = "NUMBER OF SEIZURES",
#       caption = "Source: UNODC, IEP calculations") +
#     guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
#     theme(legend.position = "top",
#           legend.title =element_blank()) +
#     scale_y_continuous(labels = scales::label_comma())
#   
#   print(p)
#   addWorksheet(wb, "No. DrugSeizures")
#   writeDataTable(wb, sheet = "No. DrugSeizures", x = tmp, startCol = 1, startRow = 1)
#   insertPlot(wb, "No. DrugSeizures", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
#              startCol = 8)


####--Conflict events in Northern Mali overlaid with location of MINUSMA/Barkhane bases--####

withdrawn_locations = c("Tessalit", "Douentza", "Kidal", "Ansongo", "Ber",
                   "Goundam", "Ménaka", "Aguelhok", "Gossi")

active_locations = c("Bamako", "Timbuktu", "Mopti",  
                   "Gao")

tmp <- data.frame(location = c(withdrawn_locations, active_locations))

# Add a "withdrawn" column based on location
tmp <- tmp %>%
  mutate(withdrawn = location %in% withdrawn_locations) %>%
  tidygeocoder::geocode(city = location, method = 'osm', verbose = TRUE) %>%
  rename(latitude = lat, longitude = long)

BASES = c("MLI")
COUNTRY_CLAUSE = paste("WHERE geocode IN ('", paste(BASES, collapse = "','"), "')", sep = "")
acled.raw <- iepg_acled(clause = COUNTRY_CLAUSE)

acled_mali.df <- acled.raw %>%
  filter(year == GTI_YEAR, !event_type=="Strategic developments") 

# Filter data for January to June
acled_mali_first_half <- acled_mali.df %>%
  filter(event_date < ymd(paste(GTI_YEAR, "-07-01", sep = "")))

# Filter data for July to December
acled_mali_second_half <- acled_mali.df %>%
  filter(event_date >= ymd(paste(GTI_YEAR, "-07-01", sep = "")))

# Get Mali map data
mali.map <- iepg_get_gadm("level0") %>%
  filter(geocode == "MLI")

fatality_range <- range(c(acled_mali_first_half$fatalities, acled_mali_second_half$fatalities), na.rm = TRUE)

# Function to create map plot with title
create_map_plot <- function(data, title) {
  ggplot(mali.map) +
    geom_sf() +
    geom_point(data = tmp, aes(x = longitude, y = latitude, fill = as.factor(withdrawn)), 
               shape = 22, size = 4, color = "black") +
    geom_point(data = data, aes(x = longitude, y = latitude, size = fatalities), 
               color = "firebrick1", alpha = 0.5) +
    geom_text(data = tmp, aes(x = longitude, y = latitude, label = location), 
              size = 3, color = "black", fontface = "bold", 
              vjust = ifelse(tmp$location == "Timbuktu", -2, 2)) +
    scale_size_continuous(range = c(1, 10), guide = 'none', limits = fatality_range) +
    scale_fill_manual(values = c("darkgreen", "blue"), name = "",
                      labels = c("Active Bases", "Withdrawn Bases")) +
    labs(title = title) +
    theme_void()
}

# Create the two maps with titles
p1 <- create_map_plot(acled_mali_first_half, "January - June")
p2 <- create_map_plot(acled_mali_second_half, "July - December")

# Combine the maps side by side and collect legend at the top
combined_map <- p1 + p2 + plot_layout(guides = 'collect')

# Print the combined map
print(combined_map)

print(p)
addWorksheet(wb, "Mali Map")
insertPlot(wb, "Mali Map", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
            startCol = 8)


# ggsave("./04_outputs/maps/Mali-map.jpg", p, height = HEIGHT, width = WIDTH)


####--Terror attack and conflict changes before and after the MINUSMA withdrawal--####

tmp = terrorism_df %>%
  dplyr::filter(geocode=="MLI", year==CURRENT_YEAR) %>%
  mutate(start_date = as.Date(start_date),
         month = format(start_date, "%Y-%m"))

acled = acled_df %>% 
  dplyr::filter(country=="Mali", year==CURRENT_YEAR, !event_type=="Strategic developments") %>%
  mutate(event_date = as.Date(event_date),
         month = format(event_date, "%Y-%m"))

acled = acled %>% arrange(event_date) %>%
  group_by(month) %>%
  summarise(attacks = n()) 

tmp = tmp %>% arrange(start_date) %>%
  group_by(month) %>%
  summarise(attacks = n()) 

tmp = rbind(tmp, acled) %>% group_by(month) %>%
  summarise(attacks = sum(attacks))


p = ggplot(tmp, aes(x = month, y = attacks, fill = month)) +
  geom_bar(stat = "sum", position = "dodge", width = 0.7, show.legend = FALSE, fill="darkgreen") + 
  geom_vline(xintercept = which(tmp$month == "2023-07"), linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Month",
       y = "Total Attacks",
       caption = "Source: Dragonfly TerrorismTracker, ACLED, IEP calculations") +
  theme_gti() +
  scale_x_discrete(labels = month.name)


print(p)
addWorksheet(wb, "Mali Graph")
writeDataTable(wb, sheet = "Mali Graph", x = tmp, startCol = 1, startRow = 1)
insertPlot(wb, "Mali Graph", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)


####--Terror attack changes in Niger over time, past 3 years and pre/post coup--#####

tmp = terrorism_df %>%
  dplyr::filter(geocode=="NER", year>=CURRENT_YEAR-3) %>%
  mutate(start_date = as.Date(start_date),
         month_year = format(start_date, "%Y-%m"))

tmp = tmp %>% arrange(start_date) %>%
  group_by(month_year) %>%
  summarise(attacks = n()) 

tmp$month_year <- factor(tmp$month_year, levels = unique(tmp$month_year))

p = ggplot(tmp, aes(x = month_year, y = attacks, fill = month_year)) +
  geom_bar(stat = "sum", position = "dodge", width = 0.7, show.legend = FALSE, fill = "navyblue") + 
  geom_vline(xintercept = which(tmp$month_year == "2023-07"), linetype = "dashed", color = "red") +
  labs(title = "",
       x = "",
       y = "Total Attacks",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  theme_gti() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(breaks = seq(0, max(tmp$attacks), by = 1))

print(p)
addWorksheet(wb, "Niger")
writeDataTable(wb, sheet = "Niger", x = tmp, startCol = 1, startRow = 1)
insertPlot(wb, "Niger", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)


####--Changes in conflict forecast risks in coastal 
#West African states esp Benin, Togo, Ghana, Mauritania where low risk has been trending up--####

#Download latest 12 month data from here: https://conflictforecast.org/downloads and save in data folder

tmp = rio::import("./02_data/raw/latest_conflictforecast_armedconf_12.csv") %>%
  dplyr::filter(year>=CURRENT_YEAR-1, isocode %in% c(coast, sahel), month=="10") %>% #change 10 to 12 once year is up
dplyr::select(isocode, year, best_model)

tmp = tmp %>% pivot_wider(names_from = year, values_from = best_model) %>%
  mutate(percentage_change = ((`2023` - `2022`) / `2022`) * 100,
         country = countrycode(isocode, "iso3c", "country.name"))

p <- ggplot(tmp, aes(x = reorder(country, percentage_change), y = percentage_change)) +
  geom_bar(stat = "identity", fill = ifelse(tmp$percentage_change > 0, "red", "limegreen")) +
  labs(title = "",
       x = "",
       y = "Percentage Change 2022-2023",
       caption = "Source: Conflict Forecast, IEP calculations") +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  theme_gti()


print(p)
addWorksheet(wb, "Conflict Forecast")
writeDataTable(wb, sheet = "Conflict Forecast", x = tmp, startCol = 1, startRow = 1)
insertPlot(wb, "Conflict Forecast", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)


saveWorkbook(wb, file = paste0(CHARTS_PATH, "GTI-Sahel-chart-file.xlsx"), overwrite = TRUE)
