##### ----- GTI STANDARD CHARTS AND TABLES: SECTION 2
#' This script is for producing all the standard GTI Charts
#' (Biggest Risers/Fallers etc)
##### -----

### --- Libraries, Variables, Functions

# Function to load packages only if not already loaded
f_LibraryLoader(tidyverse,
                rio,
                openxlsx,
                scales,
                waterfalls,
                stringr,
                patchwork,
                padr,
                extrafont,
                raster,
                iepg,
                sf)

### -- Section 2: Standard Charts and Tables

CHART_TrendSummary = c(title = paste0("Deaths from terrorism, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                       sheet = "TrendSummary", source = SOURCE_TT,
                       xtext = "", ytext = "DEATHS FROM TERRORISM",
                       type = "Chart", position = "Normal")

MAP_SignificantChanges = c(title = paste0("Most significant changes in deaths from terrorism, ",GTI_YEAR - 3,"-", GTI_YEAR),
                           sheet = "SignificantChanges", source = SOURCE_TT,
                           xtext = "", ytext = "",
                           type = "Map", position = "Normal")

CHART_WestIdeology = c(title = paste0("Attacks in the West by ideology, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                       sheet = "WestIdeology", source = SOURCE_TT,
                       xtext = "", ytext = "TERRORIST ATTACKS",
                       type = "Chart", position = "Normal")

CHART_DeathsDistribution = c(title = paste0("Distribution of deaths from terrorism, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                             sheet = "Deaths_Distribution", source = SOURCE_TT,
                             xtext = "", ytext = "NUMBER OF COUNTRIES",
                             type = "Chart", position = "Normal")

CHART_DeathsConflict = c(title = paste0("Deaths from Terrorism by Conflict Type, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                         sheet = "DeathsConflict", source = "Terrorism Tracker, UCDP, IEP Calculations",
                         xtext = "", ytext = "DEATHS FROM TERRORISM",
                         type = "Chart", position = "Normal")

CHART_DeathsConflictWar = c(title = paste0("War"),
                         sheet = "DeathsConflictWar", source = "Terrorism Tracker, UCDP, IEP Calculations",
                         xtext = "", ytext = "DEATHS FROM TERRORISM")

CHART_DeathsConflictMinor = c(title = paste0("Minor Conflict"),
                            sheet = "DeathsConflictMinor", source = "",
                            xtext = "", ytext = "")

CHART_DeathsConflictNon = c(title = paste0("Non-Conflict"),
                              sheet = "DeathsConflictNon", source = "",
                              xtext = "", ytext = "")

TABLE_RegionSummary = c(title = paste0("Average GTI Score and Change by Region"), 
                        sheet = "RegionSummary", source = SOURCE_TT,
                        xtext = "", ytext = "",
                        type = "Table", position = "Normal")

CHART_RegionTotals = c(title = paste0("Deaths from Terrorism by Conflict Type, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                         sheet = "RegionTotals", source = SOURCE_TT,
                         xtext = "", ytext = "",
                       type = "Chart", position = "Normal")

CHART_Top3Regions  = c(title = paste0("Trend in Terrorism Deaths, Top Three Regions, ",TT_FIRST_YEAR,"-", GTI_YEAR),
                       sheet = "Top3Regions", source = SOURCE_TT,
                       xtext = "", ytext = "DEATHS FROM TERRORISM",
                       type = "Chart", position = "Normal")

TABLE_RegionTables = c(title = paste0(ActiveRegion," GTI Score, Rank and Change in Score, ",GTI_YEAR - 10, "-",GTI_YEAR), 
                        sheet = paste0("RegionTables_",ActiveRegion), source = SOURCE_TT,
                        xtext = "", ytext = "",
                       type = "Table", position = "Normal")

### --- Loading Data

# Incident Data 
tt_incidents.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  rename(group = perpetrator_type, group2 = perpetrator_aggregated) 

# Summary Data
tt_summary.df <- rio::import("02_data/processed/GTI_BandedNational.rds") 


## -- CHART_TrendSummary: Total Deaths in selected Areas------------------------
#' Shows Deaths in the areas of most interest over time. These regions need to be
#' checked each year and changed if necessary

# - Tidy the data
trend.df <- tt_summary.df %>%
  filter(year >=TT_FIRST_YEAR, year <= GTI_YEAR) %>%
  mutate(chart_region = case_when(geocode == "AFG" ~ "Afghanistan",
                            geocode == "IRQ" ~ "Iraq",
                            geocode == "PAK" ~ "Pakistan",
                            geocode %in% THE_SAHEL ~ "The Sahel",
                            geocode %in% THE_WEST ~ "The West",
                            TRUE ~ "All other countries")) %>%
  group_by(chart_region, year) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  drop_na() %>%
  complete(chart_region, year, fill = list(TOTAL_DEATHS = 0))

# - Making the Chart

# Set Factors
trend.df$chart_region = factor(trend.df$chart_region, 
                               levels= c("The West",
                                          "The Sahel",
                                          "Pakistan",
                                          "Iraq",
                                          "Afghanistan",
                                          "All other countries"))

# Set Colours
colours <- c("Afghanistan" = "#ED1F29", 
             "Iraq" = "orange", 
             "Pakistan"= "#f6e38d", 
             "The Sahel" = "#678696", 
             "The West" = "#770A1E", 
             "All other countries" = "#D1D3D4")

# Base Plot
p = ggplot(trend.df, aes(x = year, 
                    y = deaths,
                    fill = chart_region)) + 
  geom_area() + 
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))+
  scale_color_manual(values = colours) + 
  scale_fill_manual(values = colours) 

# GTI Theme
pCHART_TrendSummary <- f_ThemeGTI(p,
            chart_info = CHART_TrendSummary,
            plottitle = "",
            xaxis = "Include",
            yaxis = "",
            xgridline = "",
            ygridline = "Include")

# Spreadsheet version of data
CHART_TrendSummary.df <- trend.df %>%
  pivot_wider(names_from = "year" , values_from = "deaths")

## -- Map: Most Significant Changes --------------------------------------------

# Variables
SIGNIFICANT_START = 2012 # Start of Data
SIGNIFICANT_THRESHOLD = 2020 # Compare data before and after this year
CUTOFF_LEVEL = 0.2 # Will only show data with an absolute change bigger than this

# Tidy Data
MAP_SignificantChanges.df = rio::import("./02_data/processed/clean_tt.rds") %>% 
  filter(complete.cases(latitude,longitude)) %>%
  filter(SIGNIFICANT_START >= 2012) %>%
  filter(gpi_region %in% c("Middle East and North Africa") | geocode %in% THE_SAHEL) %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  f_LatLongShape("level1")

# Complete time series
MAP_SignificantChanges.df = MAP_SignificantChanges.df %>% 
  group_by(admin_ID) %>%
  padr::pad(start_val = min(MAP_SignificantChanges.df$date), 
            end_val = max(MAP_SignificantChanges.df$date), 
            break_above = 5,
            by = "date") %>%  # Specify the column for padding
  ungroup() %>% 
  mutate(deaths_total = replace_na(deaths_total, 0))

# Calculate Change
MAP_SignificantChanges.df <- MAP_SignificantChanges.df %>%
  arrange(date) %>% group_by(admin_ID) %>% 
  mutate(mean = cummean(deaths_total), period = ifelse(lubridate::year(date) <= SIGNIFICANT_THRESHOLD, 0, 1)) %>%
  relocate(deaths_total, mean) %>% 
  group_by(admin_ID) %>% 
  mutate(p = t.test(mean[period == 0], mean[period == 1])$p.value) %>%
  group_by(admin_ID, p, period) %>%
  summarise(mean = mean(deaths_total)) %>%
  group_by(admin_ID, p) %>% 
  arrange(period) %>% 
  summarise(change = diff(mean)) %>%
  ungroup() %>%
  filter(p < 0.05) %>%
  mutate(scale = NA) %>%
  rename(geocode = admin_ID)

pos = MAP_SignificantChanges.df$change < 0
MAP_SignificantChanges.df$scale[pos] = scales::rescale(MAP_SignificantChanges.df$change[pos], to = c(-1,0)) #hack to make colours better
MAP_SignificantChanges.df$scale[!pos] = scales::rescale(MAP_SignificantChanges.df$change[!pos], to = c(0,1))


# Admin 1 Map Data
gadm1 = iepg_get_gadm("level1")
changemap.df =  gadm1 %>% left_join(MAP_SignificantChanges.df, by = "geocode")

# Country Border Map Data
country.map <- iepg_get_gadm("level0") %>% 
  filter(gpi_region %in% c("Middle East and North Africa") | geocode %in% THE_SAHEL)

# Plot
pMAP_SignificantChanges <- ggplot() +
  geom_sf(data = country.map, fill = "#FFFAFA", color = "darkgrey", size = 0.5) +
  geom_sf(data = changemap.df %>% filter(abs(scale) > CUTOFF_LEVEL), aes(fill = scale), colour = "white", lwd = 0) +
  scale_fill_distiller(palette = "RdBu", direction = -1, 
                       breaks = c(-0.5, 0.5), labels = c("Decreasing", "Increasing"), na.value = "grey") +
  f_ChangeMapTheme() + 
  theme(legend.position = c(0.7, 0.0),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent", colour = NA)) # Transparent legend background


## -- CHART_WestIdeology: Attacks in the West by ideological motivation --------
#' Ideology set based on ideological dictionary variable. May need to update each year

# Tidy the Data
west_ideology.df <- tt_incidents.df %>%
  filter(!is.na(ideology) & the_west == "West")%>%
  group_by(year, ideology) %>%
  summarise(attacks = n(), deaths = sum(deaths_total)) %>%
  ungroup() 

## -- Make the Chart

# Set the factors
west_ideology.df$ideology <- factor(west_ideology.df$ideology, 
      levels= c("Political",
                "Religious",
                "Nationalist/Religious",
                "Nationalist",
                "Single Interest",
                "Undetermined"))

# Set factor order
west_ideology.df$ideology <- factor(west_ideology.df$ideology, 
                                    levels = rev(levels(west_ideology.df$ideology)))



# Chart colours
colours <- c("Political" = "#770A1E",
             "Religious" = "#ED1D24",
             "Nationalist/Religious" = "pink",
             "Nationalist" = "#678696",
             "Single Interest" = "#A6A593",
             "Undetermined" = "lightgrey")

# Base Plot
p <- ggplot(west_ideology.df, aes(x = year, y = attacks, fill = ideology)) +
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR))))+
  scale_fill_manual(values = colours, breaks = rev(levels(west_ideology.df$ideology)))+
  geom_bar(stat = "identity", position = "stack") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

# GTI Theme
pCHART_WestIdeology <- f_ThemeGTI(p,
                            chart_info = CHART_WestIdeology,
                            plottitle = "",
                            xaxis = "Include",
                            yaxis = "",
                            xgridline = "",
                            ygridline = "Include"
                            )  +
  theme(legend.position = "top")

# Spreadsheet version of data
CHART_WestIdeology.df <- west_ideology.df %>%
  select(-deaths) %>%
  pivot_wider(names_from = "ideology" , values_from = "attacks")
CHART_WestIdeology.df[] <- lapply(CHART_WestIdeology.df, function(x) replace(x, is.na(x), 0))

## -- Distribution of deaths ---------------------------------------------------

# Tidy the Data
DeathsDistribution.df = tt_summary.df %>%
  filter(year>= TT_FIRST_YEAR, year<= GTI_YEAR) %>%
  group_by(geocode, year) %>%
  filter(deaths > 0) %>%
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(deaths_cat = case_when(deaths >=1 & deaths <=24 ~ "1-24",
                                deaths >= 25 & deaths<=99 ~ "25-99",
                                deaths >=100 & deaths <=499 ~ "100-499",
                                deaths >= 500 & deaths <=999 ~ "500-999",
                                deaths > 999 ~ "1000+")) %>%
  group_by(year, deaths_cat) %>%
  summarise(value = n_distinct(geocode))

# Set Factors
DeathsDistribution.df$deaths_cat = factor(DeathsDistribution.df$deaths_cat, 
                                     levels= c("1000+",
                                               "500-999",
                                                "100-499",
                                                "25-99",
                                                "1-24"))
# Factor Order
DeathsDistribution.df$deaths_cat = factor(DeathsDistribution.df$deaths_cat,  
                                          levels = rev(levels(DeathsDistribution.df$deaths_cat)))

# Chart Colours
colours <- c("1000+"  = "#770A1E", 
             "500-999" = "#F37053", 
             "100-499"= "#ED1D24", 
             "25-99" = "#FDC16E",
             "1-24" = "#f6e38d")


all_years <- unique(DeathsDistribution.df$year) # for setting the limits of the x-axis on the chart

# Base Plot
p = ggplot(DeathsDistribution.df, aes(x = year, y = value, fill = deaths_cat)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(limits = all_years) +
scale_fill_manual(values = colours, guide = guide_legend(reverse = TRUE))

# GTI Theme
pCHART_DeathsDistribution <- f_ThemeGTI(p,
                            chart_info = CHART_DeathsDistribution,
                            plottitle = "",
                            xaxis = "Include",
                            yaxis = "",
                            xgridline = "",
                            ygridline = "Include"
                            )  +
  theme(legend.position = "top")

# Spreadsheet version of data
CHART_DeathsDistribution.df <- DeathsDistribution.df %>%
  pivot_wider(names_from = "deaths_cat" , values_from = "value")

## -- Deaths by Conflict Type --------------------------------------------------

# tidy data
war.df = tt_incidents.df %>%
  filter(intensity == "war") %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths_total))

minorconflict.df = tt_incidents.df %>%
  filter(intensity == "minor conflict") %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths_total))

# Non-conflict 
nonconflict.df = tt_incidents.df %>%
  filter(intensity == "non-conflict") %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths_total))

# War Plot
p <- ggplot(data= war.df, aes(x=year,y= deaths)) +
  geom_line(linewidth = 0.75, colour = "black") + 
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))

# War Plot - GTI Theme  
pCHART_DeathsConflictWar <- f_ThemeGTI(p,
                              chart_info = CHART_DeathsConflictWar,
                              plottitle = "Include",
                              xaxis = "Include",
                              yaxis = "",
                              xgridline = "",
                              ygridline = "Include"
  ) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0,10000))

# Minor Plot
p <- ggplot(data= minorconflict.df, aes(x=year,y= deaths)) +
  geom_line(linewidth = 0.75, colour = "black") + 
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))

# Minor Plot - GTI Theme  
pCHART_DeathsConflictMinor <- f_ThemeGTI(p,
                                       chart_info = CHART_DeathsConflictMinor,
                                       plottitle = "Include",
                                       xaxis = "Include",
                                       yaxis = "",
                                       xgridline = "",
                                       ygridline = "Include"
) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0,4000))

# Non-Conflict Plot
p <- ggplot(data= nonconflict.df, aes(x=year,y= deaths)) +
  geom_line(linewidth = 0.75, colour = "black") + 
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))

# Minor Plot - GTI Theme  
pCHART_DeathsConflictNon <- f_ThemeGTI(p,
                                         chart_info = CHART_DeathsConflictNon,
                                         plottitle = "Include",
                                         xaxis = "Include",
                                         yaxis = "",
                                         xgridline = "",
                                         ygridline = "Include"
) +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0,750))


# combine the plots
pCHART_DeathsConflict = pCHART_DeathsConflictWar + pCHART_DeathsConflictMinor + pCHART_DeathsConflictNon

# Combine the data
CHART_DeathsConflict.df <- war.df %>%
  left_join(minorconflict.df, by = "year") %>%
  left_join(nonconflict.df, by = "year") %>%
  rename("War" = deaths.x, "Minor Conflict" = deaths.y, "Non-Conflict" = deaths)


## -- Regions: Region Summary Table --------------------------------------------

# Tidy Data
TABLE_RegionSummary.df <- tt_summary.df %>%
  select(region, year, banded_score) %>%
  filter(year == GTI_YEAR | year == GTI_YEAR - 10 | year == GTI_YEAR - 1) %>%
  group_by(region, year) %>%
  summarise(banded_score = round(mean(banded_score),3)) %>%
  pivot_wider(names_from = year, values_from = banded_score, names_prefix = "score_") %>%
  ungroup() %>%
  mutate(
    change_10 = !!sym(paste0("score_", GTI_YEAR)) - !!sym(paste0("score_", GTI_YEAR - 10)),
    change_1 = !!sym(paste0("score_", GTI_YEAR)) - !!sym(paste0("score_", GTI_YEAR - 1))
  ) %>%
  select(region,!!sym(paste0("score_", GTI_YEAR)), change_10, change_1) %>%
  rename_with(
    .cols = everything(),
    .fn = function(name) {
      case_when(
        name == paste0("score_", GTI_YEAR) ~ "Average Score",
        name == "change_10" ~ paste0("Change ", GTI_YEAR - 10, "-", GTI_YEAR),
        name == "change_1" ~ paste0("Change ", GTI_YEAR - 1, "-", GTI_YEAR),
        TRUE ~ name
      )
    }
  )

## -- Regions: Total Attacks and Deaths ----------------------------------------

#Tidy Data
regional_spreadsheet.df <- tt_summary.df %>%
  select(region, deaths, incidents) %>%
  group_by(region) %>%
  summarise("Attacks" = sum(incidents),
            "Deaths" = sum(deaths))

CHART_RegionTotals.df <- regional_spreadsheet.df %>%
  pivot_longer(cols = c("Attacks", "Deaths"), names_to = "type", values_to = "value")

# base plot
p = ggplot(pCHART_RegionTotals.df, aes(x = reorder(region, value), y = value, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Attacks" = "peachpuff", "Deaths" = "red2"))

# GTI Theme
pCHART_RegionTotals <- f_ThemeGTI(p,
                            chart_info = CHART_RegionTotals,
                            plottitle = "",
                            xaxis = "",
                            yaxis = "Include",
                            xgridline = "Include",
                            ygridline = ""
                            ) +
  theme(legend.position = c(0.9, 0.2),
        legend.justification = c("right", "bottom"),
        legend.title =element_blank())+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) 

## -- Region Trends: Deaths in Top 3 Regions -----------------------------------

# -  Tidy Data
top3.list = tt_summary.df %>%
  group_by(region) %>%
  summarise(deaths = sum(deaths)) %>%
  arrange(desc(deaths)) %>% 
  slice(1:3) %>%
  pull(region)

regiontrends.df = tt_summary.df %>% 
  filter(region %in% top3.list) %>%
  group_by(region, year) %>%
  summarise(deaths = sum(deaths))

# Base Plot
p <- ggplot(regiontrends.df, aes(x = year, y = deaths, color = region)) + 
  geom_line(linewidth = 1) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))+
  labs(title = "", x = "", y = "DEATHS FROM TERRORISM", 
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  scale_color_manual(values = c("#FEC779", "#770A1E",
                                "deepskyblue4")) 

pCHART_Top3Regions <- f_ThemeGTI(p,
                            chart_info = CHART_Top3Regions,
                            plottitle = "",
                            xaxis = "Include",
                            yaxis = "",
                            xgridline = "",
                            ygridline = "Include"
) +
  theme(legend.position = "top")

# Spreadsheet data
CHART_Top3Regions.df = regiontrends.df %>% 
  pivot_wider(names_from = "year" , values_from = "deaths" )


## -- Regional Tables ----------------------------------------------------------
#' Note that these tables are added dynamically using the f_RegionalTable function

# Tidy Data
RegionTables.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>% 
  dplyr::filter(year == GTI_YEAR | year == GTI_YEAR - 1 | year == GTI_YEAR - 10) %>%
  select(country, year, region, banded_score, rank) %>%
  pivot_wider(names_from = year, values_from = c(banded_score, rank)) %>%
  mutate(change_10 = get(paste0("banded_score_", GTI_YEAR)) - get(paste0("banded_score_", GTI_YEAR - 10))) %>%
  mutate(change_1 = get(paste0("banded_score_", GTI_YEAR)) - get(paste0("banded_score_", GTI_YEAR - 1))) %>%
  select(country, region, !!sym(paste0("banded_score_", GTI_YEAR)), !!sym(paste0("rank_", GTI_YEAR)), change_1, change_10) %>%
  rename(Country = country, 
         `Overall Score` = !!sym(paste0("banded_score_", GTI_YEAR)), 
         `Overall Rank` = !!sym(paste0("rank_", GTI_YEAR)))

# List of Regions
REGIONS = unique(region.df$region)

