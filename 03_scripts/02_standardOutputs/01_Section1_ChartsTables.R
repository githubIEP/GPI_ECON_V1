##### ----- GTI STANDARD CHARTS AND TABLES: SECTION 1
#' This script is for producing all the standard GTI Charts
#' (Biggest Risers/Fallers etc)


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
                svglite)

### --- List of Standard Charts and Tables in Section 1

# 20 Deadliest groups: Table (Will not always be included)
TABLE_20Groups = c(title = "20 Deadliest Terrorist Groups", 
                   sheet = "20Groups", source = SOURCE_TT, xtext = "", ytext = "",
                   type = "Table", position = "Normal")

# Total Deaths - Five countries YoY
CHART_5Countries = c(title = paste0("Total terrorism deaths by country, ",GTI_YEAR - 1,"-", GTI_YEAR),
                     sheet = "5Countries", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM",
                     type = "Chart", position = "Normal")

# Waterfall Chart: Deaths by Country
CHART_wfDeaths = c(title = paste0("Deaths from terrorism by country, ", GTI_YEAR),
                   sheet = "wfDeaths", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM",
                   type = "Chart", position = "Normal")

# Largest Decreases in Deaths
CHART_DeathDecreases = c(title = paste0("Largest decreases in deaths from terrorism, ",GTI_YEAR - 1,"-", GTI_YEAR),
                         sheet = "DeathDecreases", source = SOURCE_TT, xtext = "", ytext = "CHANGE IN DEATHS",
                         type = "Chart", position = "Normal")

# Largest Increases in Deaths
CHART_DeathIncreases = c(title = paste0("Largest increases in deaths from terrorism, ",GTI_YEAR - 1,"-", GTI_YEAR),
                         sheet = "DeathIncreases", source = SOURCE_TT, xtext = "", ytext = "CHANGE IN DEATHS",
                         type = "Chart", position = "Normal")

# Deadliest Terrorist Groups - Line
CHART_TerroristGroups = c(title = paste0("Four deadliest terrorist groups in, ", GTI_YEAR),
                   sheet = "TerroristGroups", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM",
                   type = "Chart", position = "Normal")

# Deadliest Terrorist Groups - Stacked Area
CHART_Top4Area = c(title = paste0("Four deadliest terrorist groups in, ", GTI_YEAR),
                   sheet = "Top4Area", source = "", xtext = "", ytext = "% OF DEATHS",
                   type = "Chart", position = "Normal")

# Table of most impact
TABLE_Top10Impacted = c(title = paste0("Ten countries most impacted by terrorism, ", GTI_FIRST_YEAR,"-",GTI_YEAR),
                        sheet = "Top10Impacted", source = SOURCE_TT, xtext = "", ytext = "",
                        type = "Table", position = "Normal")

### --- Loading Data

# Incident Data 
tt_incidents.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  rename(group = perpetrator_name, group2 = perpetrator_aggregated) 

# Summary Data
tt_summary.df <- rio::import("02_data/processed/GTI_BandedNational.rds") 

### --- GTI Section 1

## -- TABLE_20Groups -----------------------------------------------------------
# ' A table this lists the 20 deadliest terrorist groups for the latest year

# Transform Data
tab_topGroups.df <- tt_incidents.df %>% 
  filter(year == GTI_YEAR & !group2 == "Unknown", 
          !grepl("junta", group)) %>% # Removes problematic Myanmar associated groups from consideration
  group_by(group2) %>%
    summarise(attacks = n(),
            deaths = sum(deaths_total),
            injured = sum(injured_total)) %>%
  arrange(desc(deaths)) %>% 
  slice(1:20)

## -- CHART_5Countries ---------------------------------------------------------
#' A chart that shows the distribution of deaths over two years for the five
#' countries with the most deaths in the most recent year

# Function to make table of top 5 countries for this year and previous year
ty_Top5.df <- f_TopNCountries(tt_summary.df, 5, GTI_YEAR, "deaths")

# Calculate the total deaths for the same top countries in the previous year
py.df <- tt_summary.df  %>%
  filter(year == GTI_YEAR - 1) %>%
  select(country,deaths)

py_Top5.df <-py.df %>%
  filter(country %in% ty_Top5.df$country)

py_Other.df <- py.df %>%
  anti_join(py_Top5.df, by = "country") %>%
  summarise(country = "All other countries", deaths = sum(deaths))

py_Top5.df <- py_Top5.df %>% bind_rows(py_Other.df) %>%
  mutate(year = GTI_YEAR - 1)

# Combine into single table, factorize deaths and years
chart_Top5.df <- ty_Top5.df %>% 
  bind_rows(py_Top5.df) %>%
  group_by(year, country) %>%
  summarise(deaths=sum(deaths)) %>%
  ungroup() %>%
  mutate(year = factor(year),
         country = factor(country)) %>% 
  group_by(year) %>%
  arrange(desc(deaths)) 

# Base Chart
p <- ggplot(chart_Top5.df, aes(x = year, y = deaths, 
                               fill = factor(country, 
                                             levels = c(setdiff(country, "All other countries"), "All other countries")))) +
  geom_bar(stat = "identity", position = "stack") + # stacked bar chart
  geom_text(aes(label = ifelse(deaths > 1000, comma(deaths), as.character(deaths))), # labels on the chart
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("red4", "red", "pink", "lightgrey", "grey35", "darkslategrey")) + # colours for the chart
  scale_y_continuous(labels = scales::label_comma(), expand = c(0,0)) # comma separators
  
# GTI Theme Chart
pCHART_5Countries <- f_ThemeGTI(
  p, 
  chart_info = CHART_5Countries,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") 

# pivot data wider for spreadsheet
CHART_5Countries.df <- chart_Top5.df %>%
  pivot_wider(names_from = year, values_from = deaths)

## -- CHART_wfDeaths -----------------------------------------------------------
#' Waterfall chart that shows deaths and percentages for top 10 countries and
#' the rest of the world

# Get ten countries with most deaths
top10_deaths.df <- tt_summary.df %>%
  filter(year == GTI_YEAR) %>%
  slice_max(order_by = deaths, n = 10) %>%
  select(country,deaths)

# Calculate deaths for Rest of World
ROW_deaths.df <- tt_summary.df %>%
  filter(year == GTI_YEAR, !country %in% top10_deaths.df$country) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(country = "Rest of World") %>%
  select(country,deaths)

# Combine the two dataframes
CHART_wfDeaths.df <- top10_deaths.df %>% 
  bind_rows(ROW_deaths.df) %>%
  mutate(perc = round(deaths / sum(deaths) * 100, 0),
         labels = "",
         cumulative = cumsum(deaths),
         cumulative_lag = lag(cumulative, default = 0),
         country = factor(country, levels = unique(country))) %>%
  arrange(cumulative_lag)

# Wrap Country Names for chart
CHART_wfDeaths.df$country_wrapped <- str_wrap(CHART_wfDeaths.df$country, width = 5)

# Base Chart with custom elements
p <- waterfall(CHART_wfDeaths.df, 
               rect_text_labels = CHART_wfDeaths.df$labels,  # Keep original labels for bars
               draw_lines = FALSE,
               rect_border = NA,
               fill_colours = colorRampPalette(c("darkred", "darkred"))(11), fill_by_sign = FALSE) +
  annotate("text", 
           x = seq_along(CHART_wfDeaths.df$perc), 
           y = (CHART_wfDeaths.df$cumulative + CHART_wfDeaths.df$cumulative_lag) / 2, 
           label = paste0(CHART_wfDeaths.df$perc, "%"), 
           vjust = 0.5, 
           colour = "white",
           size = 3, 
           fontface = "bold") +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = seq(0, max(CHART_wfDeaths.df$cumulative), by = 2000)) +
  scale_x_discrete(labels = CHART_wfDeaths.df$country_wrapped)  # Use wrapped labels for x-axis

# Add GTI chart theme
pCHART_wfDeaths <- f_ThemeGTI(
  p, 
  chart_info = CHART_wfDeaths,
  plottitle = "",
  xaxis = "",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") 

## -- CHART_DeathDecreases, CHART_DeathIncreases -------------------------------
#' Largest Increases and Decreases in Death by Country

# Arrange dataframe
death_changes.df <- tt_summary.df %>%
  dplyr::filter(year %in% c(GTI_YEAR, GTI_YEAR - 1)) %>%
  dplyr::select(country, year, deaths) %>%
  pivot_wider(names_from = year, values_from = deaths) %>%
  mutate(diff_deaths = get(paste0(GTI_YEAR)) - get(paste0(GTI_YEAR - 1))) %>%
  arrange(diff_deaths)

CHART_DeathDecreases.df <- death_changes.df %>% 
  slice_min(order_by = diff_deaths, n = 10)

CHART_DeathIncreases.df <- death_changes.df %>%
  slice_max(order_by = diff_deaths, n = 10) %>%
  arrange(diff_deaths)

# Wrap names 
CHART_DeathDecreases.df$country_wrapped <- str_wrap(CHART_DeathDecreases.df$country, width = 5)
CHART_DeathIncreases.df$country_wrapped <- str_wrap(CHART_DeathIncreases.df$country, width = 5)


# Set base chart - Decreases
p = ggplot(CHART_DeathDecreases.df, aes(x = reorder(country, diff_deaths), y = diff_deaths)) +
  geom_col(fill = "red3") +
  geom_text(aes(label = diff_deaths, y = diff_deaths), 
            vjust = 1, size = 3, fontface = "bold") + 
  scale_y_continuous(labels = f_LabelFormatter) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))

# Add Theme
pCHART_DeathDecreases <- f_ThemeGTI(
  p, 
  chart_info = CHART_DeathDecreases,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include")

# Set base chart - Increases
p = ggplot(CHART_DeathIncreases.df, aes(x = reorder(country,diff_deaths), y = diff_deaths)) +
  geom_col(fill = "red3") +
  geom_text(aes(label = diff_deaths, y = diff_deaths), 
            vjust = -1, size = 3, fontface = "bold") + 
  scale_y_continuous(labels = f_LabelFormatter) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))

# Add Theme
pCHART_DeathIncreases <- f_ThemeGTI(
  p, 
  chart_info = CHART_DeathIncreases,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include")
  
## -- CHART_TerroristGroups ----------------------------------------------------
#' Combination of a line chart and stacked (100%) area chart

# Wrangle Data
groups.df <- tt_incidents.df %>% 
  select(group2, year, deaths_total) %>%
  filter(!is.na(deaths_total)) %>%
  group_by(group2, year) %>% 
  summarise(deaths = sum(deaths_total)) %>%
  ungroup() 

groups_top4 <- groups.df %>%
  filter(year == GTI_YEAR, group2 != "Unknown or Undefined") %>%
  arrange(desc(deaths)) %>%
  slice_head(n = 4) %>%
  mutate(group2 = as.character(group2)) %>%
  pull(group2)

groups.df <- groups.df %>%
  filter(!group2 %in% c("Unknown or Undefined")) %>%
  mutate(group2 = if_else(group2 %in% groups_top4, as.character(group2), "All Other Groups"))%>%
  group_by(group2, year) %>% 
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(group2 = fct_reorder(group2, deaths, .fun = sum))

groups_top4.df <- groups.df %>% 
  complete(year = full_seq(year, 1), group2, fill = list(deaths = 0)) %>%
  group_by(group2) 
  
# Data in spreadsheet format
groups_top4Spreadsheet.df <- groups_top4.df %>%
  pivot_wider(names_from = year, values_from = deaths) %>%
  mutate(type = "total deaths")


# Make the plot
p <- ggplot(groups_top4.df, aes(x = year, y = deaths, color = group2)) + 
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))+
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0)) +
  scale_color_manual(values = GTI_COLOURS)

pCHART_Top4Line <- f_ThemeGTI(
  p, 
  chart_info = CHART_TerroristGroups,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "top")

## -- Combine with Area Chart

# Calculate percentages
groups.df <- groups.df %>%
  complete(year = full_seq(year, 1), group2, fill = list(deaths = 0))

total_deaths_per_year <- groups.df %>%
  group_by(year) %>%
  summarize(total_deaths = sum(deaths)) 

# Calculate percentage of deaths for each group
area.df <- groups.df %>%
  left_join(total_deaths_per_year, by = "year") %>%
  mutate(perc_deaths = deaths / total_deaths * 100)

# data in spreadsheet format
area_spreadsheet.df <- area.df %>%
  select(group2, year, perc_deaths) %>%
  pivot_wider(names_from = year, values_from = perc_deaths) %>%
  arrange(`2023`) %>%
  mutate(type = "percentage")

# Make the chart
p <- ggplot(area.df, aes(x=year, y=perc_deaths, fill=group2)) + 
  geom_area() +
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2))) +
  scale_fill_manual(values = GTI_COLOURS) +
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0))

pCHART_Top4Area <- f_ThemeGTI(
  p, 
  chart_info = CHART_Top4Area,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "off")

# Spreadsheet data
CHART_TerroristGroups.df <- rbind(groups_top4Spreadsheet.df, area_spreadsheet.df)

# Combine the two charts
pCHART_TerroristGroups = (pCHART_Top4Line | pCHART_Top4Area)

## -- TABLE_Top10Impacted ------------------------------------------------------
#' Table of GTI rankings for ten countries most impacted by terrorism in latest year

# Data Wrangling
TABLE_Top10Impacted.df = tt_summary.df %>% 
  filter(year >= GTI_FIRST_YEAR) %>%
  select(year,country,rank) %>%
  pivot_wider(names_from = year, values_from = rank) %>%
  filter((!!sym(as.character(GTI_YEAR))) <= 10) %>%
  arrange(!!sym(as.character(GTI_YEAR)))

