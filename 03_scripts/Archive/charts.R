##### ----- GTI STANDARD CHARTS AND TABLES
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
                #webr,
                padr)

## -- Spreadsheet to save charts in
tc_wb <- createWorkbook()

### --- List of Standard Charts and Tables

# Commonly used sources
SOURCE_TT = "Terrorism Tracker, IEP Calculations"

## -- Section 1 - Results

# 20 Deadliest groups
TABLE_20Groups = c(title = "20 Deadliest Terrorist Groups", 
                   sheet = "20Groups", source = SOURCE_TT, xtext = "", ytext = "")

# Total Deaths - Five countries YoY
CHART_5Countries = c(title = paste0("Total terrorism deaths by country, ",GTI_YEAR - 1,"-", GTI_YEAR),
                     sheet = "Deaths_Top5", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM")

# Waterfall Chart: Deaths by Country
CHART_wfDeaths = c(title = paste0("Deaths from terrorism by country, ", GTI_YEAR),
                     sheet = "Deaths_Top10wf", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM")

# Largest Decreases in Deaths
CHART_DeathDecreases = c(title = paste0("Largest decreases in deaths from terrorism, ",GTI_YEAR - 1,"-", GTI_YEAR),
                         sheet = "DeathDecreases", source = SOURCE_TT, xtext = "", ytext = "CHANGE IN DEATHS")

# Largest Increases in Deaths
CHART_DeathIncreases = c(title = paste0("Largest increases in deaths from terrorism, ",GTI_YEAR - 1,"-", GTI_YEAR),
                         sheet = "DeathIncreases", source = SOURCE_TT, xtext = "", ytext = "CHANGE IN DEATHS")

# Deadliest Terrorist Groups - Line
CHART_Top4Line = c(title = paste0("Four deadliest terrorist groups in, ", GTI_YEAR),
                   sheet = "Top4Groups", source = SOURCE_TT, xtext = "", ytext = "DEATHS FROM TERRORISM")

# Deadliest Terrorist Groups - Stacked Area
CHART_Top4Area = c(title = paste0("Four deadliest terrorist groups in, ", GTI_YEAR),
                   sheet = "Top4Groups", source = "", xtext = "", ytext = "% OF DEATHS")

# Section 2 - Trends
#' list of standard charts from section 2

### --- Loading Data

# Incident Data 
tt_incidents.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  rename_with(~ gsub("perpetrators_0(\\d)_name", "group_\\1", .x),
              starts_with("perpetrators_") & ends_with("_name")) %>%
  rename_with(~ gsub("perpetrators_0(\\d)_type", "groupType_\\1", .x),
              starts_with("perpetrators_") & ends_with("_type"))

# Summary Data
tt_summary.df <- rio::import("02_data/processed/GTI_BandedNational.rds") 

### --- GTI Section 1

## -- TABLE_20Groups -----------------------------------------------------------
# ' A table this lists the 20 deadliest terrorist groups for the latest year

# Set Active Chart
CHART_ACTIVE = TABLE_20Groups

# Add sheet to the charts and tables spreadsheet
f_GTISheet(tc_wb, CHART_ACTIVE)

# Transform Data
tab_topGroups <- tt_incidents.df %>% 
  filter(year == GTI_YEAR & !group_1 == "Unknown", 
          !grepl("\\(undefined\\)|\\(undetermined\\)", group_1),
          !grepl("junta", group_1)) %>%
  group_by(group_1) %>%
    summarise(attacks = n(),
            deaths = sum(deaths_total),
            injured = sum(injured_total)) %>%
  arrange(desc(deaths)) %>% 
  slice(1:20)

# Write to spreadsheet  
f_GTISheetTable(tab_topGroups, CHART_ACTIVE)
saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)

# Write to tables folder
write.csv(tab_topGroups,paste0(o_TABLES, TABLE_20Groups["sheet"],".csv"),row.names = FALSE)

## -- CHART_5Countries ---------------------------------------------------------
#' A chart that shows the distribution of deaths over two years for the five
#' countries with the most deaths in the most recent year

# Set active chart
CHART_ACTIVE = CHART_5Countries

# Add sheet to chart workbook
f_GTISheet(tc_wb, CHART_ACTIVE)

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

# Add data to Spreadsheet
chart_Top5_spreadsheet.df <- chart_Top5.df %>%
  pivot_wider(names_from = year, values_from = deaths)

f_GTISheetTable(chart_Top5_spreadsheet.df, CHART_ACTIVE)

# Make the base chart with any custom elements
p <- ggplot(chart_Top5.df, aes(x = year, y = deaths, 
                               fill = factor(country, 
                                             levels = c(setdiff(country, "All other countries"), "All other countries")))) +
  geom_bar(stat = "identity", position = "stack") + # stacked bar chart
  geom_text(aes(label = ifelse(deaths > 1000, comma(deaths), as.character(deaths))), # labels on the chart
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(values = c("red4", "red", "pink", "lightgrey", "grey35", "darkslategrey")) + # colours for the chart
  scale_y_continuous(labels = scales::label_comma(), expand = c(0,0)) # comma separators
  
# Add the GTI theme
pCHART_ACTIVE <- f_ThemeGTI(
  p, 
  chart_info = CHART_ACTIVE,
  plottitle = "Include",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") 

# Save the charts as jpgs and pdfs for layout
f_GTISavePlots(CHART_ACTIVE,pCHART_ACTIVE)

# Insert image into the chart file for reference
f_GTISheetImage(CHART_ACTIVE)

# Save workbook
saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)

## -- CHART_wfDeaths -----------------------------------------------------------
#' Waterfall chart that shows deaths and percentages for top 10 countries and
#' the rest of the world

#Set Active Chart
CHART_ACTIVE = CHART_wfDeaths

# Add sheet to chart workbook
f_GTISheet(tc_wb, CHART_ACTIVE)

# Get ten countries with most deaths
top10_deaths.df <- tt_summary.df %>%
  filter(year == GTI_YEAR) %>%
  slice_max(order_by = deaths, n = 10) %>%
  select(country,deaths)

# Calculate deaths for ROW
ROW_deaths.df <- tt_summary.df %>%
  filter(year == GTI_YEAR, !country %in% top10_deaths.df$country) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(country = "Rest of World") %>%
  select(country,deaths)

# Combine the two dataframes
top10_deaths.df <- top10_deaths.df %>% 
  bind_rows(ROW_deaths.df) %>%
  mutate(perc = round(deaths / sum(deaths) * 100, 0),
         labels = "",
         cumulative = cumsum(deaths),
         cumulative_lag = lag(cumulative, default = 0),
         country = factor(country, levels = unique(country))) %>%
  arrange(cumulative_lag)

# Add Data to the spreadsheet
f_GTISheetTable(top10_deaths.df, CHART_ACTIVE)

# Wrap Country Names
top10_deaths.df$country_wrapped <- str_wrap(top10_deaths.df$country, width = 5)

# Base Chart with custom elements
p <- waterfall(top10_deaths.df, 
               rect_text_labels = top10_deaths.df$labels,  # Keep original labels for bars
               draw_lines = FALSE,
               rect_border = NA,
               fill_colours = colorRampPalette(c("darkred", "darkred"))(11), fill_by_sign = FALSE) +
  annotate("text", 
           x = seq_along(top10_deaths.df$perc), 
           y = (top10_deaths.df$cumulative + top10_deaths.df$cumulative_lag) / 2, 
           label = paste0(top10_deaths.df$perc, "%"), 
           vjust = 0.5, 
           colour = "white",
           size = 3, 
           fontface = "bold") +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = seq(0, max(top10_deaths.df$cumulative), by = 2000)) +
  scale_x_discrete(labels = top10_deaths.df$country_wrapped)  # Use wrapped labels for x-axis

pCHART_ACTIVE <- f_ThemeGTI(
  p, 
  chart_info = CHART_ACTIVE,
  plottitle = "",
  xaxis = "",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") 

# Save the charts as jpgs and pdfs for layout
f_GTISavePlots(CHART_ACTIVE,pCHART_ACTIVE)

# Insert image into the chart file for reference
f_GTISheetImage(CHART_ACTIVE)
saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)

## -- CHART_DeathDecreases, CHART_DeathIncreases -------------------------------
#' Largest Increases and Decreases in Death by Country

# Add sheets to chart workbook
f_GTISheet(tc_wb, CHART_DeathDecreases)
f_GTISheet(tc_wb, CHART_DeathIncreases)

# Arrange dataframe
death_changes.df <- tt_summary.df %>%
  dplyr::filter(year %in% c(GTI_YEAR, GTI_YEAR - 1)) %>%
  dplyr::select(country, year, deaths) %>%
  pivot_wider(names_from = year, values_from = deaths) %>%
  mutate(diff_deaths = get(paste0(GTI_YEAR)) - get(paste0(GTI_YEAR - 1))) %>%
  arrange(diff_deaths)

decreases.df <- death_changes.df %>% 
  slice_min(order_by = diff_deaths, n = 10)

increases.df <- death_changes.df %>%
  slice_max(order_by = diff_deaths, n = 10)

# Write Data to the spreadsheet
f_GTISheetTable(decreases.df, CHART_DeathDecreases)
f_GTISheetTable(increases.df, CHART_DeathIncreases)

# Wrap names 
decreases.df$country_wrapped <- str_wrap(decreases.df$country, width = 5)
increases.df$country_wrapped <- str_wrap(increases.df$country, width = 5)

# Set base chart - Decreases
p = ggplot(decreases.df, aes(x = reorder(country, diff_deaths), y = diff_deaths)) +
  geom_col(fill = "red3") +
  geom_text(aes(label = diff_deaths, y = diff_deaths), 
            vjust = 1, size = 3, fontface = "bold") + 
  scale_y_continuous(labels = f_LabelFormatter) +
  scale_x_discrete(labels = decreases.df$country_wrapped) 

# Add Theme
pCHART_DeathDecreases <- f_ThemeGTI(
  p, 
  chart_info = CHART_DeathDecreases,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include")

# Export
f_GTISavePlots(CHART_DeathDecreases,pCHART_DeathDecreases)
f_GTISheetImage(CHART_DeathDecreases)

# Set base chart - Increases
p = ggplot(increases.df, aes(x = reorder(country, diff_deaths), y = diff_deaths)) +
  geom_col(fill = "red3") +
  geom_text(aes(label = diff_deaths, y = diff_deaths), 
            vjust = -1, size = 3, fontface = "bold") + 
  scale_y_continuous(labels = f_LabelFormatter) +
  scale_x_discrete(labels = increases.df$country_wrapped) 

# Add Theme
pCHART_DeathIncreases <- f_ThemeGTI(
  p, 
  chart_info = CHART_DeathIncreases,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include")

# Export
f_GTISavePlots(CHART_DeathIncreases,pCHART_DeathIncreases)
f_GTISheetImage(CHART_DeathIncreases)
saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)
  
## -- CHART_Top4Line -----------------------------------------------------------
#' Line chart of four deadliest terrorist groups, excluding unknown
#' Combine this chart with the one below

# Add new worksheet
f_GTISheet(tc_wb, CHART_Top4Line)

# Wrangle Data
groups.df <- tt_incidents.df %>% 
  select(perpetrator_name, year, deaths_total) %>%
  mutate(group = case_when(
    is.na(perpetrator_name) ~ "Unknown or Undefined",
    grepl("^Islamic State", perpetrator_name) ~ "Islamic State",
    grepl("undetermined|undefined|unknown|Unknown", perpetrator_name, ignore.case = TRUE) ~ "Unknown or Undefined",
    TRUE ~ as.character(perpetrator_name))) %>%
  filter(!is.na(deaths_total)) %>%
  group_by(group, year) %>% 
  summarise(deaths = sum(deaths_total)) %>%
  ungroup() 

groups_top4 <- groups.df %>%
  filter(year == GTI_YEAR, group != "Unknown or Undefined") %>%
  arrange(desc(deaths)) %>%
  slice_head(n = 4) %>%
  pull(group)

groups.df <- groups.df %>%
  mutate(group = if_else(!(group %in% c(groups_top4, "Unknown or Undefined")), "All Other Groups", as.character(group)))%>%
  group_by(group, year) %>% 
  summarise(deaths = sum(deaths)) %>%
  ungroup() %>%
  mutate(group = fct_reorder(group, deaths, .fun = sum))

groups_top4.df <- groups.df %>% 
  complete(year = full_seq(year, 1), group, fill = list(deaths = 0)) %>%
  group_by(group) %>%
  filter(!group %in% c("Unknown or Undefined", "All Other Groups"))

# Add to charts spreadsheet
groups_top4Spreadsheet.df <- groups_top4.df %>%
  pivot_wider(names_from = year, values_from = deaths)

f_GTISheetTable(groups_top4Spreadsheet.df, CHART_Top4Line)

# Make the plot
p <- ggplot(groups_top4.df, aes(x = year, y = deaths, color = group)) + 
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = c(seq(paste0(TT_FIRST_YEAR), paste0(GTI_YEAR), 2)))+
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0)) +
  scale_color_manual(values = c("#678696", "#F9B298", "#ED1D24",
                                "#770A1E")) 

pCHART_Top4Line <- f_ThemeGTI(
  p, 
  chart_info = CHART_Top4Line,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "top")

## -- CHART_Top4Area -------------------------------------------------

# Calculate percentages
total_deaths_per_year <- groups.df %>%
  group_by(year) %>%
  summarize(total_deaths = sum(deaths))

# Calculate percentage of deaths for each group
groups.df <- groups.df %>%
  left_join(total_deaths_per_year, by = "year") %>%
  mutate(perc_deaths = round(deaths / total_deaths * 100),3)

# Add data to spreadsheet
groups_spreadsheet.df <- groups.df %>%
  select(group, year, perc_deaths) %>%
  pivot_wider(names_from = year, values_from = perc_deaths) %>%
  arrange(`2023`)

# Add to charts spreadsheet
writeDataTable(tc_wb, sheet = CHART_Top4Line[["sheet"]], x = groups_spreadsheet.df, startCol = 3, startRow = 15)

# Make the chart
p <- ggplot(groups.df, aes(x=year, y=perc_deaths, fill=group)) + 
  geom_area() +
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2))) +
  scale_fill_manual(values = c("#678696", "#F9B298", "#ED1D24","#770A1E",  "lightgrey", "darkgrey")) +
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0))

pCHART_Top4Area <- f_ThemeGTI(
  p, 
  chart_info = CHART_Top4Area,
  plottitle = "",
  xaxis = "Include",
  yaxis = "",
  xgridline = "",
  ygridline = "Include") +
  theme(legend.position = "top")

# Combine the two charts
pCHART_Top4Combined = (pCHART_Top4Line | pCHART_Top4Area)
f_GTISavePlots(CHART_Top4Line,pCHART_Top4Combined)
f_GTISheetImage(CHART_Top4Line)
saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)


## -- TABLE_Top10Impacted ------------------------------------------------------
#' Table of GTI rankings for ten countries most impacted by terrorism in latest year

# Set active chart and add sheet
CHART_ACTIVE = TABLE_Top10Impacted
f_GTISheet(tc_wb, CHART_ACTIVE)

# Data Wrangling
top10.df = tt_summary.df %>% 
  filter(year >= GTI_FIRST_YEAR) %>%
  select(year,country,rank) %>%
  pivot_wider(names_from = year, values_from = rank) %>%
  filter((!!sym(as.character(GTI_YEAR))) <= 10) %>%
  arrange(!!sym(as.character(GTI_YEAR)))

# Write table to spreadsheet
f_GTISheetTable(top10.df, CHART_ACTIVE)

# Write table to tables folder
write.csv(top10.df,paste0(o_TABLES, CHART_ACTIVE["sheet"],".csv"),row.names = FALSE)


# gt(table) %>%
#   data_color(columns = 2:14,
#              colors = scales::col_numeric(
#                palette = c("darkred", "red", "white")))



## -- Country Profile Visualizations -------------------------------------------
#' For each country, calculate total incidents, dead, and injured in most recent year,
#' Line chart of deaths trends, and pie chart of target types

# Prepare Data
cp_Top10.df = tt_summary.df %>% filter(year == GTI_YEAR, rank<=10)
cp_Top10list = cp_Top10.df %>% pull(country)
cp_Top10iso = cp_Top10.df %>% pull(geocode)


for(i in seq(1,10,1)){
  
  ## -- Set Active Country
  ACTIVE_CODE = cp_Top10iso[i]
  ACTIVE_COUNTRY = cp_Top10list[i]
  
  ## -- Create line chart of terrorist attacks
  tmp1 = tt_summary.df %>% 
    filter(geocode == ACTIVE_CODE) %>%
    select(geocode, year, deaths)
  
  CHART_Profile1 = c(title = paste0(comma(sum(tmp1$deaths)), " deaths from terrorism since ",TT_FIRST_YEAR), 
                     sheet = paste0("Top10_",as.character(ACTIVE_CODE)), source = "", xtext = "", ytext = "")
  
  #addWorksheet(wb, sheetName = as.character(focus_code))
  #writeDataTable(wb, sheet = as.character(focus_code), x = tmp1, startCol = 1, startRow = 1)
  
  # Base plot
  p1 <- ggplot(tmp1, aes(x = year, y = deaths, 
                         colour = GTI_COLOURS[1])) + 
    geom_line(size = 1.25)
  
  # Theming
  p1 <- f_ThemeGTI(
    p1,
    chart_info = CHART_Profile1,
    plottitle = "Include",
    xaxis = "Include",
    yaxis = "",
    xgridline = "",
    ygridline = "Include") +
    theme(legend.position="none",
          plot.margin = margin(t = 5),
          plot.caption = element_blank(), 
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12)) +
    scale_x_continuous(breaks = c(TT_FIRST_YEAR, GTI_YEAR))+
    scale_y_continuous(labels = scales::label_comma(), expand = c(0,0))
  
  ## -- Attacks by target 
  tmp3 <- tt_incidents.df %>%
    filter(geocode == ACTIVE_CODE, year == GTI_YEAR) %>%
    count(targets, sort = TRUE) %>%
    slice_max(n = 3, order_by = n) %>%
    pull(targets)
  
  tmp_3 <- tt_incidents.df %>%
    filter(geocode == ACTIVE_CODE, year == GTI_YEAR) %>%
    mutate(targets = ifelse(targets %in% tmp3, as.character(targets), "Other")) %>%
    count(targets) %>%
    arrange(desc(n))
  
  tmp3 = tmp_3  %>%
    mutate(perc_inc = n/sum(n)) %>%
    select(targets, perc_inc) %>%
    mutate(sort_key = ifelse(targets == "Other", -1, perc_inc)) %>%
    arrange(desc(sort_key)) %>%
    mutate(targets = factor(targets, levels = unique(targets[order(sort_key, decreasing = FALSE)])))
  
  #nwriteDataTable(wb, sheet = as.character(focus_code), x = tmp3, startCol = 9, startRow = 1)
  
  
  p3 <- ggplot(tmp3, aes(x = 2, y = perc_inc, fill = targets)) +
    geom_bar(width = 0.7, stat = "identity") +
    coord_polar(theta = "y") +
    xlim(c(1, 2.5)) +
    labs(fill = "", 
         title = paste0("Attack targets in ",GTI_YEAR)) +
    theme_void() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(l = 80))) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("grey",GTI_COLOURS[2],GTI_COLOURS[3], GTI_COLOURS[1]))
  
  pCHART_profile = (p1 | p3) + plot_layout(widths = c(7, 3))
}
 

####---Section 2 Charts ---#####
wb <- createWorkbook()

##Deaths from Terrorism
sahel = c("NER", "BFA", "NGA", "MLI", "TCD", "MRT",
                    "GMB", "SEN", "GIN", "CMR")


tmp = terrorism_df %>%
  dplyr::filter(year>=MIN_YEAR, year<=CURRENT_YEAR) %>%
  mutate(region = case_when(geocode == "AFG" ~ "Afghanistan",
                               geocode == "IRQ" ~ "Iraq",
                               geocode == "PAK" ~ "Pakistan",
                               geocode %in% c(sahel) ~ "Sahel",
                               gpi_region %in% c("Europe","North America") ~ "Europe and North America",
                               TRUE ~ "All other countries")) %>%
  group_by(region, year) %>%
  summarise(deaths = sum(deaths_total))

# Completing missing years if it is needed   
tmp = tmp %>% 
  ungroup() %>%
  drop_na() %>%
  complete(region, year, fill = list(TOTAL_DEATHS = 0))

tmp$region = factor(tmp$region, levels= c("Europe and North America",
                                          "Sahel",
                                          "Pakistan",
                                          "Iraq",
                                          "Afghanistan",
                                          "All other countries"))

addWorksheet(wb, "Deaths from Terrorism")
writeDataTable(wb, sheet = "Deaths from Terrorism", x = tmp, startCol = 1, startRow = 1)

colours <- c("Afghanistan"  = "#770A1E", 
               "Iraq" = "#F37053", 
               "Pakistan"= "#ED1D24", 
               "Sahel" = "#678696", 
               "Europe and North America" = "#D1D3D4", 
               "All other countries" = "#A6A593")



p = ggplot(tmp, aes(x = year, 
                          y = deaths,
                          fill = region)) + 
  geom_area() + 
  # theme_gti() + 
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2)))+
  scale_color_manual(values= colours) + 
  scale_fill_manual(values= colours) + 
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y = "DEATHS FROM TERRORISM",
    caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "top",
        legend.title =element_blank()) +
  scale_y_continuous(labels = scales::label_comma())


print(p)
insertPlot(wb, "Deaths from Terrorism", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 5)

### Distribution of deaths by terrorism (stacked bar chart with categories of deaths per year)

tmp = terrorism_df %>%
  dplyr::filter(year>=MIN_YEAR, year<=CURRENT_YEAR) %>%
  dplyr::filter(deaths_total >0) %>%
  group_by(geocode, year) %>%
  summarise(deaths = sum(deaths_total)) %>%
  ungroup() %>%
  mutate(deaths_cat = case_when(deaths >=1 & deaths <=24 ~ "1-24",
                                     deaths >= 25 & deaths<=99 ~ "25-99",
                                     deaths >=100 & deaths <=499 ~ "100-499",
                                     deaths >= 500 & deaths <=999 ~ "500-999",
                                     deaths > 999 ~ "1000+")) %>%
  group_by(year, deaths_cat) %>%
  summarise(geocode = n_distinct(geocode))


tmp$deaths_cat = factor(tmp$deaths_cat, levels= c("1000+",
                                                  "500-999",
                                                  "100-499",
                                                  "25-99",
                                                  "1-24"))

all_years <- unique(tmp$year)
tmp$deaths_cat <- factor(tmp$deaths_cat, levels = rev(levels(tmp$deaths_cat)))

colours <- c("1000+"  = "#770A1E", 
               "500-999" = "#F37053", 
               "100-499"= "#ED1D24", 
               "25-99" = "#FDC16E",
               "1-24" = "lightgrey")


addWorksheet(wb, "Deaths Distribution")
writeDataTable(wb, sheet = "Deaths Distribution", x = tmp, startCol = 1, startRow = 1)

p = ggplot(tmp, aes(x = year, y = geocode, fill = deaths_cat)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_x_discrete(limits = all_years) +
  scale_fill_manual(values = colours, guide = guide_legend(reverse = TRUE)) + 
   labs(title = NULL, subtitle = NULL, x = NULL, y = "DEATHS FROM TERRORISM",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = "top", legend.title = element_blank())


print(p)
insertPlot(wb, "Deaths Distribution", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 5)

### Deaths by Conflict Type

addWorksheet(wb, "Deaths by Conflict")

 
tmp1 = terrorism_df %>%
  dplyr::filter(intensity == "war") %>%
  group_by(year) %>%
  summarise(war_deaths = sum(deaths_total))

writeDataTable(wb, sheet = "Deaths by Conflict", x = tmp1, startCol = 1, startRow = 1)

## War
p1 = ggplot(data=tmp1, 
                  aes(x=year,
                      y=war_deaths)) +
  geom_line(size=1.5,
            colour = "black") + 
  scale_y_continuous(labels = scales::label_comma(), limits = c(0,10000)) +
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2))) +
  annotate("text",label=paste0("WAR"), x=2018.5,y=7500,color = "black", fontface="bold") +
  theme_minimal() +
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y = "DEATHS FROM TERRORISM",
    caption = "Source: Dragonfly TerrorismTracker, UCDP, IEP calculations")


# Minor
tmp2 = terrorism_df %>%
  dplyr::filter(intensity == "minor conflict") %>%
  group_by(year) %>%
  summarise(minor_conflict_deaths = sum(deaths_total))

writeDataTable(wb, sheet = "Deaths by Conflict", x = tmp2, startCol = 4, startRow = 1)

p2 = ggplot(data=tmp2, 
                  aes(x=year,
                      y=minor_conflict_deaths)) +
  geom_line(size=1.5,
            colour = "red") + 
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, 5000)) + 
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2))) +
  theme_minimal() +
  annotate("text",label=paste0("MINOR ARMED CONFLICT"), x=2013.5,y=4500,color = "red", fontface="bold") +
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y=NULL)

# Non-conflict 
tmp3 = terrorism_df %>%
  dplyr::filter(intensity == "non-conflict") %>%
  group_by(year) %>%
  summarise(non_conflict_deaths = sum(deaths_total))

writeDataTable(wb, sheet = "Deaths by Conflict", x = tmp3, startCol = 7, startRow = 1)

p3 = ggplot(data=tmp3, 
                  aes(x=year,
                      y=non_conflict_deaths)) +
  geom_line(size=1.5,
            colour = "forestgreen") + 
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, 600)) + 
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2))) +
  theme_minimal() +
  annotate("text",label=paste0("NON-CONFLICT"), x=2013,y=550,color = "forestgreen", fontface="bold") +
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y=NULL)


# combine the plots
p = p1 + p2 + p3


print(p)
insertPlot(wb, "Deaths by Conflict", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 20,
           startCol = 1)

###Lethality Rate - Conflict and Non-conflict

tmp = terrorism_df %>%
  group_by(year, conflict) %>%
  summarise(deaths = sum(deaths_total),
            attacks = n()) %>%
  mutate(lethality = deaths/attacks)

addWorksheet(wb, "Lethality Rate")
writeDataTable(wb, sheet = "Lethality Rate", x = tmp, startCol = 1, startRow = 1)


p = ggplot(data = tmp,
       aes(x=year,
           y=lethality,
           colour = conflict)) +
  geom_line(size = 1.5)  +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5)) +
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2))) +
  scale_color_manual(values = c("non-conflict" = "navy", "conflict" = "red"),
                     labels = function(x) str_to_title(x)) +
  labs(title = "",
       x = "",
       y = "AVERAGE DEATHS PER ATTACK",
       caption = "Source: UCDP, Dragonfly TerrorismTracker, IEP calculations") +
  theme_gti() +
  theme(legend.position = c(.998, .998),
        legend.justification = c("right", "top"),
        legend.title =element_blank())


print(p)
insertPlot(wb, "Lethality Rate", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 8)

####The West - Terror Attacks by Motivation

tmp = terrorism_df %>%
  dplyr::filter(`The West` == "West") %>%
  mutate(motivation = case_when(
    perpetrators_1_type %in% c("Global Islamist", "Other Religious Extremist/Cultist") ~ "Religious",
    perpetrators_1_type %in% c("Far Left/Revolutionary", "Far Right/Extreme Right") ~ "Ideological",
    perpetrators_1_type == "Nationalist/Separatist" ~ "Nationalist/Separatist",
    perpetrators_1_type %in% c("Undetermined", NA) ~ "Unclear",
    TRUE ~ "Other")) %>%
  group_by(year, motivation) %>%
  summarise(attacks = n()) %>%
  ungroup() %>%
  drop_na() %>%
  complete(motivation, year, fill = list(attacks = 0))

tmp$motivation = factor(tmp$motivation, levels= c("Ideological",
                                                  "Religious",
                                                  "Nationalist/Separatist",
                                                  "Other",
                                                  "Unclear"))

tmp$motivation <- factor(tmp$motivation, levels = rev(levels(tmp$motivation)))


addWorksheet(wb, "West Attacks")
writeDataTable(wb, sheet = "West Attacks", x = tmp, startCol = 1, startRow = 1)



colours <- c("Ideological" = "red4",
             "Religious" = "gold",
             "Nationalist/Separatist" = "navy",
             "Other" = "forestgreen",
             "Unclear" = "grey")

p = ggplot(tmp, aes(x = year, y = attacks, fill = motivation)) +
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR))))+
  scale_fill_manual(values = colours, breaks = rev(levels(tmp$motivation)))+
  geom_bar(stat = "identity", position = "stack") +
  theme_gti() +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(title = "",
       x = "",
       y = "NUMBER OF ATTACKS",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  theme(legend.position = "top",
        legend.title =element_blank())


print(p)
insertPlot(wb, "West Attacks", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 5)


###Deaths by motivation

tmp = terrorism_df %>%
  dplyr::filter(`The West` == "West") %>%
  mutate(motivation = case_when(
    perpetrators_1_type %in% c("Global Islamist", "Other Religious Extremist/Cultist") ~ "Religious",
    perpetrators_1_type %in% c("Far Left/Revolutionary", "Far Right/Extreme Right") ~ "Ideological",
    perpetrators_1_type == "Nationalist/Separatist" ~ "Nationalist/Separatist",
    perpetrators_1_type %in% c("Undetermined", NA) ~ "Unclear",
    TRUE ~ "Other"),
    deaths_total = ifelse(is.na(deaths_total), 0, deaths_total)) %>%
  group_by(motivation) %>%
  summarise(deaths = sum(deaths_total)) %>%
  dplyr::filter(!motivation=="Other")

addWorksheet(wb, "West Deaths")
writeDataTable(wb, sheet = "West Deaths", x = tmp, startCol = 1, startRow = 1)


p = ggplot(tmp, aes(x = reorder(motivation, -deaths), y = deaths)) +
  geom_col(fill = "navy") +
  theme_gti() +
  labs(title = "",
       x = "",
       y = "DEATHS OF TERRORISM",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations")


print(p)
insertPlot(wb, "West Deaths", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startCol = 4,
           startRow = 1)

###Regional Trends
## GTI Score, Rank and Change in Score by Region

tmp = gti_national %>%
  dplyr::filter(year>=paste0(BASE_YEAR)) %>%
  group_by(region, year) %>%
  summarise(average = mean(banded_score)) %>%
  pivot_wider(names_from = year, values_from = average) %>%
  mutate(change_year = get(paste0(CURRENT_YEAR)) - get(paste0(PREVIOUS_YEAR)),
            change_decade = get(paste0(CURRENT_YEAR)) - get(paste0(BASE_YEAR))) %>%
  rename(average_score = (paste0(CURRENT_YEAR))) %>%
  dplyr::select(region, average_score, change_decade, change_year) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  arrange(desc(average_score))

addWorksheet(wb, "Regional Trends")
writeDataTable(wb, sheet = "Regional Trends", x = tmp, startCol = 1, startRow = 1)

###Attacks and Deaths by Region 2007-

tmp = terrorism_df %>%
  mutate(deaths_total = ifelse(is.na(deaths_total), 0, deaths_total)) %>%
  group_by(iep_region) %>%
  summarise("Attacks" = n(),
            "Deaths" = sum(deaths_total))

tmp <- tmp %>%
  pivot_longer(cols = c("Attacks", "Deaths"), names_to = "type", values_to = "value")

addWorksheet(wb, "Regional AttacksDeaths")
writeDataTable(wb, sheet = "Regional AttacksDeaths", x = tmp, startCol = 1, startRow = 1)

p = ggplot(tmp, aes(x = reorder(iep_region, value), y = value, fill = type)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = ifelse(value > 1000, scales::label_comma()(value), as.character(value))), 
            hjust = -0.2, 
            position = position_dodge(0.9),
            colour = "black") +
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = c("Attacks" = "peachpuff", "Deaths" = "red2")) +
  theme(legend.position = c(0.9, 0.2),
        legend.justification = c("right", "bottom"),
        legend.title =element_blank())+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = "",
       x = "",
       y = "",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations")


print(p)
insertPlot(wb, "Regional AttacksDeaths", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startCol = 5,
           startRow = 1)

####Trend in Terrorism deaths by region - top 3 regions

top3 = terrorism_df %>%
  group_by(iep_region) %>%
  summarise(deaths = sum(deaths_total)) %>%
  arrange(desc(deaths)) %>% 
  slice(1:3) %>%
  pull(iep_region)

tmp = terrorism_df %>% 
  dplyr::filter(iep_region %in% top3) %>%
  group_by(iep_region, year) %>%
  summarise(deaths = sum(deaths_total))

addWorksheet(wb, "Region Line")
writeDataTable(wb, sheet = "Region Line", x = tmp, startCol = 1, startRow = 1)

p = ggplot(tmp, aes(x = year, y = deaths, color = iep_region)) + 
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_blank()) + 
  scale_x_continuous(breaks = c(seq(paste0(MIN_YEAR), paste0(CURRENT_YEAR), 2)))+
  labs(title = "", x = "", y = "DEATHS FROM TERRORISM", 
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  scale_y_continuous(labels = scales::label_comma(), limits = c(0, NA)) +
  scale_color_manual(values = c("grey", "red4",
                                "deepskyblue4")) +
  theme(legend.position = c(.998, .998),
        legend.justification = c("right", "top"),
        legend.title =element_blank())


print(p)
insertPlot(wb, "Region Line", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 5)

###Attacks by weapons by region
options(scipen =999)

tmp = terrorism_df %>% 
  mutate(weapon = case_when(weapons_1 %in% c("Explosives", "UXO and Mines", "Rockets",
                                           "Grenade", "Missiles", "Mortars") ~ "Explosives",
                            weapons_1 == "Melee Weapons" ~ "Melee Weapons",
                            weapons_1 == "Firearms" ~ "Firearms",
                            weapons_1 == "Incendiaries" ~ "Incendiaries",
                            weapons_1 == "Vehicle" ~ "Vehicle",
                                TRUE ~ "Other")) %>%
  group_by(iep_region, weapon) %>%
  summarise(attacks = n()) %>%
  ungroup() %>%
  complete(iep_region, weapon, fill = list(attacks = 0)) %>%
  group_by(iep_region) %>%
  mutate(perc_attacks = attacks/sum(attacks))



tmp$weapon = factor(tmp$weapon, levels= c("Explosives",
                                          "Firearms",
                                          "Incendiaries",
                                          "Melee Weapons",
                                          "Vehicle",
                                          "Other"))

tmp$weapon <- factor(tmp$weapon, levels = rev(levels(tmp$weapon)))

addWorksheet(wb, "Weapons by Region")
writeDataTable(wb, sheet = "Weapons by Region", x = tmp, startCol = 1, startRow = 1)



colours <- c("Explosives" = "red4",
             "Firearms" = "red",
             "Incendiaries" = "pink",
             "Melee Weapons" = "gold",
             "Vehicle" = "grey",
             "Other" = "navy")



p = ggplot(tmp, aes(factor(iep_region, levels = rev(unique(iep_region))), y = perc_attacks, fill = weapon)) +
  scale_fill_manual(values = colours, breaks = rev(levels(tmp$weapon)))+
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  theme_gti() +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = label_percent()) +
  labs(title = "",
       x = "",
       y = "",
       caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  theme(legend.position = "top",
        legend.title =element_blank()) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


print(p)
insertPlot(wb, "Weapons by Region", height = HEIGHT, width = WIDTH, fileType = "jpg",
           startRow = 1,
           startCol = 6)


###Regional Trends Tables
regions = unique(gti_national$region)
region_tables <- list()

for(i in seq_along(regions)){
    focus_region = regions[i]
    
  region_data <- gti_national %>%
    dplyr::select(ID_0, country, banded_score, year, rank) %>%
    dplyr::filter(year %in% c(BASE_YEAR, CURRENT_YEAR, PREVIOUS_YEAR))
  
  region_pivot <- region_data %>%
    pivot_wider(names_from = year, values_from = c(banded_score, rank))
  
  region_pivot <- region_pivot %>%
    group_by(ID_0) %>%
    mutate(!!paste0("Change ", BASE_YEAR, "-", CURRENT_YEAR) := !!sym(paste0("banded_score_", CURRENT_YEAR)) - !!sym(paste0("banded_score_", BASE_YEAR)),
           !!paste0("Change ", PREVIOUS_YEAR, "-", CURRENT_YEAR) := !!sym(paste0("banded_score_", CURRENT_YEAR)) - !!sym(paste0("banded_score_", PREVIOUS_YEAR))) %>%
        rename("Overall Score" := !!sym(paste0("banded_score_", CURRENT_YEAR)), 
           "Overall Rank" := !!sym(paste0("rank_", CURRENT_YEAR))) %>% iepsqlite::add_region() %>%
    mutate_at(vars(matches("^Change|^Overall Score$")), ~ round(., 3)) %>%
    dplyr::select(region, ID_0, country, "Overall Score", "Overall Rank", starts_with("Change")) %>%
    arrange(`Overall Rank`) %>%
    dplyr::filter(region %in% focus_region) %>%
    ungroup()
  
  # Calculate regional average for Change columns
  regional_averages <- region_pivot %>%
    group_by(region) %>%
    summarise(across(starts_with("Change"), mean, na.rm = TRUE)) %>%
    mutate(across(starts_with("Change"), ~ round(., 3)))
  
  regional_averages$country <- "Regional Average"
  
  region_pivot <- bind_rows(region_pivot, regional_averages) %>%
    mutate_all(~ifelse(is.na(.), "", .))
  
  # Add each region's data as a new sheet in the workbook
  sheet_name <- substr(focus_region, 1, 10) 
  addWorksheet(wb, sheetName = as.character(sheet_name))
  writeData(wb, sheet = sheet_name, x = region_pivot)
}

saveWorkbook(tc_wb, CHARTBOOK, overwrite = TRUE)

###Epicentre Map Test




sahel_countries = c("BFA", "CMR", "TCD", "MLI", "NER", "GMB", "GIN", "NGA", "SEN", "MRT")

tmp = readRDS("./02_data/processed/clean_tt.rds") %>%
  dplyr::filter(complete.cases(longitude)) %>% dplyr::filter(complete.cases(latitude)) %>%
  dplyr::filter(year >= 2012) %>%
  dplyr::filter(gpi_region %in% c("Middle East and North Africa") | geocode %in% sahel_countries) %>%
  dplyr::select(-"last_appeared_in_tt", -"disappeared_from_tt_on") %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y"))  %>% 
  latlong2shp("level1")
countries = unique(tmp$geocode)

cutoff_yr = 2020

tmp = tmp %>% group_by(ID) %>%
  padr::pad(start_val = min(tmp$date), end_val = max(tmp$date), break_above = 5) %>%
  ungroup() %>% mutate(deaths_total = replace_na(deaths_total, 0))


tmp = tmp %>% arrange(date) %>% group_by(ID) %>% 
  mutate(mean = cummean(deaths_total), period = ifelse(lubridate::year(date) <= cutoff_yr, 0, 1)) %>%
  relocate(deaths_total, mean) %>% 
  group_by(ID) %>% 
  mutate(p = t.test(mean[period == 0], mean[period == 1])$p.value) %>%
  group_by(ID, p, period) %>%
  summarise(mean = mean(deaths_total)) %>%
  group_by(ID, p) %>% 
  arrange(period) %>% 
  summarise(change = diff(mean)) %>%
  ungroup() %>%
  dplyr::filter(p < 0.05) %>%
  mutate(scale = NA)
pos = tmp$change < 0
tmp$scale[pos] = scales::rescale(tmp$change[pos], to = c(-1,0)) #hack to make colours better
tmp$scale[!pos] = scales::rescale(tmp$change[!pos], to = c(0,1))
plot(tmp$change, tmp$scale)
cutoff_lvl = 0.3

gadm = iep_get_shapefile("level1")
gadm = gadm %>% left_join(tmp %>% rename(ID_1 = ID))

bbox = gadm %>% na.omit() %>%
  st_bbox()

map <- ggplot() + geom_sf(data = gadm %>% dplyr::filter(abs(scale) > cutoff_lvl), aes(fill = scale), colour = NA, lwd = 0) +
  gpi_map_theme()
map = map +   scale_fill_distiller(palette = "RdBu", direction = -1, 
                                   breaks = c(-0.5, 0.5), labels = c("Decreasing", "Increasing" ), na.value = "white")
map = add_world_boundaries(map) +
  labs(fill = "")
map = map + coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], expand = FALSE)
map = map + labs(title = paste("Most Significant Changes in Deaths from Terrorism since", cutoff_yr)) +
  gpi_map_theme() + theme(legend.position = "right")

map

# # cities = dbReadTable(con, "cities") %>% filter(capital == "primary") %>% group_by(city_id) %>%
# #   top_n(1, population) %>% filter(ID_2 %in% unique(tmp$ID)) %>% ungroup()
# gadm2 = gadm %>% sf::st_centroid() %>%sf::st_coordinates() %>% cbind(gadm) %>% filter(!is.na(scale)) %>%
#   sf::st_drop_geometry() %>% dplyr::select(X, Y, COUNTRY) %>% distinct()
# map = map + ggrepel::geom_label_repel(data = gadm2, aes(x = X, y = Y, label = COUNTRY), size = 2)
# # ggsave(map, filename = "./src/gti-2023/03-section-3/charts/01-mena-sahel-change-admin-map.png", height = 3)
# # ggsave(map, filename = "./src/gti-2023/03-section-3/charts/01-mena-sahel-change-admin-map.pdf")
# 
# 


