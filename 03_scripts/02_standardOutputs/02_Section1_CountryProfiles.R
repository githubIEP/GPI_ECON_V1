## -- Country Profile Visualizations -------------------------------------------
#' For each country, calculate total incidents, dead, and injured in most recent year,
#' Line chart of deaths trends, and pie chart of target types

f_LibraryLoader(tidyverse,
                sf,
                iepg,
                scales,
                patchwork,
                extrafont)

tt_summary.df <- rio::import("02_data/processed/GTI_BandedNational.rds") 
tt_incidents.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  rename(group = perpetrator_name, group2 = perpetrator_aggregated) 

# Empty dataset to bind top incident description
descriptions.df = NULL

# Get top 10 countries
top10.list <- tt_summary.df %>% 
  ungroup() %>% 
  dplyr::filter(year == GTI_YEAR) %>% 
  arrange(desc(banded_score)) %>% 
  head(n = 10) %>% 
  pull(geocode)

top10table.df <- tt_summary.df %>% 
  filter(geocode %in% top10.list, year == GTI_YEAR) %>% 
  arrange(desc(banded_score)) %>% 
  select(country,rank, banded_score, deaths, incidents, injured)

rio::export(top10table.df, paste0(TABLE_FILES,"/top10countryprofiles.csv"),row.names = FALSE)

# Get top 10 country shapefiles
f_top10ShapeFiles(top10.list)


for(i in seq(1,10)){

# Set Active Country
focus_country = top10.list[i]

## -- Create line chart of terrorist attacks
countrytrend.df = tt_summary.df %>% 
  filter(geocode == focus_country) %>%
  select(geocode, year, deaths)

CHART_Profile1 = c(title = paste0(comma(sum(countrytrend.df$deaths)), " deaths from terrorism since ",TT_FIRST_YEAR), 
                   sheet = paste0("Top10_",as.character(focus_country)), source = "", xtext = "", ytext = "")

# Base plot
p1 <- ggplot(countrytrend.df, aes(x = year, y = deaths, 
                       colour = GTI_COLOURS[1])) + 
  geom_line(linewidth = 1)

# GTI Theme
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
targets.list <- tt_incidents.df %>%
  filter(geocode == focus_country, year == GTI_YEAR) %>%
  count(targets, sort = TRUE) %>%
  slice_max(n = 3, order_by = n) %>%
  pull(targets)

countrytargets.df <- tt_incidents.df %>%
  filter(geocode == focus_country, year == GTI_YEAR) %>%
  mutate(targets = ifelse(targets %in% targets.list, as.character(targets), "Other")) %>%
  count(targets) %>%
  arrange(desc(n))

countrytargets.df = countrytargets.df  %>%
  mutate(perc_inc = n/sum(n)) %>%
  select(targets, perc_inc) %>%
  mutate(sort_key = ifelse(targets == "Other", -1, perc_inc)) %>%
  arrange(desc(sort_key)) %>%
  mutate(targets = factor(targets, levels = unique(targets[order(sort_key, decreasing = FALSE)])))

p3 <- ggplot(countrytargets.df, aes(x = 2, y = perc_inc, fill = targets)) +
  geom_bar(width = 0.7, stat = "identity") +
  coord_polar(theta = "y") +
  xlim(c(1, 2.5)) +
  labs(fill = "", 
       title = paste0("Attack Targets in ",GTI_YEAR)) +
  theme_void() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(l = 80))) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("grey",GTI_COLOURS[2],GTI_COLOURS[3], GTI_COLOURS[1], GTI_COLOURS[5]))

pCHART_profile = (p1 | p3) + plot_layout(widths = c(7, 3))


svg_profile <- paste0(CHART_FILES, "/top10_profileheader", focus_country, ".svg")
ggsave(svg_profile, pCHART_profile, device = "svg", width = 18, height = 7, units = CHART_UNIT)


  # Filter the attacks
  countryattacks.df <- tt_incidents.df %>% 
    filter(year == GTI_YEAR, deaths_total >= 0, geocode == focus_country) %>% 
    select(event_id, date, geocode, latitude, longitude, deaths_total,summary) %>%
    distinct()
  
  # Get the map of the country
  map_path = paste0("02_data/raw/Shapefiles/gadm41_", focus_country, "_0.shp")
  country.map <- st_read(map_path) %>%
    rename(geocode = GID_0)
  
  # Add attacks to map
  countryattacks.df <- countryattacks.df %>%
    left_join(country.map)
  
  # Get single biggest attacks
  biggest_single.df = countryattacks.df %>%
    slice_max(deaths_total, n = 1) %>%
    f_LatLongShape("level1")
  
  incident_summary = biggest_single.df$summary
  incident_region = biggest_single.df$admin_Name

  # Map Plot
  p <- ggplot() +
    geom_sf(data = country.map) + 
    geom_point(data = countryattacks.df, 
               aes(x = longitude, y = latitude, size = deaths_total),
               alpha = 0.4,
               colour = "darkred") +
    geom_point(data = biggest_single.df,
               aes(x = longitude, y = latitude, size = deaths_total),
               shape = 1, 
               stroke = 1.5) +
    scale_x_continuous(expand = expansion(add = c(0.25,5))) + 
    theme_void() + 
    theme(legend.position = "none") + 
    scale_size_continuous(range = c(min(countryattacks.df$deaths_total+1), max(countryattacks.df$deaths_total) * 0.05))
  
  # Add description of event to country name
  my_description = data.frame("geocode" = focus_country, "Summary" = incident_summary, "Area" = incident_region)
  descriptions.df = rbind(descriptions.df,my_description)
  
  svg_map <- paste0(MAP_FILES, "/top10_", focus_country, ".svg")
  ggsave(svg_map, p, device = "svg", width = 8, height = 8, units = CHART_UNIT)
  
}

rio::export(descriptions.df,paste0(MAP_FILES,"/top10countryincidents.csv"))