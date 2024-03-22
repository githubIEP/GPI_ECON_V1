####--Terror attacks in coastal West African states--####

coast = c("BEN", "TGO", "GHA", "CIV", "MRT", "GMB", "GIN", 
          "SLE", "LBR")

tmp = gti_national %>% dplyr::filter(ID_0 %in% coast, year>= CURRENT_YEAR-5) %>%
  dplyr::select(country, year, incidents_total)

p = ggplot(tmp, aes(x = year, 
                    y = incidents_total,
                    fill = country)) + 
  geom_area() +
  theme_gti() +
  labs(
    title = NULL, 
    subtitle = NULL, 
    x = NULL,
    y = "TERROR ATTACKS",
    caption = "Source: Dragonfly TerrorismTracker, IEP calculations") +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(legend.position = "top",
        legend.title =element_blank()) +
  scale_y_continuous(labels = scales::label_comma())

print(p)
addWorksheet(wb, "Attacks on Coast")
writeDataTable(wb, sheet = "Attacks on Coast", x = tmp, startCol = 1, startRow = 1)
insertPlot(wb, "Attacks on Coast", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 8)

####--Terror Attacks in Coastal West Africa - Map - 5 Years--####

west_africa = c("BEN", "TGO", "GHA", "CIV", "MRT", "NER",
                "BFA", "NGA", "MLI", "MRT",
                "GMB", "SEN", "GIN", "CMR", "SLE", "LBR")

tmp = terrorism_df %>% dplyr::filter(geocode %in% coast, year>= CURRENT_YEAR-5)
shp = iep_get_shapefile("level0") %>%
  dplyr::filter(ID_0 %in% west_africa)

shp1 = iep_get_shapefile("level0") %>%
  dplyr::filter(ID_0 %in% coast)

p <- ggplot() +
  geom_sf(data = shp,  fill="lightgray", colour=NA, linewidth=0.9) +
  geom_sf(data = shp1,  fill="slategray1", colour=NA, linewidth=0.9)+
  geom_point(data = tmp, aes(longitude, latitude, size = deaths_total), color = "firebrick1", alpha = 0.5, show.legend = F) +
  geom_sf(data = shp, color="#232323", fill=NA, linewidth=0.5)+
  geom_text(data = shp1, aes(LON, LAT, label = NAME), size = 3.5, fontface = "bold",
            hjust = ifelse(shp1$NAME == "Gambia", 2.2, 0.5)) +
  theme_void()

print(p)
addWorksheet(wb, "Coast Map")
insertPlot(wb, "Coast Map", height = HEIGHT, width = WIDTH, fileType = "jpg", startRow = 1,
           startCol = 1)

# Comparison with Positive Peace
PPI.db <- f_dbIEPCombine(1:60, "PPI")
PPI.df <- PPI.db %>% 
  filter(year == 2022 | year == 2009) %>%
  filter(type == "banded" & indicator = "well-Functioning Government") %>%
  select(geocode,year,indicator,value)



# Calculate change
df_2009 <- PPI.df %>% filter(year == 2009) %>% select(-year) %>% rename(value_2009 = value)
df_2022 <- PPI.df %>% filter(year == 2022) %>% select(-year) %>% rename(value_2023 = value)
PPI.df <- left_join(df_2009, df_2022, by = c("geocode","indicator")) %>%
  mutate(ppichange = value_2022 - value_2009) %>%
  select(-value_2009)

PPI_2022.df <- PPI.df %>% select(geocode,indicator, value_2022)