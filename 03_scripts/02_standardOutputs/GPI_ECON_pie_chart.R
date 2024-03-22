impact <- econcost %>%
  filter(subtype == "impact")

imp <- impact %>%
  filter(year == max(year)) %>%
  group_by(indicator2) %>%
  summarise(value = sum(value)) %>%
  ungroup()

row <- imp


  
  row <- row %>%
    filter(indicator2 %in% c("Terrorism", 
                             # "Violent crime", 
                             "Peacebuilding",
                             "Refugees and IDPs",
                             "Small arms", 
                             "Incarceration", 
                             "GDP losse", 
                             "Fear",
                             "Peacekeeping"))
  


new_row <- data.frame(indicator2 = "Other", value = sum(row$value))

tmp <- imp

tmp <- rbind(imp, new_row)


tmp <- tmp[!tmp$indicator2 %in% c("Terrorism", 
                                 # "Violent crime", 
                                 "Peacebuilding",
                                 "Refugees and IDPs",
                                 "Small arms", 
                                 "Incarceration", 
                                 "GDP losses", 
                                 "Fear",
                                 "Peacekeeping"), ]




imp <- tmp

imp <- imp %>% 
  mutate(all = sum(value)) %>%
  mutate(pc = value/all) %>%
  mutate(pc = pc*100)



# 
# p <- ggplot(imp) +
#   geom_col(aes(x = 1, y = value, fill = indicator2), width = 0.7, color = "white") +  
#   coord_polar(theta = "y") +
#   geom_label(aes(x = 1.5, y = cumsum(value) - 0.5 * value,
#                  label = paste(indicator2, ": ", round(pc), "%")),
#              color = "black", size = 3, hjust = 0) +  
#   theme_void() +  
#   theme(legend.position = "none",
#         plot.background = element_rect(fill = "transparent", color = NA)) +  
#   annotate("text", x = 0, y = 0, label = "", size = 6, color = "black")
# 
# p
# 
# 

p <- ggplot(imp) +
  geom_col(aes(x = 1, y = value, fill = indicator2), width = 0.7, color = "white") +
  coord_polar(theta = "y") +
  geom_label(aes(x = 1.5, y = cumsum(value) - 0.5 * value,
                 label = paste(indicator2, ": ", round(pc), "%")),
             color = "black", size = 3, hjust = 1, vjust = 0) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA)) +
  annotate("text", x = 0, y = 0, label = "", size = 6, color = "black")

p



