item.cost <- econcost %>% subset(subtype=="impact") %>% group_by(indicator, year) %>% 
  summarise(total=sum(value)) %>% filter (year %in% c(2020, 2022))


item.cost <- item.cost %>% pivot_wider (names_from = year, values_from = total)

item.cost <- item.cost %>% rename (year2020 = c(2), year2022 = c(3)) %>% 
                            mutate (change = year2022 - year2020) %>%
                            mutate (percent = (year2022 - year2020)/ year2020)


# Comparing Milex with rest


item.cost <- econcost %>% subset(subtype=="impact") %>% group_by(indicator, year) %>% 
                          summarise(total=sum(value)) %>% ungroup()

miliexp <- item.cost %>% filter (indicator == "milex") %>% rename (milex = total) %>%
                               select (year, milex)

misc.cost <- item.cost %>% filter (!indicator == "milex") %>% group_by(year) %>% 
                            summarise(misc.total=sum(total))


costs <- miliexp %>% left_join(misc.cost, by = "year")


costs %>% ggplot(aes(x = year)) +
  geom_line(aes(y = milex/10^12, color = "Milex"), size = .75) +
  geom_line(aes(y = misc.total/10^12 , color = "Misc"), size = .75) +
  labs(x = "Year", y = "Total Cost (Constant 2022 US$ PPP, trillions)", color = "") +
  # scale_y_continuous (breaks = c(2000000000000, 4000000000000, 6000000000000,
  #                                8000000000000, 10000000000000, 12000000000000),
  #                     labels = c("2", "4", "6", "8", "10", "12")) +
  scale_x_continuous (breaks = c(2008:2022)) + 
  scale_color_manual(values = c("Milex" = "blue", "Misc" = "red"),
                     labels = c("Military Costs", "Total Costs excl. Military")) +
  theme_minimal()

