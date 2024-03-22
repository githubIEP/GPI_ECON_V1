f_DBScatter <- function(id1,id2) {
  df1 <- iepg_get(id1) 
  df2 <- iepg_get(id2)
  
  year1 <- max(df1$year, na.rm = TRUE)
  year2 <- max(df2$year, na.rm = TRUE)
  
  df1 <- df1 %>% filter(year == year1 & disaggregation == "raw") %>%
    select(geocode,variablename,value)
  df2 <- df2 %>% filter(year == year2 & disaggregation == "raw") %>%
    select(geocode,variablename,value)
  
  df <- bind_rows(df1,df2) %>%
    pivot_wider(names_from = variablename, values_from = value)
  
  # how to extract the name of the column?
  p <- ggplot(df, aes(x = ??, y = ??)) +
  #   geom_point(aes(color = geocode), size = 3) +
  #   geom_smooth(method = "lm", se = FALSE) +
  #   labs(x = paste("ariable", variable_id_1, "(", latest_year_1, ")"),
  #        y = paste("Variable", variable_id_2, "(", latest_year_2, ")"),
  #        title = paste("Scatterplot of Variable", variable_id_1, "vs Variable", variable_id_2)) +
  #   theme_minimal()
  
  # Print the scatterplot
  #print(p)
  
}