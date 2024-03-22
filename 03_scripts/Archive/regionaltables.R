### --- Standard Regional Tables ----------------------------------------------



region.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>% 
  dplyr::filter(year == GTI_YEAR | year == GTI_YEAR - 1 | year == GTI_YEAR - 10) %>%
  select(country, year, region, banded_score, rank) %>%
  pivot_wider(names_from = year, values_from = c(banded_score, rank)) %>%
  mutate(change_10 = get(paste0("banded_score_", GTI_YEAR)) - get(paste0("banded_score_", GTI_YEAR - 10))) %>%
  mutate(change_1 = get(paste0("banded_score_", GTI_YEAR)) - get(paste0("banded_score_", GTI_YEAR - 1))) %>%
  select(country, region, !!sym(paste0("banded_score_", GTI_YEAR)), !!sym(paste0("rank_", GTI_YEAR)), change_1, change_10) %>%
  rename(Country = country, 
         `Overall Score` = !!sym(paste0("banded_score_", GTI_YEAR)), 
         `Overall Rank` = !!sym(paste0("rank_", GTI_YEAR)))
         

REGIONS = unique(region.df$region)

for(area in REGIONS) {
  df = region.df %>% filter(region == area) %>%
    select(-region)
  
  # Calculate the averages for Change10 and Change1
  avg_change10 <- mean(df$change_10, na.rm = TRUE)
  avg_change1 <- mean(df$change_1, na.rm = TRUE)
  
  # Create a new row with the desired values
  new_row <- data.frame(Country = "Regional Average", 
                        `Overall Score` = NA, 
                        `Overall Rank` = NA, 
                        change_10 = avg_change10, 
                        change_1 = avg_change1,
                        check.names = FALSE)
  
  # Add the new row to the dataframe
  df <- rbind(df, new_row) %>%
    select(Country,`Overall Score`,`Overall Rank`, change_10, change_1) %>%
    rename(!!sym(paste0("Change ", GTI_YEAR - 10, "-", GTI_YEAR)) := change_10,
           !!sym(paste0("Change ", GTI_YEAR - 1, "-", GTI_YEAR)) := change_1)
  
  write.csv(df, paste0("04_outputs/tables/", area, "_trends.csv"),row.names = FALSE, na = "")
}

