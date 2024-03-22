##### ----- Manually Checking Myanmar Data

myanmar_check.df <- rio::import("./02_data/raw/cleaned-condensed-tt-data.rds") %>% 
  dplyr::filter(year > 2020 & iep_country == "Myanmar")

write.csv(myanmar_check.df,"04_outputs/tables/myanmar_check.csv", row.names = FALSE)