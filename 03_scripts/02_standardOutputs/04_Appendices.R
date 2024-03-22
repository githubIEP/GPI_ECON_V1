##### ----- GTI STANDARD CHARTS AND TABLES: APPENDICES
#' This script is for producing the standard appendix tables

### --- Appendix Information

# Appendix A: GTI Ranks and Scores
TABLE_AppendixA = c(title = paste0("GTI Ranks and Scores, ", GTI_YEAR),
                   sheet = "AppendixA", source = SOURCE_TT, xtext = "", ytext = "",
                   type = "Table", position = "Special")

# Appendix B: 50 Deadliest Terrorist Attacks
TABLE_AppendixB = c(title = paste0("50 Deadliest Terrorist Attacks, ", GTI_YEAR),
sheet = "AppendixB", source = SOURCE_TT, xtext = "", ytext = "",
type = "Table", position = "Special")

### --- Data Tidying

## -- Appendix A ---------------------------------------------------------------

# Tidy Data
TABLE_AppendixA.df <- rio::import("02_data/processed/GTI_BandedNational.rds") %>%
  select(rank, country, year, score = banded_score) %>%
  filter(year %in% c(GTI_YEAR, GTI_YEAR - 1)) %>%
  pivot_wider(names_from = year, values_from = c(rank, score)) %>%
  mutate(`score change` = !!sym(paste0("score_",GTI_YEAR)) - !!sym(paste0("score_",GTI_YEAR - 1))) %>%
  select(`GTI Rank` = !!sym(paste0("rank_",GTI_YEAR)),
         Country = country,
         !!paste0(GTI_YEAR," GTI score (out of 10)") := !!sym(paste0("score_",GTI_YEAR)),
         !!paste0("Change in score (",GTI_YEAR - 1,"-",GTI_YEAR,")") := `score change`)%>%
  arrange(`GTI Rank`)

# -- Appendix B ---------------------------------------------------------------
  
  # Tidy Data
  TABLE_AppendixB.df = rio::import("02_data/processed/clean_TT.rds")%>% 
  filter(year == GTI_YEAR, deaths_total >= 0) %>% 
  select(event_id, country, date, `State/Province` = admin_Name, Organisation = perpetrator_name, Fatalities = deaths_total, `Attack Target` = event_type ) %>%
  distinct()%>%
  select(-event_id) %>%
  arrange(desc(Fatalities)) %>% 
  slice(1:50) %>%
  mutate(rank = row_number(), .before = 1)