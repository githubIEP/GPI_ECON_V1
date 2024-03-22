##### ----- Correlates of Terrorism
#' This script is for producing the correlates of terrorism table and charts
##### -----

### --- Libraries
f_LibraryLoader(tidyverse,
                iepg,
                #rqog,
                tictoc,
                extrafont)


### --- Correlations / Scatterplots for GTI vs GPI/PPI -------------------------

CHART_GPIscatter = c(title = paste0("Impact of Terrorism vs Global Peace Index"),
                     sheet = "GPIscatter", source = SOURCE_TT, xtext = "GLOBAL TERRORISM INDEX", ytext = "GLOBAL PEACE INDEX")

CHART_PPIscatter = c(title = paste0("Impact of Terrorism vs Positive Peace Index"),
                     sheet = "PPIscatter", source = SOURCE_TT, xtext = "GLOBAL TERRORISM INDEX", ytext = "POSITIVE PEACE INDEX")

## -- Tidy Data

# List of countries
countries.df = rio::import(IEP_NAMES) %>% 
  mutate(OECD = if_else(geocode %in% THE_OECD, "OECD", "Rest of the World"),
         SAHEL = if_else(geocode %in% THE_SAHEL, "SAHEL", "Rest of the World"))

# GTI Scores
corr_gti.df = readRDS("./02_data/processed/GTI_BandedNational.rds") %>%
  mutate(variablename = "GTI", value = banded_score) %>%
  select(geocode, variablename, year, value)


# GPI Indicators
corr_gpi.df = iepg_search("GPI 2023 Report") %>%
  filter(disaggregation == "banded") %>% pull(muid)

corr_gpi.df = iepg_get(uid = (corr_gpi.df)) %>%  ungroup() %>%
  select(geocode, variablename, year, value)
corr_gpi.list = corr_gpi.df %>% distinct(variablename) %>% pull(variablename)

# PPI Indicators
corr_ppi.df = iepg_search("PPI 2023 Report") %>%
  filter(disaggregation == "banded") %>% pull(muid)

corr_ppi.df = iepg_get(uid = (corr_ppi.df)) %>% ungroup() %>%
  select(geocode, variablename, year, value)
corr_ppi.list = corr_ppi.df %>% distinct(variablename) %>% pull(variablename)

# Combine Data

correlates.df <- rbind(corr_gti.df, corr_ppi.df, corr_gpi.df) %>%
  left_join(countries.df)

## -- Correlation Tables

# Calculate correlations for OECD countries
OECD_correlations.df <- correlates.df %>%
  filter(OECD == "OECD") %>%
  f_CalculateCorrelations()

# Calculate correlations for Rest of the World
ROW_correlations.df <- correlates.df %>%
  filter(OECD == "Rest of the World") %>%
  f_CalculateCorrelations()

# Correlation for Both

all_correlations.df <- correlates.df %>%
  f_CalculateCorrelations()

# Combine the results
correlation_table.df <- full_join(OECD_correlations.df, ROW_correlations.df, by = "variablename") %>%
rename(OECD = Correlation.x, `Rest of the World` = Correlation.y)

correlation_table.df <- full_join(correlation_table.df, all_correlations.df, by = "variablename") %>%
  rename(All = Correlation, Indicator = variablename) %>%
  mutate(Index = case_when(
    Indicator %in% GPI ~ "GPI",
    Indicator %in% PPI ~ "PPI"),
    OECD = round(OECD, 2),
           `Rest of the World` = round(`Rest of the World`, 2),
    All = round(All, 2)) %>% 
  filter(Indicator != "terrorism impact", Indicator != "GTI") %>%
  arrange(desc(OECD)) %>% relocate(Index)

# GPI Scatterplot
GPIscatter.df = correlates.df %>%
  filter(variablename %in% c("GTI", "overall score")) %>%
  pivot_wider(names_from = variablename, values_from = value) %>%
  select(OECD, GTI, `overall score`) %>%
  na.omit()

# Base Plot
p <- ggplot(GPIscatter.df, aes(x = GTI, y = `overall score`, color = OECD)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
  labs(x = "GTI Score", y = "GPI Overall Score", color = "") +
  geom_smooth(method = "lm", se = FALSE, aes(group = OECD)) 
pCHART_GPIscatter <- f_ThemeGTI(p,
                                CHART_GPIscatter,
                                plottitle = "", 
                                xaxis = "Include", 
                                yaxis = "Include", 
                                xgridline = "Include", 
                                ygridline = "Include") +
                                theme(legend.position = c(.9,.1))


pCHART_GPIscatter <- f_ScatterLabels(pCHART_GPIscatter, 
                                     xaxis = "Include",
                                     yaxis = "Include",
                                     left_text = "Lower Impact",
                                     right_text = "Higher Impact",
                                     up_text = "Less Peaceful",
                                     down_text = "More Peaceful")
                                     
                                    
# PPI Scatterplot
PPIscatter.df = correlates.df %>%
  filter(variablename %in% c("GTI", "PPI Overall Score")) %>%
  pivot_wider(names_from = variablename, values_from = value) %>%
  select(OECD, GTI, `PPI Overall Score`) %>%
  na.omit()

# Base Plot
p <- ggplot(PPIscatter.df, aes(x = GTI, y = `PPI Overall Score`, color = OECD)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
  labs(x = "GTI Score", y = "GPI Overall Score", color = "") +
  geom_smooth(method = "lm", se = FALSE, aes(group = OECD)) 

# GTI Theme
pCHART_PPIscatter <- f_ThemeGTI(p,
                                CHART_PPIscatter,
                                plottitle = "", 
                                xaxis = "Include", 
                                yaxis = "Include", 
                                xgridline = "Include", 
                                ygridline = "Include") +
  theme(legend.position = c(.9,.1))


pCHART_PPIscatter <- f_ScatterLabels(pCHART_PPIscatter, 
                                     xaxis = "Include",
                                     yaxis = "Include",
                                     left_text = "Lower Impact",
                                     right_text = "Higher Impact",
                                     up_text = "Less Peaceful",
                                     down_text = "More Peaceful",
                                     yposition = 0.02)                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
tmp = ALL_SCORES %>%
  filter(variablename %in% c("GTI", "PPI Overall Score")) %>%
  pivot_wider(names_from = variablename, values_from = value) %>%
  select(OECD, GTI, `PPI Overall Score`) %>%
  na.omit()

p = ggplot(tmp, aes(x = GTI, y = `PPI Overall Score`, color = OECD)) +
  geom_point() +
  scale_color_manual(values = c("OECD" = "#2D4F5E", "Rest of the World" = "#770A1E")) +
  labs(x = "GTI Score", y = "PPI Overall Score", color = "") +
  geom_smooth(method = "lm", se = FALSE, aes(group = OECD)) +
  theme_minimal() +
  labs(caption = "Source: Dragonfly TerrorismTracker, IEP calculations")
