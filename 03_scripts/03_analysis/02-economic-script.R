
# Getting GDP per cap PPP Constant, GDP current, GDP Constant, GDP PPP Constant, GDP Deflator from WDI

wdi.all <- get.wdi("all",c("NY.GDP.PCAP.PP.KD", "PA.NUS.PPPC.RF", "NY.GDP.DEFL.ZS",
                           "NY.GDP.PCAP.CD", "NY.GDP.PCAP.KD"),
                   2007,2022) %>% mutate(year = year+1)

wdi.data <- wdi.all %>% 
  rename('GDP per cap PPP Constant 2017' = NY.GDP.PCAP.PP.KD,  
         'PPP Conversion Factor' = PA.NUS.PPPC.RF, 'GDP Deflator' = NY.GDP.DEFL.ZS, 
         'GDP per cap Current' = NY.GDP.PCAP.CD,'GDP per cap Constant' = NY.GDP.PCAP.KD)




wdi.data <- gpi.grid %>% left_join(wdi.data, by = c("iso3c", "year"))

wdi.data$country[wdi.data$iso3c == "KSV"] <- "Kosovo"
wdi.data$country[wdi.data$iso3c =="TWN"] <- "Taiwan"

wdi.data <- wdi.data %>% group_by (iso3c, country, year) %>% pivot_longer(cols = c(4:8)) %>%
                         rename (variablename = name, geocode = iso3c) %>% ungroup()
                                                                            


# Imputation

wdi.data.impute <- wdi.data %>% select (geocode, year, variablename, value) %>%
                               filter(complete.cases(value))

wdi.grid <- wdi.data %>% select (geocode, year, variablename)

wdi.data.impute <- wdi.data.impute %>%
  index_data_pad() %>% 
  right_join(wdi.grid, by = c("geocode", "year", "variablename")) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    variablename,
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    variablename, 
    geocode) %>%
  # # mutate(imputed2 = imputeTS::na_interpolation(imputed)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))


wdi.data <- wdi.data.impute
  
rm(wdi.data.impute, wdi.grid)

# =======================================================================================================================================

## GDP Values

gdp.wdi <- wdi.data %>% filter (variablename %in% c("GDP per cap Current", "GDP per cap Constant", "GDP per cap PPP Constant 2017"))

gdp.wdi <- gdp.wdi %>% rename (iso3c = geocode) %>% left_join (pop, by = c("iso3c", "year"))

gdp.wdi <- gdp.wdi %>% mutate (value = value * population) %>% select (-population)

gdp.wdi <- gdp.wdi %>% group_by (iso3c, year ) %>% pivot_wider(names_from = variablename) %>%
                       rename (gdp = 'GDP per cap Current', gdpcons = 'GDP per cap Constant', gdpconsppp = 'GDP per cap PPP Constant 2017') %>%
                       select (iso3c, year, gdp, gdpcons, gdpconsppp)

gdp.wdi <- gdp.wdi %>% mutate (gdpcons = 1 / .87 * gdpcons) # turning WDI constant 2015 to constant 2022




# =====================================================================================================================================

## GDP per cap Constant

gdp.pc.constant <- wdi.data %>% filter (variablename == "GDP per cap Constant")

gdp.pc.constant <- gdp.pc.constant %>% select (geocode, year, value) %>% rename (iso3c = geocode, gdp.pc.cons = value)

gdp.pc.constant <- gdp.pc.constant %>% mutate (gdp.pc.cons = 1 / .87 * gdp.pc.cons) # turning constant 2015 to constant 2022

# ==================================================================================================================================


## GDP Deflator

deflator <- wdi.data %>% filter (variablename == "GDP Deflator")

deflator <- deflator %>% select (geocode, year, value) %>% rename (iso3c = geocode)


deflator_2022 <- deflator %>% filter (year == 2022)

deflator <- deflator %>% left_join(deflator_2022, by = "iso3c")

deflator <- deflator %>% select (iso3c, year.x, value.x, value.y) %>%
  mutate (value.x = value.x / value.y) %>%
  rename (value = value.x, year = year.x) %>%
  select (iso3c, year, value)

deflator <- deflator %>% rename (deflator = value)

deflator <- deflator %>% group_by(year) %>% summarize (deflator = median(deflator)) # using median deflator for all countries

rm(deflator_2022)

# ==============================================================================================================================

## PPP Conversion Factor
ppp.conv <- wdi.data %>% filter (variablename == "PPP Conversion Factor")

ppp.conv  <- ppp.conv  %>% select (geocode, year, value) %>% rename (iso3c = geocode)

# ========================================================================================================================

## PPP US Ratio

wdi.gdpc.ppp <- wdi.data %>% filter (variablename == "GDP per cap PPP Constant 2017")

wdi.gdpc.ppp  <- wdi.gdpc.ppp  %>% select (geocode, year, value) %>% rename (iso3c = geocode)



wdi.gdpc.ppp <- wdi.gdpc.ppp %>% rename(gdpc.ppp.con = value)

ppp_us <- wdi.gdpc.ppp %>% filter (iso3c == "USA")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% left_join(ppp_us, by="year")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% mutate (scale = gdpc.ppp.con.x/gdpc.ppp.con.y)

ppp <- wdi.gdpc.ppp %>% select (c(1, 2, 6)) %>% rename(iso3c = iso3c.x)

rm(wdi.gdpc.ppp, ppp_us, wdi.data, wdi.all)

# =========================================================================================================

