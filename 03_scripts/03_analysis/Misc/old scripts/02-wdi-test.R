
# ESTIMATING GDP PC Constant PPP SCALE


## WDI PPP PC Constant Estimates ================================================================

wdi.gdpc.ppp <- get.wdi("all","NY.GDP.PCAP.PP.KD",2007,2022) %>% 
  rename(value=NY.GDP.PCAP.PP.KD) %>% mutate(year=year+1) 


wdi.gdpc.ppp <- gpi.grid %>% left_join(wdi.gdpc.ppp)

wdi.gdpc.ppp$country[wdi.gdpc.ppp$iso3c == "KSV"] <- "Kosovo"
wdi.gdpc.ppp$country[wdi.gdpc.ppp$iso3c =="TWN"] <- "Taiwan"



## Imputing NAs =========================================================================

wdi.gdpc.ppp<- wdi.gdpc.ppp %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP per cap PPP Constsnt 2017")

gdp.impute <-  wdi.gdpc.ppp %>% filter(complete.cases(value))


gdp.impute <- gdp.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

wdi.gdpc.ppp <- gdp.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

rm(gdp.impute)


## ESTMATING Scale ================================================================

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% rename(gdpc.ppp.con = value)

ppp_us <- wdi.gdpc.ppp %>% filter (iso3c == "USA")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% left_join(ppp_us, by="year")

wdi.gdpc.ppp <- wdi.gdpc.ppp %>% mutate (scale = gdpc.ppp.con.x/gdpc.ppp.con.y)

ppp <- wdi.gdpc.ppp %>% select (c(1, 2, 6)) %>% rename(iso3c = iso3c.x)

rm(wdi.gdpc.ppp, ppp_us)


##===============================================================================

# PPP Conversion Factor ====================================================

ppp.conv <- get.wdi("all","PA.NUS.PPPC.RF",2006,2022) %>%   
  rename(value=PA.NUS.PPPC.RF) %>% mutate(year=year+1)

ppp.conv <- gpi.grid %>% left_join(ppp.conv, by=c("year","iso3c"))

ppp.conv$country[ppp.conv$iso3c == "KSV"] <- "Kosovo"
ppp.conv$country[ppp.conv$iso3c =="TWN"] <- "Taiwan"



ppp.conv <- ppp.conv %>% rename(geocode = iso3c) %>% mutate (variablename = "PPP Conversion Factor")

ppp.conv.impute <-  ppp.conv %>% filter(complete.cases(value))


ppp.conv.impute <- ppp.conv.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

ppp.conv <- ppp.conv.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

rm(ppp.conv.impute)


## ==============================================================================

# USD Deflator =================================================================

deflator <- get.wdi("all","NY.GDP.DEFL.ZS",2006,2022) %>%   
  rename(value= NY.GDP.DEFL.ZS) %>% mutate(year=year+1)


deflator <- gpi.grid %>% left_join(deflator, by=c("year","iso3c"))

deflator$country[deflator$iso3c == "KSV"] <- "Kosovo"
deflator$country[deflator$iso3c =="TWN"] <- "Taiwan"

deflator <- deflator %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP Deflator")

deflator.impute <-  deflator %>% filter(complete.cases(value))


deflator.impute <- deflator.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

deflator <- deflator.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)


deflator_2022 <- deflator %>% filter (year == 2022)

deflator <- deflator %>% left_join(deflator_2022, by = "iso3c")

deflator <- deflator %>% select (iso3c, year.x, value.x, value.y) %>%
                        mutate (value.x = value.x / value.y) %>%
                        rename (value = value.x, year = year.x) %>%
                        select (iso3c, year, value)

deflator <- deflator %>% rename (deflator = value)

deflator <- deflator %>% group_by(year) %>% summarize (deflator = median(deflator))

rm(deflator.impute, deflator_2022)

## =============================================================================

# GDP Current ==================================================================

gdp.current <- get.wdi("all","NY.GDP.MKTP.CD",2007,2022) %>% 
  rename(value=NY.GDP.MKTP.CD) %>% mutate(year=year+1) 


gdp.current <- gpi.grid %>% left_join(gdp.current)

gdp.current$country[gdp.current$iso3c == "KSV"] <- "Kosovo"
gdp.current$country[gdp.current$iso3c =="TWN"] <- "Taiwan"



## Imputing NAs =========================================================================

gdp.current <- gdp.current %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP Current")

current.impute <-  gdp.current %>% filter(complete.cases(value))


current.impute <- current.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

gdp.current <- current.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

gdp.current <- gdp.current %>% rename (gdp = value)

rm(current.impute)

## ===============================================================================

# GDP Constant ==================================================================

gdp.constant <- get.wdi("all","NY.GDP.MKTP.KD",2007,2022) %>% 
  rename(value=NY.GDP.MKTP.KD) %>% mutate(year=year+1) 


gdp.constant <- gpi.grid %>% left_join(gdp.constant)

gdp.constant$country[gdp.constant$iso3c == "KSV"] <- "Kosovo"
gdp.constant$country[gdp.constant$iso3c =="TWN"] <- "Taiwan"



## Imputing NAs =========================================================================

gdp.constant <- gdp.constant %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP Constant")

constant.impute <-  gdp.constant %>% filter(complete.cases(value))


constant.impute <- constant.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

gdp.constant <- constant.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

gdp.constant <- gdp.constant %>% rename ( gdpcons = value)

gdp.constant <- gdp.constant %>% mutate (gdpcons = 1 / .87 * gdpcons) # turning WDI constant 2015 to constant 2022

rm(constant.impute)

## ==============================================================================

# GDP Constant PPP =============================================================
gdp.constant.ppp <- get.wdi("all","NY.GDP.MKTP.PP.KD",2007,2022) %>% 
  rename(value=NY.GDP.MKTP.PP.KD) %>% mutate(year=year+1) 


gdp.constant.ppp <- gpi.grid %>% left_join(gdp.constant.ppp)

gdp.constant.ppp$country[gdp.constant.ppp$iso3c == "KSV"] <- "Kosovo"
gdp.constant.ppp$country[gdp.constant.ppp$iso3c =="TWN"] <- "Taiwan"



## Imputing NAs =========================================================================

gdp.constant.ppp <- gdp.constant.ppp %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP Constant")

constant.ppp.impute <-  gdp.constant.ppp %>% filter(complete.cases(value))


constant.ppp.impute <- constant.ppp.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

gdp.constant.ppp <- constant.ppp.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

gdp.constant.ppp <- gdp.constant.ppp %>% rename ( gdpconsppp = value)



rm(constant.ppp.impute)

## =============================================================================

gdp.wdi <- gdp.current %>% left_join(gdp.constant) %>% left_join(gdp.constant.ppp)

rm(gdp.current, gdp.constant, gdp.constant.ppp)

## =============================================================================


# GDP per cap Constant  =============================================================
gdp.pc.constant <- get.wdi("all","NY.GDP.PCAP.KD",2007,2022) %>% 
  rename(value=NY.GDP.PCAP.KD) %>% mutate(year=year+1) 


gdp.pc.constant <- gpi.grid %>% left_join(gdp.pc.constant)

gdp.pc.constant$country[gdp.pc.constant$iso3c == "KSV"] <- "Kosovo"
gdp.pc.constant$country[gdp.pc.constant$iso3c =="TWN"] <- "Taiwan"


## Imputing NAs =========================================================================

gdp.pc.constant <- gdp.pc.constant %>% rename(geocode = iso3c) %>% mutate (variablename = "GDP Constant")

pc.constant.impute <-  gdp.pc.constant %>% filter(complete.cases(value))


pc.constant.impute <- pc.constant.impute %>%
  index_data_pad() %>%
  rename(iso3c = geocode) %>%
  right_join(gpi.grid) %>%
  rename(geocode = iso3c) %>%
  complete(geocode,  year) %>%
  mutate (value = imputed) %>%
  select (-imputed, imputation_type) %>%
  ungroup() %>%
  mutate(region = countrycode::countrycode(geocode, "iso3c", "region")) %>% # using regional averages for countries missing all country-years!
  mutate(region = ifelse(geocode == "KSV", "Europe & Central Asia", region)) %>%
  group_by(
    region, year) %>%
  mutate(reg_av = mean(value, na = T)) %>%
  mutate(imputed = ifelse(is.na(value), reg_av, value)) %>%
  mutate(imputed = ifelse(is.nan(imputed), NA, imputed)) %>%
  filter(!is.na(region)) %>%
  ungroup() %>% 
  group_by(
    geocode) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & !is.nan(reg_av), paste("Regional Average", (year[!is.nan(reg_av)])), imputation_type)) %>%
  mutate(imputation_type = ifelse(is.na(imputation_type) & is.nan(reg_av), paste("Regional Average", max(year[!is.nan(reg_av)])), imputation_type)) %>%
  ungroup() %>%
  mutate (value = imputed) %>%
  select (-c(5:8))

gdp.pc.constant <- pc.constant.impute %>% select (geocode, year, value) %>% rename(iso3c = geocode) %>%
  right_join (gpi.grid)

gdp.pc.constant <- gdp.pc.constant %>% rename ( gdp.pc.cons = value)

gdp.pc.constant <- gdp.pc.constant %>% mutate (gdp.pc.cons = 1 / .87 * gdp.pc.cons) # turning constant 2015 to constant 2022

rm(pc.constant.impute)
