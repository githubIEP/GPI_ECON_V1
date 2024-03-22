########### UNHCR funding #########


########### Prior to 2018 #########

# Copy old data from last years folder

#unhcr<- read_csv("Data/Refugees and IDP costs unhcr 2020.csv") %>%
  unhcr<- read_csv("Data/2023_data/Refugees and IDP costs unhcr 2020.csv") %>%
  gather(year,unhcr,-iso3c) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year))))
  

unhcr<- unhcr %>% mutate(unhcr=as.numeric(as.character(unhcr)))


########### Update from PDFS #########
# For the 2021 GPI I have the following

# UNHCR Final 2020 Global Funding Overview as of 31 December 2020
# UNHCR Final 2019 Global Funding Overview as of 31 December 2019
# UNHCR Final 2018 Global Funding Overview as of 31 December 2018

## They are located here
# S:\Institute for Economics and Peace\Global Peace Index\2021 GPI\Economic Impact of Violence\UNHCR funding


# Do this for the new data
#If the numbers differ from the previous year (they have revised the figures then i update it all)

############ ADD IN 2021 ############

#UNHCR_2021 <- read_excel("Data/Global Funding Overview 31 December 2021-converted.xlsx", 
UNHCR_2021 <- read_excel("Data/2023_data/Global Funding Overview 31 December 2021-converted.xlsx", 
                         sheet = "data", col_types = c("text", 
                                                       "numeric"))

UNHCR_2021$iso3c = countrycode::countrycode(UNHCR_2021$country, "country.name", "iso3c")
UNHCR_2021$iso3c[UNHCR_2021$country=="Kosovo"] <- "KSV"



UNHCR_2021 <- na.omit(UNHCR_2021)
UNHCR_2021 <- UNHCR_2021[UNHCR_2021$iso3c %in% pos,]
UNHCR_2021 <- UNHCR_2021 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2021$year = 2021
UNHCR_2021 <- rename(UNHCR_2021, unhcr=total)
UNHCR_2021 <- UNHCR_2021 %>% select(iso3c,year,unhcr)

############ ADD IN 2022 ############

# UNHCR_2022 <- read_excel("Data/2023_data/Global Funding Overview 31 December 2022-converted.xlsx", 
#                          sheet = "data", col_types = c("text", 
#                                                        "numeric"))
# 
# UNHCR_2022$iso3c = countrycode::countrycode(UNHCR_2021$country, "country.name", "iso3c")
# UNHCR_2022$iso3c[UNHCR_2022$country=="Kosovo"] <- "KSV"
# 
# 
# 
# UNHCR_2022 <- na.omit(UNHCR_2022)
# UNHCR_2022 <- UNHCR_2022[UNHCR_2022$iso3c %in% pos,]
# UNHCR_2022 <- UNHCR_2022 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
# UNHCR_2022$year = 2022
# UNHCR_2022 <- rename(UNHCR_2022, unhcr=total)
# UNHCR_2022 <- UNHCR_2022 %>% select(iso3c,year,unhcr)




unhcr <- unhcr %>% rbind(UNHCR_2021) 
# unhcr <- unhcr %>% rbind(UNHCR_2022) 

unhcr <- unhcr[unhcr$iso3c %in% pos,]
unhcr <- unhcr %>% na.omit() #%>% mutate(year=year-1)
unhcr <- gpi.grid %>% left_join(unhcr) 


unhcr <- unhcr %>% rename (geocode = iso3c, value = unhcr) %>% mutate (variablename = "unhcr")

unhcr <-  index_data_pad(unhcr)
  
unhcr <- unhcr %>% select (c(1, 2, 5)) %>% rename (iso3c = geocode, unhcr = imputed)

unhcr <- gpi.grid %>% left_join(unhcr)


#rm(UNHCR_2018,UNHCR_2020,UNHCR_2019)


# finished