########### UNHCR funding #########


########### Prior to 2018 #########
setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')

# Copy old data from last years folder

unhcr<- read_csv("Data/Refugees and IDP costs unhcr 2018.csv") %>%
  gather(year,unhcr,-country) %>%
  mutate(year=as.numeric(as.character(gsub("X","",year)))) %>% rename(country=country)
  

unhcr<- unhcr %>% mutate(iso3c=countrycode(country,"country.name","iso3c")) %>% 
  subset(select=-country) %>% 
  mutate(unhcr=as.numeric(as.character(unhcr)))


########### Update from PDFS #########
# For the 2021 GPI I have the following

# UNHCR Final 2020 Global Funding Overview as of 31 December 2020
# UNHCR Final 2019 Global Funding Overview as of 31 December 2019
# UNHCR Final 2018 Global Funding Overview as of 31 December 2018

## They are located here
# S:\Institute for Economics and Peace\Global Peace Index\2021 GPI\Economic Impact of Violence\UNHCR funding
setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')


# Do this for the new data
#If the numbers differ from the previous year (they have revised the figures then i update it all)


############ ADD in 2020 ############


# You will be required to delete out many of the entries
# I have done this in excel as it was quicker

#Key words deleted 
# private   UNHCR
# you will need to manually check which ones need to go

#1. load in data 
#2. run iso3c formula to see what you can quickly eliminate


UNHCR_2020 <- read_excel("UNHCR funding/Global Funding Overview 31 December 2020-converted.xlsx", 
                         sheet = "data", col_types = c("text", 
                                                       "numeric"))

UNHCR_2020$iso3c = countrycode::countrycode(UNHCR_2020$country, "country.name", "iso3c")
UNHCR_2020$iso3c[UNHCR_2020$country=="Kosovo"] <- "KSV"



UNHCR_2020 <- na.omit(UNHCR_2020)
UNHCR_2020 <- UNHCR_2020[UNHCR_2020$iso3c %in% pos,]
UNHCR_2020 <- UNHCR_2020 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2020$year = 2021
UNHCR_2020 <- rename(UNHCR_2020, unhcr=total)
UNHCR_2020 <- UNHCR_2020 %>% select(iso3c,year,unhcr)



############ ADD in 2019 ############



UNHCR_2019 <- read_excel("UNHCR funding/Global Funding Overview 31 December 2019-converted.xlsx", 
                         sheet = "data", col_types = c("text", 
                                                       "numeric"))
UNHCR_2019$iso3c = countrycode::countrycode(UNHCR_2019$country, "country.name", "iso3c")


UNHCR_2019$iso3c[UNHCR_2019$country=="Kosovo"] <- "KSV"



UNHCR_2019 <- na.omit(UNHCR_2019)


UNHCR_2019 <- UNHCR_2019[UNHCR_2019$iso3c %in% pos,]
UNHCR_2019 <- UNHCR_2019 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2019$year = 2020
UNHCR_2019 <- rename(UNHCR_2019, unhcr=total)

UNHCR_2019 <- UNHCR_2019 %>% select(iso3c,year,unhcr)

############ ADD in 2019 ############



UNHCR_2018 <- read_excel("UNHCR funding/Global Funding Overview 31 December 2018-converted.xlsx", 
                         sheet = "data", col_types = c("text", 
                                                       "numeric"))
UNHCR_2018$iso3c = countrycode::countrycode(UNHCR_2018$country, "country.name", "iso3c")


UNHCR_2018$iso3c[UNHCR_2018$country=="Kosovo"] <- "KSV"



UNHCR_2018 <- na.omit(UNHCR_2018)


UNHCR_2018 <- UNHCR_2018[UNHCR_2018$iso3c %in% pos,]
UNHCR_2018 <- UNHCR_2018 %>% mutate(country=countrycode(iso3c,"iso3c","country.name")) 
UNHCR_2018$year = 2019
UNHCR_2018 <- rename(UNHCR_2018, unhcr=total)


UNHCR_2018 <- UNHCR_2018 %>% select(iso3c,year,unhcr)



setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')

unhcr <- unhcr %>% rbind(UNHCR_2018) %>% rbind(UNHCR_2020)%>% rbind(UNHCR_2019)

unhcr <- unhcr[unhcr$iso3c %in% pos,]
unhcr <- unhcr %>% na.omit() %>% mutate(year=year-1)
unhcr <- gpi.grid %>% left_join(unhcr) %>% mutate(unhcr=ifelse(is.na(unhcr),0,unhcr)) %>% distinct()



rm(UNHCR_2018,UNHCR_2020,UNHCR_2019)


# finished


tmp <- unhcr %>% spread(year, unhcr) %>% dplyr::select(-`2021`)
write.csv(tmp, "Data/Refugees and IDP costs unhcr 2020.csv")
