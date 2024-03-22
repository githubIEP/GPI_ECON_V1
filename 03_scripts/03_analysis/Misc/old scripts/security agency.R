# security agencies
setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')

secu.agnecy <- read.csv("data/security agency costs 2018gpi.csv", stringsAsFactors = FALSE) %>%
  mutate(X2007=X2008, X2019=X2018,X2020=X2018)%>%
  mutate(iso3c=countrycode(country,"country.name","iso3c")) %>%
  gather(year, secu.agnecy, -c(country, iso3c))%>% 
  mutate(year=as.numeric(as.character(gsub("X","",year))))

secu.agnecy$iso3c[secu.agnecy$country=="Kosovo"] <- "KSV"

secu.agnecy <- secu.agnecy[secu.agnecy$iso3c %in% pos, ]    


secu.agnecy <- subset(secu.agnecy,select=c(iso3c,year,secu.agnecy))

secu.agnecy <- gpi.grid %>% left_join(secu.agnecy)
secu.agnecy <- subset(secu.agnecy,!(iso3c=="PSE" & year<2014))
secu.agnecy <- subset(secu.agnecy,!(iso3c=="SSD" & year<2009))
                                
                                  


# secu.agnecy <- secu.agnecy %>%  mutate(country=countrycode(iso3c,"iso3c","country.name")) %>% na.omit()
#                                        
# secu.agnecy$iso3c[secu.agnecy$country=="Kosovo"] <- "KSV"
# secu.agnecy$variablename <- "secu.agnecy" 




