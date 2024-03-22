


#pb <- read.csv("Data/peacebuilding 2022.csv") %>% 
pb <- read.csv("Data/2023_data/peacebuilding 2022.csv") %>%   
  mutate(iso3c=countrycode(Recipient,"country.name","iso3c")) %>%  mutate(iso3c=ifelse(Recipient=="Kosovo","KSV",iso3c))

pb <- pb[pb$iso3c %in% pos,]


# Check that that coutnrys are the ones we actually want
country.list <- as.data.frame(table(pb$Recipient))

rm(country.list)




# pb.countries <- read_excel("Data/pb countries.xlsx")

#pb.countries <- read_excel("Data/Peacebuilding Fund - Projects by Country.xlsx")
pb.countries <- read_excel("Data/2023_data/Peacebuilding Fund - Projects by Country.xlsx")

pb.countries <- pb.countries %>%  mutate(iso3c=countrycode(Countries,"country.name","iso3c")) %>% 
  mutate(iso3c=ifelse(Countries=="Kosovo","KSV",iso3c)) %>%
  distinct(iso3c) %>%
  mutate(country=countrycode(iso3c,"iso3c", "country.name")) %>% mutate(country=ifelse(iso3c=="KSV","Kosovo",country))

pb <- left_join(pb.countries,pb, by="iso3c" )



pb <- pb %>% subset(Donor=="Official Donors, Total") %>% 
  subset(Channel=="All Channels") %>% 
  subset(Flow=="Official Development Assistance") %>% 
  subset(Type.of.aid=="All Types, Total") %>% 
  subset(Value>0) %>% 
  subset(Amount.type=="Current Prices") %>% 
  subset(SECTOR=c("15210","15240","15250","15261","15230","15220","15130","15152","15113","15150","15153","15160","15170","15110","15111","15112","15151")) %>% 
  subset(Flow.type=="Gross Disbursements") 


pb <- pb %>% mutate(Year = Year+2) %>% group_by(Year, iso3c, Recipient) %>% 
  summarise(value=sum(Value, na.rm = TRUE)) %>% mutate(variablename = "peacebuilding") %>%
  rename(country=Recipient) %>%
  rename(year = "Year") 


pb <- subset(pb,!(iso3c=="PSE" & year<2015))
pb <- subset(pb,!(iso3c=="SSD" & year<2010))


pb$value <- pb$value*10^6

peacebuilding <- pb %>% select(iso3c,year,value) %>% rename(peacebuilding=value) %>% 
  subset(year > 2006)
#  subset(year > 2007)



rm(pb.countries)


peacebuilding <- gpi.grid %>% left_join(peacebuilding) 


peacebuilding <- peacebuilding %>% mutate (peacebuilding = case_when(is.na(peacebuilding) ~ 0,
                                                       TRUE ~ peacebuilding ))


