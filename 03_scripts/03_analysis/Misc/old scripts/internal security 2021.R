# Internal security  ------------------------------------------------------


setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence')
pos.exp <- read_csv("Internal Security Expenditure/IMF data.csv")



pos.exp <- pos.exp %>% filter(`COFOG Function Code`=="GF03") %>% filter(`Attribute`=="Value") %>% filter(`Unit Name`=="Percent of GDP") %>% 
  filter(`Sector Name`=="General government") %>% 
  select(-"Country Code",-"COFOG Function Code",-"Sector Code",-"Unit Code",-"Indicator Code",-"Global DSD Time Series Code",-"X61") %>% 
  rename(variable.name="COFOG Function Name", country="Country Name", unit="Unit Name")


pos.exp <- pos.exp %>% gather(year, value, -c(country, variable.name, unit)) %>% mutate(value=as.numeric(as.character(value))) %>% 
  mutate(year=as.numeric(as.character(year))) %>%  mutate(year=year+1) 



pos.exp <- pos.exp%>%  mutate(iso3c=countrycode(country,"country.name","iso3c"))
pos.exp$iso3c[pos.exp$country=="Kosovo, Republic of"] <- "KSV"
pos.exp$iso3c[pos.exp$country=="Eswatini, Kingdom of"] <- "SWZ"
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]


pos.exp <- pos.exp %>% select(iso3c, year, value, country) %>%
  mutate(variablename="public order and safety") %>%
  subset(!(year<2005))


gpi.grid.tmp <- gpi.grid %>% mutate(count=1) %>% spread(year,count) %>% mutate(`2005`=`2008`,`2004`=`2008`,`2006`=`2008`) %>% 
  gather(year, count, -iso3c) %>% select(year,iso3c) %>% mutate(year=as.numeric(as.character(year)))

gpi.grid.tmp <- subset(gpi.grid.tmp,!(iso3c=="PSE" & year<2014))
gpi.grid.tmp <- subset(gpi.grid.tmp,!(iso3c=="SSD" & year<2009))

#work out who is missing data
pos.exp <- gpi.grid.tmp %>% left_join(pos.exp)



missing2 <- function(df){
  left_join(gpi.grid.tmp,df)
  df <- df[rowSums(is.na(df)) > 0,]
  return(df)}
### MISSING IS USING WRONG GRID
tmp <- missing2(pos.exp)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))



# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==7) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==12) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==17) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

pos.exp <- pos.exp %>% left_join(count_of_missing)



#delete out all that dont have at least one variable!  
pos.exp <- pos.exp %>% subset(!new=="delete")



# We can now impute given what remains has at least one value
pos.exp <- pos.exp %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="public order and safety")

pos.exp <- index_data_pad(pos.exp)

pos.exp <- pos.exp %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)

pos.exp <- pos.exp[pos.exp$year>2005,]
pos.exp <- pos.exp[pos.exp$iso3c %in% pos,]
pos.exp <- subset(pos.exp,!(iso3c=="PSE" & year<2014))
pos.exp <- subset(pos.exp,!(iso3c=="SSD" & year<2009))
pos.exp <- subset(pos.exp,!(year<2006))

setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')





#use gdp current not the constant calc, you'll apply constant later               mohib
pos.exp <- pos.exp %>% 
  merge(gdp.wdi[,c("iso3c","year","gdp")],by=c("iso3c","year")) %>% 
  mutate(value=value/100) %>% mutate(intsecu=value*gdp) %>% distinct()

pos.exp <- subset(pos.exp, select = c(iso3c,year,intsecu))

pos.exp1 <- gpi.grid %>% left_join(pos.exp)
pos.exp1 <- pos.exp1 %>% subset(is.na(intsecu))

pos.intsec <- unique(pos.exp1$iso3c)







#########################################################   FOR MISSING Data    ################################################
#fill in the police rate
gpidata <- read.csv("S:/Institute for Economics and Peace/Global Peace Index/Calculation Files/dashboard_data.csv") %>%
  rename_all(tolower) %>% # Note this may need to get deleted out
  subset(type=="Raw") %>% 
  rename(iso3c=code) %>%  
  mutate(year=year-1) %>%
  subset(select=-type) %>% select(-region) %>%select(-govt) %>% 
  mutate(value=as.numeric(as.character(value))) %>%
  subset(!year==2021)


# GPI countries 
police <- subset(gpidata, indicator=="Police Rate" ) 
police$value <- as.numeric(as.character(police$value))
# police$year <- police$year-2
police <- police %>%  select(country, iso3c, year, value)

tmp <- pop %>% select(year,iso3c,population)

police <- police %>% full_join(tmp, by=c("iso3c", "year")) %>%
  rename(pop=population) %>% 
  mutate(pop100k=pop/10^5) %>% mutate(value=value*pop100k) %>% 
  merge(unitcost.scaled[,c("iso3c", "year", "police.direct")], by=c("iso3c", "year"))%>%
  mutate(police.cost=value*police.direct)%>% select(iso3c, country, year, police.cost)


police <- police[police$iso3c %in%pos.intsec, ]

police <- subset(police, select = c(iso3c,year,police.cost))

police <- rename(police,intsecu =police.cost)

pos.exp1 <- pos.exp1  %>%  select(year, iso3c)%>% left_join(police) %>%  select(year, iso3c, intsecu)


pos.exp <- pos.exp %>% rbind(pos.exp1)

pos.exp <- gpi.grid %>% left_join(pos.exp)

pos.exp <- subset(pos.exp,!(iso3c=="PSE" & year<2014))
pos.exp <- subset(pos.exp,!(iso3c=="SSD" & year<2009))


pos.exp3 <- rename(pos.exp,value=intsecu)

pos.exp3 <- distinct(pos.exp3)

count <- pos.exp3 %>% group_by(iso3c) %>% tally()
