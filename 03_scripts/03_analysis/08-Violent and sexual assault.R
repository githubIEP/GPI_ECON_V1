
#################################### Violent assault ###################################

#violent.assault <- read.csv("Data/assault_data.csv") %>% rename(country=Country, year=Year, value=Rate) %>% 
violent.assault <- read.csv("Data/2023_data/assault_data.csv") %>% rename(country=Country, year=Year, value=Rate) %>%
  select(country, year, value) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% subset(year>2006)  %>%
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)")

violent.assault = violent.assault %>% mutate(value = round(value,2))




violent.assault$iso3c <- countrycode::countrycode(violent.assault$country, "country.name","iso3c")
violent.assault$iso3c[violent.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
violent.assault$country[violent.assault$iso3c=="KSV"] <- "Kosovo"
violent.assault$variablename <- "violent assault"
violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]



### WORK OUT WHO IS MISSING



#work out who is missing data
violent.assault <- gpi.grid %>% left_join(violent.assault)
tmp <- missing(violent.assault)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))



# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==8) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==15) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

violent.assault <- violent.assault %>% left_join(count_of_missing)



#delete out all that dont have at least one variable!  
violent.assault <- violent.assault %>% subset(!new=="delete")



# We can now impute given what remains has at least one value
violent.assault <- violent.assault %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="violent assault")

violent.assault <- index_data_pad(violent.assault)

violent.assault <- violent.assault %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)




## Average by peace and region

violent.assault.average <- violent.assault %>%  left_join(Peace_and_region)
violent.assault.average <- violent.assault.average %>% group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
violent.assault.peace.level <- violent.assault %>%  left_join(Peace_and_region)
violent.assault.peace.level <- violent.assault.peace.level %>% select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))



##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

violent.assault <- gpi.grid %>% left_join(violent.assault)

##### From who is missing allocate the average rate by peace level and region

tmp <- missing(violent.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)

tmp <- tmp %>% left_join(violent.assault.average) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="violent assault") %>% na.omit() 

#ADD to main DF

violent.assault <- violent.assault %>% na.omit() %>% rbind(tmp) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
violent.assault <- gpi.grid %>% left_join(violent.assault)

tmp <- missing(violent.assault)


#If data still does not have a match so I will take the average of the peace level #

tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% left_join(violent.assault.peace.level) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="violent assault")


# The 'additional' missing data needs to be added in 

violent.assault <- violent.assault %>% na.omit() %>% rbind(tmp)

violent.assault <- violent.assault[violent.assault$iso3c %in% pos,]


rm(violent.assault.average)
rm(violent.assault.peace.level)





#################### Sexual violence #############################
# Not in GPI data 


#sexual.assault <- read_csv("Data/sexual_violence.csv") %>% 
sexual.assault <- read_csv("Data/2023_data/sexual_violence.csv") %>% 
  rename(country=Country, year=Year, value="Measure Values", rate="Measure Names") %>% 
  select(country, year, value, rate) %>% 
  mutate(value = as.numeric(as.character(gsub(",","",value)))) %>% 
  subset(year>2006)  %>%
  #subset(year>2007)  %>% 
  subset(!country=="United Kingdom (Northern Ireland)") %>% 
  subset(!country=="United Kingdom (Scotland)") %>%
  subset(!rate=="Count")

sexual.assault = sexual.assault %>% mutate(value = round(value,2))




sexual.assault$iso3c <- countrycode::countrycode(sexual.assault$country, "country.name","iso3c")
sexual.assault$iso3c[sexual.assault$country=="Kosovo under UNSCR 1244"] <- "KSV"
sexual.assault$country[sexual.assault$iso3c=="KSV"] <- "Kosovo"
sexual.assault$variablename <- "sexual assault"
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]



### WORK OUT WHO IS MISSING



#work out who is missing data
sexual.assault <- gpi.grid %>% left_join(sexual.assault)
tmp <- missing(sexual.assault)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))



# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==9) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==15) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}

count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

sexual.assault <- sexual.assault %>% left_join(count_of_missing)



#delete out all that dont have at least one variable!  
sexual.assault <- sexual.assault %>% subset(!new=="delete")



# We can now impute given what remains has at least one value
sexual.assault <- sexual.assault %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="sexual assault")

sexual.assault <- index_data_pad(sexual.assault)

sexual.assault <- sexual.assault %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed)




## Average by peace and region

sexual.assault.average <- sexual.assault %>%  left_join(Peace_and_region)
sexual.assault.average <- sexual.assault.average %>% group_by(region, peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))


# get average for peace level (this is because PRK does not have data and no other coutnry in the region is very low peace)
#calculated by just using the average of Very low peace countries
sexual.assault.peace.level <- sexual.assault %>%  left_join(Peace_and_region)
sexual.assault.peace.level <- sexual.assault.peace.level %>% select(-region) %>% 
  group_by(peace_level,year) %>%
  summarise(average=mean(value, na.rm=T))



##### ADD IN MISSING COUNTRIES AND LEFT JOIN IN

sexual.assault <- gpi.grid %>% left_join(sexual.assault)

##### From who is missing allocate the average rate by peace level and region

tmp <- missing(sexual.assault)
tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)

tmp <- tmp %>% left_join(sexual.assault.average) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="sexual assault") %>% na.omit() 

#ADD to main DF

sexual.assault <- sexual.assault %>% na.omit() %>% rbind(tmp) 


##### MISSING COUNTRIES Because a certainr region in a year does not have that peace level.
sexual.assault <- gpi.grid %>% left_join(sexual.assault)

tmp <- missing(sexual.assault)


#If data still does not have a match so I will take the average of the peace level #

tmp <- tmp %>%  left_join(Peace_and_region)
tmp <- tmp %>% select(-variablename,-value)
tmp <- tmp %>% left_join(sexual.assault.peace.level) %>% distinct() %>% rename(value=average) %>%
  select(-peace_level)  %>% select(-region) %>% mutate(variablename="sexual assault")


# The 'additional' missing data needs to be added in 

sexual.assault <- sexual.assault %>% na.omit() %>% rbind(tmp)

sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]


rm(sexual.assault.average)
rm(sexual.assault.peace.level)


# sexual.assault <- rbind(sexual.assault, sexual.assault_missing)
sexual.assault <- sexual.assault[sexual.assault$iso3c %in% pos,]
sexual.assault <- subset(sexual.assault,!(iso3c=="PSE" & year<2014))
sexual.assault <- subset(sexual.assault,!(iso3c=="SSD" & year<2009))





##### TURN RATE TO COUNT FOR BOTH SEXUAL AND VIOLENT ASSAULT
# Violent and sexual assault together -------------------------------------
crime <- as.data.frame(rbind(violent.assault, sexual.assault)) 

crime <- crime%>% left_join(pop)

crime <- crime %>% mutate(value=(population/100000)*value) %>%
  select(iso3c, year, value, variablename)

# names(sexual.assault)
# 
# sexual.assault <- sexual.assault %>% select(iso3c, year, value, variablename)
# 
# violent.assault <-violent.assault %>%  select(iso3c, year, value, variablename)
# 
# # Violent and sexual assault together -------------------------------------
# crime <- as.data.frame(rbind(violent.assault, sexual.assault)) 
#   
#   
# # 
# #   merge(gpidata[,c("iso3c","region")], by="iso3c") %>% unique() %>% 
# #   group_by(year, variablename, region) %>% 
# #   mutate(regvalue=sum(value, na.rm = TRUE)) %>% 
# #   ungroup()
# 
# 
# 
# # # prepare population to impute based on that 
# # pop2 <- pop %>% rename(pop=value) %>% 
# #   merge(gpidata[,c("iso3c","year","region")]) %>% 
# #   subset(select=-country) %>% 
# #   group_by(year, region) %>% 
# #   mutate(regpop = sum(pop)) %>% 
# #   ungroup() %>% unique() %>% 
# #   mutate(popratio= pop/regpop)
# # pop2$year <- pop2$year-1
# 
# #impute sexual crime and violent crimes for countries with 
# # missing for the whole time series - making shit up
# # 
# # crime <- crime %>% 
# #   merge(pop2[,c("iso3c","year","popratio")], by=c("iso3c","year"))
# # 
# # for (i in 1:nrow(crime)){
# #   if (is.na(crime[i,"value"])){
# #     crime[i,"value.imputed"] <- crime[i,"regvalue"]*crime[i,"popratio"]
# #   }else{
# #     crime[i,"value.imputed"] <- crime[i,"value"]
# #   }
# # }
# 

crime = crime %>% mutate(value = round(value,3))

crime2 <- crime %>% group_by(iso3c, year,variablename) %>% 
  summarise(value=sum(value)) %>% ungroup()


rm(crime, violent.assault.tmp, violent.assault_missing, violent.assault_missing_tmp, violent.assault_regional_average, sexual.assault.tmp, sexual.assault_missing,
   sexual.assault_missing_tmp, sexual.assault_regional_average, tmp, fear.region.average)
