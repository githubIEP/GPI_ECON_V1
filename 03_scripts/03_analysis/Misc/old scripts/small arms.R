
########################  New way   #################################################
# We have recieved the small arms survey data. The following script will clean it. 


# You will only need to add an additional year now on, the clean data is called: 
##                    small arms cleaned for 2020 GPI.csv




setwd('S:/Institute for Economics and Peace/Global Peace Index/2021 GPI/Economic Impact of Violence/Small arms survey')

tmp <- read_excel("Total imports by countries 2007-17.xlsx", 
                  sheet = 1, col_types = c("text", 
                                           "numeric", "text", "text", "numeric"))


{j = length(excel_sheets("Total imports by countries 2007-17.xlsx"))
  for(i in 1:j){
    
    print(i)
    tmp2 = rio::import("Total imports by countries 2007-17.xlsx", which = i)
    
    #join together
    
    tmp <- rbind(tmp, tmp2)}}


tmp <- distinct(tmp)


tmp <- tmp %>% mutate(year=as.numeric(as.character(gsub("X","", year)))) %>% rename(value=total) %>% 
  mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% 
  mutate(iso3c=countrycode(Country, "country.name","iso3c")) 

tmp$variablename <- "small arms"
tmp <- tmp[tmp$iso3c %in% pos,]


tmp <- mutate(tmp, year=year+3)


#work out who is missing data
small_arms <- gpi.grid %>% left_join(tmp)
tmp <- missing(small_arms)

count_of_missing <- as.data.frame(table(tmp$iso3c))
count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))



# This will tell us who to delete to impute (you will need to update the numbers here)
#it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 

for(c in 1:nrow(count_of_missing)){
  if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==7) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==12) {
    count_of_missing[c,"new"]<-"delete"}
  else if(count_of_missing[c,"Freq"]==14) {
    count_of_missing[c,"new"]<-"delete"}
  else {count_of_missing[c,"new"] <- "keep"}
}



count_of_missing <- count_of_missing %>% rename(iso3c=Var1)

small_arms <- small_arms %>% left_join(count_of_missing)



#delete out all that dont have at least one variable!  
small_arms <- small_arms %>% subset(!new=="delete")



# We can now impute given what remains has at least one value
small_arms <- small_arms %>% select(iso3c, variablename, year, value) %>% 
  rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
  mutate(value=as.numeric(value)) %>% mutate(variablename="small arms")

small_arms <- index_data_pad(small_arms)
small_arms <- small_arms %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
  rename(iso3c=geocode, value=imputed) 


small.arms = small_arms
small.arms <- rename(small.arms, sarms = value) %>% select(-variablename)

# write.csv(small.arms, "small arms cleaned for 2020 GPI.csv")



########################              OLD WAY #################################################

setwd('C:/Users/hbardwell/Documents/Github/GPI_2021_ECONOMIC_IMPACT')

# 
# 
# 
# small.arms <- read_excel("Data/Small arms survey data 2020.xlsx", sheet = "clean")%>%
#   rename(country=Country) %>% 
#   mutate(year=as.numeric(as.character(gsub("X","", year)))) %>% 
#   mutate(value=as.numeric(as.character(gsub(",","",value)))) %>% 
#   mutate(iso3c=countrycode(country, "country.name","iso3c")) 
# 
# 
# 
# small.arms$variablename <- "small arms"
# small.arms <- small.arms[small.arms$iso3c %in% pos,]
# 
# 
# 
# ### WORK OUT WHO IS MISSING
# 
# 
# 
# #work out who is missing data
# small.arms <- gpi.grid %>% left_join(small.arms)
# tmp <- missing(small.arms)
# 
# count_of_missing <- as.data.frame(table(tmp$iso3c))
# count_of_missing <- count_of_missing %>%  mutate(Freq=as.numeric(Freq))
# 
# 
# 
# # This will tell us who to delete to impute (you will need to update the numbers here)
# #it is saying if it has zero values (14 NAs) then write delete, PSE and SSD are exceptions 
# 
# for(c in 1:nrow(count_of_missing)){
#   if(count_of_missing[c,"Var1"]=="PSE" & count_of_missing[c,"Freq"]==7) {
#     count_of_missing[c,"new"]<-"delete"}
#   else if(count_of_missing[c,"Var1"]=="SSD" & count_of_missing[c,"Freq"]==12) {
#     count_of_missing[c,"new"]<-"delete"}
#   else if(count_of_missing[c,"Freq"]==14) {
#     count_of_missing[c,"new"]<-"delete"}
#   else {count_of_missing[c,"new"] <- "keep"}
# }
# 
# count_of_missing <- count_of_missing %>% rename(iso3c=Var1)
# 
# small.arms <- small.arms %>% left_join(count_of_missing)
# 
# 
# 
# #delete out all that dont have at least one variable!  
# small.arms <- small.arms %>% subset(!new=="delete")
# 
# 
# 
# # We can now impute given what remains has at least one value
# small.arms <- small.arms %>% select(iso3c, variablename, year, value) %>% 
#   rename(geocode=iso3c) %>% mutate(year=as.numeric(year))%>%
#   mutate(value=as.numeric(value)) %>% mutate(variablename="small arms")
# 
# small.arms <- index_data_pad(small.arms)
# 
# # Small.arms.data <- small.arms%>%  mutate(value=imputed*10^6)
# 
# small.arms <- small.arms %>% select("geocode"    ,     "year"   , "imputed"  ,       "variablename") %>%
#   rename(iso3c=geocode, value=imputed) %>%  mutate(value=value*10^6)




