######### THIS IS THE OLD WAY> DO BOTH AND SEE IF YOU COME UP WITH THE SAME NUMBERS  ########


# US veteran affairs and interest on milex or war borrowing ---------------

vet <- read_excel("Data/veteran affairs calc.xlsx") %>%
  rename(year="Function and Subfunction", vet.int="total" ) %>% mutate(iso3c="USA") %>% 
  subset(select=c(iso3c,year,vet.int)) %>% 
  full_join(gpi.grid, by=c("iso3c","year")) %>% 
  mutate(vet.int=ifelse(is.na(vet.int),0,vet.int))
vet <- subset(vet,!(iso3c=="PSE" & year<2015))
vet <- subset(vet,!(iso3c=="SSD" & year<2010))


############# Automate Vet affairs and interest

################## https://www.whitehouse.gov/omb/historical-tables/  ####################

#Update the link to Table 5.1—Budget Authority by Function and Subfunction: 1976–2025
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2020/02/hist05z1_fy21.xlsx"   ## UPDATE GPI 2022   Update the link to Table 5.1—Budget Authority by Function and Subfunction: 1976–2025
download.file(dataURL, destfile=temp, mode='wb')

test <- readxl::read_excel(temp, skip =2)
test <-test %>%  subset(`Function and Subfunction`=="Total, Veterans Benefits and Services") %>%
  rename(`2020`=`2020 estimate`, `2021`=`2021 estimate`, `2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`) %>% 
  gather(year, value, -c(`Function and Subfunction`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6)




#Update the link to Table 6.1—Composition of Outlays: 1940–2025
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://www.whitehouse.gov/wp-content/uploads/2020/02/hist06z1_fy21.xlsx"      ## UPDATE GPI 2022 Update the link to Table 6.1—Composition of Outlays: 1940–2025
download.file(dataURL, destfile=temp, mode='wb')


test_interest <- readxl::read_excel(temp, skip =1)
test_interest <- test_interest[9,]

test_interest <- test_interest %>% rename(`2020`=`2020 estimate`, `2021`=`2021 estimate`, `2022`=`2022 estimate`,`2023`=`2023 estimate`,`2024`=`2024 estimate`,`2025`=`2025 estimate`) %>% 
  gather(year, value, -c(`Category`)) %>%  subset(!year=="TQ") %>% 
  mutate(year=as.numeric(year), value=as.numeric(value)) %>% mutate(value=value*10^6) %>%
  mutate(interest=value*0.2) %>% select(year, interest)


vet_tmp <- left_join(test,test_interest)
vet_tmp <- vet_tmp %>%subset(year>2006) %>% mutate(value=value+interest) %>% select(year, value) %>%subset(year<2021)
vet_tmp$iso3c = "USA"


vet_tmp <- vet_tmp %>% full_join(gpi.grid, by=c("iso3c","year")) %>% 
  mutate(value=ifelse(is.na(value),0,value))


vet_tmp <- subset(vet_tmp,!(iso3c=="PSE" & year<2015))
vet_tmp <- subset(vet_tmp,!(iso3c=="SSD" & year<2010))

vet_tmp <- vet_tmp %>%  rename(vet.int=value)

vet = vet_tmp


