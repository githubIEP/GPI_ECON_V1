#unit cost variables are in constant 2017 USD
# except for refugee & IDP 

#Deaths.from.Internal.Conflict" |indicator=="Deaths.from.External.Conflict

unitcost.var <- crime2 %>% 
  spread(variablename, value) %>%
  left_join(homicide[,c("iso3c","year","value")], by=c("iso3c","year")) %>%
  rename(homicide=value) %>% left_join(incar[,c("iso3c", "year","value")]) %>%
  rename(incar=value) %>% 
  left_join (conflict[,c("iso3c","year","battle_deaths")],
        by=c("iso3c","year")) %>% 
  left_join(gti.sum[,c("iso3c","year","killed","wounded")], by=c("iso3c","year")) %>% 
 left_join(fear, by=c("iso3c","year")) %>% 
  left_join(suicide, by=c("iso3c", "year")) %>%  
 left_join (unitcost.scaled, by=c("iso3c", "year"))



unitcost.var <- unitcost.var %>% 
  # Assault
  mutate(assault.dir.cost=`violent assault`*violentassault.direct, assault.indir.cost=`violent assault`*violentassault.indirect) %>%
  #sexual assault
  mutate(sexassault.dir.cost=`sexual assault`*sexualassault.direct, sexassault.indir.cost=`sexual assault`*sexualassault.indirect)%>%
  # suicide
  mutate(suicide.dir.cost=`suicidevalue`*suicide.direct, suicide.indir.cost=`suicidevalue`*suicide.indirect) %>% 
  #homicide
  mutate(homi.dir.cost=homicide*homicide.direct, homi.indir.cost=homicide*homicide.indirect)%>%
  #incarceration
  mutate(incar.dir.cost=incar*minwage.direct)%>%
  #fear
  mutate(fear.indir.cost=fear*fear.indirect)%>%
  #ext conflict deaths
  mutate(battle_deaths.dir.cost=battle_deaths * homicide.direct)%>%

  # terrorism deaths
  mutate(terrdeath.dir.cost=killed*homicide.direct, terrdeath.indir.cost=killed*homicide.indirect)%>%
  # terrorism wounded
  mutate(wounded.dir.cost=wounded*violentassault.direct, wounded.indir.cost=wounded*violentassault.indirect)


unitcost.var <- merge(unitcost.var, refugidp[,c("iso3c","year","refugeidp")],
                      by=c("iso3c", 'year'))

unitcost.var2 <- unitcost.var %>% 
  subset(select=c(1:2,24:39)) %>% 
  gather(indicator,value,-c(iso3c,year)) %>% 
  separate(indicator,c("indicator","type","cost")) %>% 
  subset(select=-cost) %>% mutate(type=ifelse(indicator=="refugeidp","indir",type)) %>% 
  mutate(type1=ifelse(type=="indir","indirect","direct")) %>% 
  subset(select=-type) %>% rename(type=type1)



# non unit cost vars ------------------------------------------------------
# all in current USD, need to deflate



# uncost <- milex %>% merge(pos.exp, by=c("iso3c","year"), all=TRUE) %>% 
#   merge(priv.secu, by=c("iso3c","year"), all=TRUE) %>% 
#   full_join(secu.agnecy,by=c("iso3c","year"),all=TRUE) %>% 
#   full_join(small.arms,by=c("iso3c","year"),all=TRUE) %>% 
#   full_join(peacebuilding,by=c("iso3c","year"),all=TRUE) %>% 
#   full_join(peacekeeping,by=c("iso3c","year"),all=TRU) %>% 
#   full_join(unhcr, by=c("iso3c","year"),all=TRUE) %>% 
#   merge(vet, by=c("iso3c","year"),all=TRUE) %>% unique() %>% 
#   merge(def.wdi2, by="year",all=TRUE) 
#%>% 
 # subset(year >2006) %>% subset(year < 2022) 
 #subset(year >2007) %>% subset(year < 2023) 

uncost <- milex %>% left_join(pos.exp, by=c("iso3c","year")) %>% 
  left_join(priv.secu, by=c("iso3c","year")) %>% 
  left_join(secu.agnecy,by=c("iso3c","year")) %>% 
  left_join(small.arms,by=c("iso3c","year")) %>% 
  left_join(peacebuilding,by=c("iso3c","year")) %>% 
  left_join(peacekeeping,by=c("iso3c","year")) %>% 
  left_join(unhcr, by=c("iso3c","year")) %>% 
  left_join(vet, by=c("iso3c","year")) %>% 
  left_join(deflator, by=c("year"))



########## convert all to constant 2022      ##########

uncost <- mutate_at(uncost, vars(3:11), funs(./deflator))   # Fix later 3:11

# GDP losses that are already in constant 2019 USD
uncost <- left_join(uncost, gdplosses, by=c("iso3c","year"))

uncost2 <- uncost %>%
  subset(select=-deflator) %>% 
  gather(indicator, value, -c(iso3c, year)) %>% 
  mutate(type="direct") 
#%>% na.omit(value)



# put all teh costs together  ---------------------------------------------

# to do
#1) check sum with last years USD constant
#2) convert to PPP
#3)create Impact
#4) % of GDP
#5) per capita
# def.med <- ppp.conv2 %>% group_by(iso3c) %>% 
#     summarise(con.median=median(value))



# ppp.conv <- ppp.conv %>%  select(-variablename) 

cost <- rbind(uncost2, unitcost.var2) %>% rename(costusd=value) %>% 
  left_join(ppp.conv, by=c("iso3c", "year")) %>% rename(con.median=value)


cost <- cost %>%   merge(gdp.wdi, by=c("iso3c","year"), all=TRUE) %>%  
  left_join(pop, by=c("iso3c","year")) %>% 
  rename(pop=population) %>% na.omit(indicator)



cost$gdp <- as.numeric(as.character(cost$gdp))

cost2 <- cost
cost <-cost2



# create ppp costs

#pos.converter <- c('assault','fear','extconfdeath','homi',"incar", "intconfdeath","sexassault","suicide", "terrdeath", "wounded")
pos.ppp <- c("gdplosses","intsecu","milex","peacebuilding","peacekeep","privsecu.cost",
             "refugeidp","sarms","secu.agnecy","unhcr","vet.int")
cost$ppp <- 0
cost[cost$indicator %in% pos.ppp,"ppp"] <-  1








cost$costppp <- cost$costusd

for(i in 1:nrow(cost)){
  if(cost[i,"ppp"]==1){
    cost[i,"costppp"] <- cost[i,"costusd"]/cost[i,"con.median"]
  }
}




cost$type[cost$indicator=="gdplosses"]='indirect'


for(j in 1:nrow(cost)){
  if(cost[j,"type"]=="direct"){
    cost[j,"impact"] <- cost[j,"costppp"]*2
  }else{
    cost[j,"impact"] <- cost[j,"costppp"]
  }
}


cost[c,'pdgp.cost.usd1'] <- cost[c,"costusd"]/cost[c,"gdpcons"]
cost[c,'pgdp.cost.ppp1'] <- cost[c,"costppp"]/cost[c,"gdpconsppp"]
cost[c,'pgdp.impact'] <- cost[c,"impact"]/cost[c,"gdpconsppp"]
#per capita
cost[c,'pcap.cost.usd'] <- cost[c,"costusd"]/cost[c,"pop"]
cost[c,'pcap.cost.ppp'] <- cost[c,"costppp"]/cost[c,"pop"]
cost[c,'pcap.impact'] <- cost[c,"impact"]/cost[c,"pop"]


#domain
cost$domain[cost$indicator=="assault"]="Violent crime"
cost$domain[cost$indicator=="battle"]="Conflict"
cost$domain[cost$indicator=="fear"]="Other"
cost$domain[cost$indicator=="gdplosses"]="Conflict"
cost$domain[cost$indicator=="homi"]="Homicide"
cost$domain[cost$indicator=="incar"]="Internal security expenditure"
cost$domain[cost$indicator=="intsecu"]="Internal security expenditure"
cost$domain[cost$indicator=="milex"]="Military expenditure"


cost$domain[cost$indicator=="peacebuilding"]="Conflict"
cost$domain[cost$indicator=="peacekeep"]="Conflict"
cost$domain[cost$indicator=="privsecu.cost"]="Private security expenditure"
cost$domain[cost$indicator=="refugeidp"]="Conflict"
cost$domain[cost$indicator=="sarms"]="Other"
cost$domain[cost$indicator=="secu.agnecy"]="Internal security expenditure"
cost$domain[cost$indicator=="sexassault"]="Violent crime"
cost$domain[cost$indicator=="suicide"]="Suicide"
cost$domain[cost$indicator=="terrdeath"]="Conflict"
cost$domain[cost$indicator=="unhcr"]="Conflict"
cost$domain[cost$indicator=="vet.int"]="Military expenditure"
cost$domain[cost$indicator=="wounded"]="Conflict"



#indicator for charts
cost$indicator2[cost$indicator=="assault"]="Violent crime"
cost$indicator2[cost$indicator=="battle"]="Conflict deaths"
cost$indicator2[cost$indicator=="fear"]="Fear"
cost$indicator2[cost$indicator=="gdplosses"]="GDP losses"
cost$indicator2[cost$indicator=="homi"]="Homicide"
cost$indicator2[cost$indicator=="incar"]="Incarceration"
cost$indicator2[cost$indicator=="intsecu"]="Internal security expenditure"
cost$indicator2[cost$indicator=="milex"]="Military expenditure"


cost$indicator2[cost$indicator=="peacebuilding"]="Peacebuilding"
cost$indicator2[cost$indicator=="peacekeep"]="Peacekeeping"
cost$indicator2[cost$indicator=="privsecu.cost"]="Private security"
cost$indicator2[cost$indicator=="refugeidp"]="Refugees and IDPs"
cost$indicator2[cost$indicator=="sarms"]="Small arms"
cost$indicator2[cost$indicator=="secu.agnecy"]="Internal security expenditure"
cost$indicator2[cost$indicator=="sexassault"]="Violent crime"
cost$indicator2[cost$indicator=="suicide"]="Suicide"
cost$indicator2[cost$indicator=="terrdeath"]="Terrorism"
cost$indicator2[cost$indicator=="unhcr"]="Refugees and IDPs"
cost$indicator2[cost$indicator=="vet.int"]="Military expenditure"
cost$indicator2[cost$indicator=="wounded"]="Terrorism"




econcost <- cost %>% 
  subset(select=c(1:5,13:21)) %>% 
  gather(subtype, value, -c(iso3c,year,indicator, type, domain,indicator2)) %>% 
  mutate(country=countrycode(iso3c,"iso3c","country.name")) 
econcost$country[econcost$iso3c=="KSV"] <- "Kosovo"


#fix errors
 
econcost$value[econcost$indicator=="gdplosses" & econcost$iso3c=="IND"]=0
econcost$value[econcost$indicator=="gdplosses" & econcost$iso3c=="CHN"]=0


econcost <- econcost %>% distinct()

#econcost_aggr <- econcost %>% 
 # filter (year == 2021 & subtype =='impact') %>%
  #summarize (total_cost = sum(value))


# tmp <- econcost %>% subset(subtype=="impact") %>% group_by(year, domain) %>% summarise(total=sum(value)) %>% ungroup() %>% spread(domain, total)
tmp <- econcost %>% subset(subtype=="impact") %>% group_by(year) %>% summarise(total=sum(value)) %>% ungroup()

p = tmp %>% ggplot(aes(x = year, y = total/10^12)) +
  geom_line (size = 0.75, color = 'red') + 
  # scale_y_continuous (breaks = c(15000000000000, 16000000000000, 17000000000000, 
  #                                18000000000000, 19000000000000, 20000000000000),
  #                     labels = c("15", "16", "17", "18", "19", "20")) +
  scale_x_continuous (breaks = c(2008:2022)) + 
  labs(y = "Total Cost (Constant 2022 US$ PPP, trillions)")
print(p)



rio::export(econcost, "Economic Impact of Violence.xlsx")



