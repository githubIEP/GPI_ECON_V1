# UNIT COST

#unit costs are in constant 2020 USD from the work done in the spreadsheet

##############   Much of the unit cost stuff is done in Excel. See spread sheet #####################

#unitcost <- read_excel("Data/unit costs for gpi2022.xlsx", 
unitcost <- read_excel("Data/2023_data/unit costs for gpi2022.xlsx", 
                          sheet = "unit costs r")
unitcost <- within(unitcost, indicator <- paste(Indicator, type,sep='.'))
unitcost <- unitcost[,c("indicator", "unitcost")]

unitcost <- spread(unitcost, indicator, unitcost)


unitcost2 <- cbind(ppp, unitcost)

unitcost.scaled <- mutate_at(unitcost2,vars(3:15), funs(.*scale) )

unitcost.scaled <- unitcost.scaled[,c(2,1,4:15)]





