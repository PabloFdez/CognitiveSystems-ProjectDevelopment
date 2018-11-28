###################################################
#####            PROJECT DEVELOPMENT          #####
###################################################

# COGNITIVE SYSTEMS
# 21 - NOV - 2018

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(arules)

# Media beneficios al mes:
#   (21293*2)/6
# [1] 7097.667
baskets <- read_csv("/Users/PFD/Desktop/UPM/MUII/S1/Cognitivos/Entrega1/baskets_DMS.csv", na = "NONE")
View(baskets)

baskets$Time <- as.POSIXct(baskets$Time,format="%H:%M:%S")
baskets$hour <- as.numeric(strftime(baskets$Time, format="%H"))
baskets[, c("Date", "Time")] <- NULL

perTransaction <- aggregate(baskets$Item, list(baskets$Transaction),length)
hist(baskets$Time,breaks="hours")
hist(baskets$hour)

breakfast <- baskets[baskets$hour>=7 & baskets$hour <=12,]
breakfast[, c("Date", "Time")] <- NULL
breakfast <- breakfast[breakfast$Item !="NCN", ]

frequentItems <- eclat (baskets, parameter = list(supp = 0.07, maxlen = 15))
print(data(Groceries))

#step 1
head(baskets)
#check both transaction and item are factors
class(baskets$Transaction)
class(baskets$Item)
baskets$Transaction <- as.factor(baskets$Transaction)
class(baskets$Transaction)

#step 2
isolate vectors
transact <- subset(baskets$Transaction, baskets$Transaction != 'Other')
item <- subset(baskets$Item, baskets$Item != 'Other')

#step3
create item list
transact_by_item <- split(item, transact)
transact_by_item <- split(baskets$Item, baskets$Transaction)
class(transact_by_item)
basket <- as(transact_by_item, "transactions")

# #step4
basket <- as(transact_by_item, "transactions")
View(transact_by_item)

inspect(basket[1:20])
 
#step apply apriori
soporte <- 30 / dim(basket)[1]
rules <- apriori(basket,parameter = list(support = soporte,confidence = 0.30,target = "rules"))
summary(rules)
a <- inspect(sort(x = rules, decreasing = TRUE, by = "confidence"))

breakfast$Transaction <- as.factor(breakfast$Transaction)
transact_by_itemB <- split(breakfast$Item, breakfast$Transaction)
basketB <- as(transact_by_itemB, "transactions")
rulesB <- apriori(basketB,parameter = list(support = soporte,confidence = 0.30,target = "rules"))
summary(rulesB)
aB <- inspect(sort(x = rulesB, decreasing = TRUE, by = "confidence"))

sort(aB$count,decreasing=TRUE)[1:14]

# ITEMS MAS VENDIDOS JUNTOS en el DESAYUNO:

# 317 -> Pastry & COFFEE                        
# 224 -> Medialuna & COFFEE 
# 193 -> Pastry & BREAD 
# 168 -> Tea & COFFEE 
# 147 -> Toast & COFFEE 
# 136 -> Cake & COFFEE
# 120 -> Medialuna & BREAD 
# 117 -> Hot chocolate & COFFEE 
# 117 -> Cookies & COFFEE 
# 80 -> Muffin & COFFEE
