
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

sort(a$count,decreasing=TRUE)[1:12]

# ITEMS MAS VENDIDOS JUNTOS:

# 518 -> Cake & COFFEE                        
# 472 -> Tea & COFFEE 
# 450 -> Pastry & COFFEE 
# 362 -> Sandwich & COFFEE 
# 333 -> Medialuna & COFFEE 
# 280 -> Hot chocolate & COFFEE 
# 276 -> Pastry & BREAD
# 267 -> Cookies & COFFEE 
# 224 -> Toast & COFFEE 
# 195 -> Juice & COFFEE 
