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
print(sort(table(baskets$Item),decreasing=TRUE))

# Ocurrencias de los articulos en el periodo del desayuno
with(breakfast, table(Item)) 
# Ocurrencias de los articulos totales
with(baskets, table(Item)) 

# Top 10 de los productos mas comunes en el desayuno
ocur <- sort(with(breakfast, table(Item)),decreasing=TRUE)[1:10]
View(ocur)
# Top 10 de los productos mas comunes totales
ocurTot <- sort(with(baskets, table(Item)) ,decreasing=TRUE)[1:10]
View(ocurTot)

# Listas de nombres de los productos mas vendidos (Top10)
nom <- names(ocur)
nomTot <- names(ocurTot)
