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
pan = lista[1]
 if (lista[1] == pan){
   print("Hola")
 }
 
 
 lista <- transact_by_item
 n <- length(transact_by_item)
 
 lisDup <- duplicated(lista, incomparables = NA)
 listaNom <- "Articulos"
 
 a = 1
 b=1
 for(b in 1: n){
   if(lisDup[b] == TRUE){
     listaNom[a] <- lista[b]
     a = a + 1;
   }
 }
 
 i = 0
 j = 0
 
 
 for (i in 1:100){
 
   for (j in 1:100){

     aux = transact_by_item[i]
     rep = 0
 
     #print(transact_by_item[j])
 
     if(transact_by_item[j] == transact_by_item[i]){
       rep = rep +1
       }
   }
 
   print(rep)
}
