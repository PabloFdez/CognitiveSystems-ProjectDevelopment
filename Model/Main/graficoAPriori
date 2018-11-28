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

#########################################
## GRAFICO APRIORI:

# aprioriTab <- subset(baskets, baskets$Item == listOfNames[1])
# firstIteration = FALSE
# for (i in listOfNames) {
#   print(i)
#   if (firstIteration == FALSE){
#     firstIteration = TRUE
#     next
#   }
#   aprioriTab <- rbind(aprioriTab,subset(baskets, baskets$Item == i))
# }
# 
# ggplot(aprioriTab,aes(aprioriTab$Item)) + 
#   geom_bar(width=0.7, fill="steelblue") + 
#   theme(axis.text.x = 
#           element_text(size  = 10,angle = 45,
#                        hjust = 1,vjust = 1)) +
#   xlab("Items") + ylab("Count")
# # + labs(x="", y="", title="Not cleaned data Items count") 



ItemsCountFirstX <- function(x){
  xMostSoldItems <- sort(table(baskets$Item),decreasing=TRUE)[1:x]
  print(xMostSoldItems)
  listOfNames <- names(xMostSoldItems)
  aprioriTab <- subset(baskets, baskets$Item == listOfNames[1])
  firstIteration = FALSE
  for (i in listOfNames) {
    if (firstIteration == FALSE){
      firstIteration = TRUE
      next
    }
    aprioriTab <- rbind(aprioriTab,subset(baskets, baskets$Item == i))
  }
  
 
  ggplot(aprioriTab,aes(aprioriTab$Item)) + 
    geom_bar(width=0.7, fill="steelblue") + 
    theme(axis.text.x = 
            element_text(size  = 10,angle = 45,
                         hjust = 1,vjust = 1)) +
    xlab("Items") + ylab("Count") + 
    labs(x="", y="")
}


ItemsCountFirstX(10)
