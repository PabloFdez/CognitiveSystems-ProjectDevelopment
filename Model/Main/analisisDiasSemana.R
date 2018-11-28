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

# TOP 10 Productos mas vendidos en el desayuno
sort(table(breakfast$Item),decreasing=TRUE)[1:10]

  # 5 horas del DESAYUNO que mas productos se venden
  sort(table(breakfast$hour),decreasing=TRUE)[1:5]

# TOP 10 Productos mas vendidos
sort(table(baskets$Item),decreasing=TRUE)[1:10]
  
  # 5 horas del DÍA que mas productos se venden
  sort(table(baskets$hour),decreasing=TRUE)[1:5]
  
# [30-OCT-16]    [31-OCT-16]    [1-NOV-16]       [2-NOV-16]     [3-NOV-16]    [4-NOV-16]     [5-NOV-16]   
#   DOMINGO         LUNES         MARTES          MIERCOLES        JUEVES        VIERNES        SABADO  

weekDays <- baskets
weekDays[, c("Item", "Time")] <- NULL  
View(weekDays)

# Filtramos el numero de Pedidos diarios entre los dias de una semana
weekDays %>% filter(between(Date, ymd("2016-10-30"), ymd("2016-11-5")))

# Buscamos el numero maximo de pedidos por día
weekSales <-weekDays %>%  filter(between(Date, ymd("2016-10-30"), ymd("2016-10-31")))
View(weekSales)

DOMINGO=filter(weekDays, Date=="2016-10-30")
count(DOMINGO) # Domingo = 180

LUNES=filter(weekDays, Date=="2016-10-31")
count(LUNES) # Lunes = 205

MARTES=filter(weekDays, Date=="2016-11-1")
count(MARTES) # Martes = 154

MIERCOLES=filter(weekDays, Date=="2016-11-2")
count(MIERCOLES) # Miercoles = 169

JUEVES=filter(weekDays, Date=="2016-11-3")
count(JUEVES) # Jueves = 195

VIERNES=filter(weekDays, Date=="2016-11-4")
count(VIERNES) # Viernes = 192

SABADO=filter(weekDays, Date=="2016-11-5")
count(SABADO) # Sabado = 283

print(sort(table(baskets$Item),decreasing=TRUE))
