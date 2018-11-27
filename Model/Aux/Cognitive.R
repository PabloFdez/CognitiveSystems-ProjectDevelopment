baskets <- read.csv("BreadBasket_DMS.csv")
baskets$Time <- as.POSIXct(baskets$Time,format="%H:%M:%S")
baskets$hour <- as.numeric(strftime(baskets$Time, format="%H"))
baskets[, c("Date", "Time")] <- NULL

perTransaction <- aggregate(baskets$Item, list(baskets$Transaction),length)
hist(baskets$Time,breaks="hours")
hist(baskets$hour)

breakfast <- baskets[baskets$hour>=7 & baskets$hour <=12,]
breakfast[, c("Date", "Time")] <- NULL
breakfast <- breakfast[breakfast$Item !="NCN", ]