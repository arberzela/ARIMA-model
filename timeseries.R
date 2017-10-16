library(zoo)
library(anytime)
library(xts)
library(ggfortify)
library(lubridate)
library(ggplot2)

load("QuarterlyAggregated.RData")
time <- qa$DateTime
time_s <- qa$DateTimeNum
price <- qa$Weighted.Average.Price..EUR.
wind <- qa$TotalWind
pv <- qa$TotalSolar
mw <- qa$Volume.Buy..MW.

for (i in 1:length(price)){
  if (price[i]!=0){
    break
  }
}

price <- price[i:length(price)]
time <- ISOdatetime(2011,12,14,12,45,0)+1:19048*60*15
time_s <- time_s[i:length(time_s)]
wind <- wind[i:length(wind)]
pv <- pv[i:length(pv)]
mw <- mw[i:length(mw)]

pricemw <- price
pricemw <- replace(pricemw,is.nan(pricemw),0)

pvts<-xts(pv, time, frequency=96)
windts<-xts(wind, time, frequency=96)
ts <- xts(pricemw, time, frequency=96)
#ts<-diff(ts)
#ts_1 <- ts(pricemw, time, frequency=96)
#par(mfrow=c(2,1))
#plot(ts, xlab="Time", ylab= "Electricity Price (in ???/MWh)", main=NULL)
#plot(ts_1)

monthpv <- numeric(7)
daypv <- numeric(7)
for(i in 1:length(pvts)){
  if(month(pvts[i]) == 12){
    monthpv[1] <- monthpv[1] + pv[i]
    daypv[1] <- daypv[1] + 1
  } 
  else if(month(pvts[i]) == 1){
    monthpv[2] <- monthpv[2] + pv[i]
    daypv[2] <- daypv[2] + 1
  }
  else if(month(pvts[i]) == 2){
    monthpv[3] <- monthpv[3] + pv[i]
    daypv[3] <- daypv[3] + 1
  }
  else if(month(pvts[i]) == 3){
    monthpv[4] <- monthpv[4] + pv[i]
    daypv[4] <- daypv[4] + 1
  }
  else if(month(pvts[i]) == 4){
    monthpv[5] <- monthpv[5] + pv[i]
    daypv[5] <- daypv[5] + 1
  } 
  else if(month(pvts[i]) == 5){
    monthpv[6] <- monthpv[6] + pv[i]
    daypv[6] <- daypv[6] + 1
  }
  else if(month(pvts[i]) == 6){
    monthpv[7] <- monthpv[7] + pv[i]
    daypv[7] <- daypv[7] + 1
  }
}

monthpv <- monthpv/daypv
time_index1 <- seq(from = as.POSIXct("2011-12-14"), to = as.POSIXct("2012-06-29"), by = 30*3600*24)
#pvts <- xts(monthpv, time_index1)

log<- data.frame(Date=c("2011/12/25","2012/01/28","2012/02/15","2012/03/01","2012/04/02","2012/05/05","2012/06/07"), quant=monthpv)
log$Date <- as.Date(log$Date,"%Y/%m/%d")
log$Month <- as.Date(cut(log$Date, breaks = "month"))

# graph by month:
ggplot(data = log, aes(Month, quant)) + labs(y='Solar Infeed (in MW)') + stat_summary(fun.y = sum, geom = "bar", fill='darkblue')

monthpv <- numeric(7)
daypv <- numeric(7)
for(i in 1:length(windts)){
  if(month(windts[i]) == 12){
    monthpv[1] <- monthpv[1] + wind[i]
    daypv[1] <- daypv[1] + 1
  } 
  else if(month(windts[i]) == 1){
    monthpv[2] <- monthpv[2] + wind[i]
    daypv[2] <- daypv[2] + 1
  }
  else if(month(windts[i]) == 2){
    monthpv[3] <- monthpv[3] + wind[i]
    daypv[3] <- daypv[3] + 1
  }
  else if(month(windts[i]) == 3){
    monthpv[4] <- monthpv[4] + wind[i]
    daypv[4] <- daypv[4] + 1
  }
  else if(month(windts[i]) == 4){
    monthpv[5] <- monthpv[5] + wind[i]
    daypv[5] <- daypv[5] + 1
  } 
  else if(month(windts[i]) == 5){
    monthpv[6] <- monthpv[6] + wind[i]
    daypv[6] <- daypv[6] + 1
  }
  else if(month(windts[i]) == 6){
    monthpv[7] <- monthpv[7] + wind[i]
    daypv[7] <- daypv[7] + 1
  }
}

monthpv <- monthpv/daypv
time_index1 <- seq(from = as.POSIXct("2011-12-14"), to = as.POSIXct("2012-06-29"), by = 30*3600*24)
#pvts <- xts(monthpv, time_index1)

log<- data.frame(Date=c("2011/12/25","2012/01/28","2012/02/15","2012/03/01","2012/04/02","2012/05/05","2012/06/07"), quant=monthpv)
log$Date <- as.Date(log$Date,"%Y/%m/%d")
log$Month <- as.Date(cut(log$Date, breaks = "month"))

# graph by month:
ggplot(data = log, aes(Month, quant)) + labs(y='Wind Infeed (in MW)') + stat_summary(fun.y = sum, geom = "bar", fill='darkblue')