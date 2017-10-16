library(forecast)
library(stats)
library(lubridate)

monthd <- model.matrix(~as.factor(month(ts)))[,2:7]
colnames(monthd) <- c('Feb','Mar','Apr','May','Jun', 'Dec')

weekd <- model.matrix(~as.factor(wday(ts)))[,2:7]
colnames(weekd) <- c('Tue' ,'Wed','Thu','Fri','Sat','Sun')

hourd <- model.matrix(~as.factor(hour(ts)))[,2:24]
colnames(hourd) <- c(1:23)

xreg<- cbind(monthd, weekd, hourd)

deas_price <- lm(ts ~ xreg)
deas_wind <- Arima(windts, order = c(0,0,0), xreg = xreg)
deas_pv <- Arima(pvts, order = c(0,0,0), xreg = xreg)

price1 <- xts(resid(deas_price), time)
wind1 <- xts(resid(deas_wind), time)
pv1 <- xts(resid(deas_pv), time)

#autoplot(price1)+labs(x='Time', y='Electricity Price (in ???/MWh)')