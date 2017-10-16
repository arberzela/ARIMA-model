model1 <- auto.arima(price1, xreg=cbind(wind1, pv1))
model3<-Arima(ts, order = c(8,0,7), xreg=cbind(wind1, pv1))

price1<-xts(fitted(b), time)
plot(ts, xlab="Time", ylab= "Electricity Price (in ???/MWh)", main=NULL)
lines(price1, col=2)