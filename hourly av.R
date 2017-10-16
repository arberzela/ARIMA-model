library(xts)
library(zoo)
model2 <- Arima(price1, order = c(8, 0, 7), xreg = cbind(wind1, pv1))
a <- numeric(4762)
u <- numeric(4762)
o <- numeric(4762)

#fit <- xts(fitted(model2), time)
price2 <- resid(deas_price)
i <- 1
b <- 1
while(i < length(pricemw)){
  a[b] <- (pricemw[i]+pricemw[i+1]+pricemw[i+2]+pricemw[i+3])/4
  u[b] <- (price2[i]+price2[i+1]+price2[i+2]+price2[i+3])/4
  o[b] <- (fitted(model2)[i]+fitted(model2)[i+1]+fitted(model2)[i+2]+fitted(model2)[i+3])/4
  i <- i+4
  b <- b+1
}


time1 <- ISOdatetime(2011,12,14,12,0,0)+1:4762*60*60

c <- xts(a, time1)
d <- xts(u, time1)
fit1 <- xts(o, time1)
#autoplot(c,  xlab="Time", ylab= "Electricity Price (in ???/MWh)", main=NULL)