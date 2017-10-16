library(xts)

a<-numeric(96)
for (i in 1:length(price)){
  a[(i%%96)+1]<-a[(i%%96)+1]+price[i]
}

a<-a/198.4167

time_index <- seq(from = as.POSIXct("2012-05-15 13:00"), to = as.POSIXct("2012-05-16 12:45"), by = 900)

c<-a[2:96]
c[96]<-a[1]
a<-c
z <- xts(a, order.by = time_index)
plot(z, major.format = "%H:%M", xlab="Time", ylab= "Electricity Price", main=NULL)