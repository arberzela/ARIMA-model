library(zoo)
library(anytime)
library(xts)
library(ggfortify)
library(lubridate)
library(ggplot2)

m <- as.data.frame(deas_price$coefficients[2:7])
m <- m[,1]
m <- c(m[6],m[1:5])
d <- as.data.frame(deas_price$coefficients[8:13])
d <- d[,1]
h <- as.data.frame(deas_price$coefficients[14:36])
h <- h[,1]

log1<- data.frame(date=c("2011/12/27","2012/02/15","2012/03/08","2012/04/06","2012/05/05","2012/06/10"), quant=m)
log1$date <- as.Date(log1$date,"%Y/%m/%d")
log1$daily <- d
log1$weekday <- weekdays(log1$date)
log1$weekday <- factor(log1$weekday, levels=c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
log1$month <- as.Date(cut(log1$date, breaks = "month"))

log2 <- data.frame(hr=seq(23), hourly=h)
log2$hr <- factor(log2$hr, levels=seq(23))

# graph by month:
ggplot(data = log1, aes(month, quant)) + labs(y='Average Electricity Price (in ???/MW)', x='Month') + stat_summary(fun.y = sum, geom = "bar", fill='darkblue')
ggplot(data = log1, aes(weekday, daily)) + labs(y='Average Electricity Price (in ???/MW)', x='Weekday') + stat_summary(fun.y = sum, geom = "bar", fill='darkblue')
ggplot(data = log2, aes(hr, hourly)) + labs(y='Average Electricity Price (in ???/MW)', x='Hour') + stat_summary(fun.y = sum, geom = "bar", fill='darkblue')
