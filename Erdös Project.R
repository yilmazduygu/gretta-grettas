install.packages("fpp2")
library(fpp2)
install.packages("forecast")
library(forecast)
# Data load in
clim <- read.csv("G:/My Drive/Everything/Belgeler/RDirectory/noaa_newark_data.csv")

# Process the data
n <- length(clim$temp)
clim$date <- as.Date(paste(clim$year[1:n], clim$mo[1:n],clim$da[1:n], sep = "-"))
clim$X<-clim$X+1
head(clim,2)
colnames(clim) <- c("index", "year","mo","da","temp","dewp","visib","wdsp","prcp","fog","rain_drizzle","snow_ice_pellets","hail","thunder","tornado_funnel_cloud","date")

## How temperature changed over the years
par(mfrow=c(2,2))
plot(clim$date,clim$temp, pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2000)],clim$temp[which(clim$year>2000)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2010)],clim$temp[which(clim$year>2010)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")
plot(clim$date[which(clim$year>2015)],clim$temp[which(clim$year>2015)], pch=16, cex=.1, type="p",xlab="",ylab="Average Temp (F)")

## The cycle seems to be fairly annual.
## Now let's use the seasonal naive method to predict 
## the temperature of april and may
clim_train <- clim[which(clim$date<="2020-03-18"),]
clim_test <- clim[which(clim$date>"2020-03-18"),]

# Getting the predicted dates from a year ago
# meaning cycle = 365
cycle = 365*1
predicted_temp <- c()
prediction_dates <- clim_test$date - cycle

for(i in 1:length(prediction_dates)){
  pred<-clim_train$temp[which(clim_train$date == prediction_dates[i])]
  predicted_temp <- rbind(predicted_temp,pred)
}
predicted_temp<-predicted_temp[1:length(predicted_temp)]
predicted_temp
# Now let's plot the predicted temperatures against
# the observed temperatures
par(mfrow=c(1,1))
plot(clim_train$date[which(clim_train$date>"2019-03-18")],clim_train$temp[which(clim_train$date>"2019-03-18")], pch=16, cex=.5,
     xlab='Dates',ylab='Temperature in F', col='blue', xlim=as.Date(c("2019-03-18", "2020-05-18")))
points(clim_test$date,clim_test$temp,pch=16, cex=.5,col='red')
points(clim_test$date,predicted_temp,pch=16,cex=.5,col='green')
###


date<-1:length(clim_test$date)
lo <- loess(clim_test$temp~date, span=.5, degree=2,parametric = "clim_test$temp")
lo2 <- loess(predicted_temp~date, span=.5, degree=2,parametric = "predicted_temp")
plot(date,clim_test$temp,pch=16, cex=1, col="black",xlab="Days since quarantine")
df<-predict(lo,se=TRUE)
df2<-predict(lo2,se=TRUE)
lines(df$fit, col='red', lwd=2)
lines(df$fit-df$se.fit,col="red", lty=2)
lines(df$fit+df$se.fit,col="red", lty=2)

lines(df2$fit, col='green', lwd=2)
lines(df2$fit-df$se.fit,col="green", lty=2)
lines(df2$fit+df$se.fit,col="green", lty=2)
legend(2, 70, legend=c("Predicted with Seasonal Naive Method", "Fit with using real data"),
       col=c("green", "red"), lty=1, cex=0.8)

## Decomposition into parts
par(mfrow=c(2,2))
climts <- ts(clim$temp, start= c(1973,01,01), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="additive")
plot(fit_decompose)
## Decomposition into parts 
climts <- ts(clim$temp, start= c(1973,01,01), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="multiplicative")
plot(fit_decompose)
## Decomposition into parts 
climts <- ts(clim$temp, start= c(2016,03,18), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="additive")
plot(fit_decompose)
## Decomposition into parts 
climts <- ts(clim$temp, start= c(2016,03,18), end=c(2020,05,18), frequency = 365)
options(scipen=999)
fit_decompose <- decompose(climts , type="multiplicative")
plot(fit_decompose)

## Exponential Smoothing yaparken test datasinin oldugu aylar-günlere
## correspond eden bi önceki yillarin datasini alip,
## onlarin weightingi exponentially decay edecek.

fit_ses <- ses(Organic_Traffic)
summary(fit_ses)
#Plot the forecasted values
plot(fit_ses)





## How visibility changed over the years
par(mfrow=c(2,2))
plot(clim$date,clim$visib, pch=16, cex=.1, type="p",xlab="",ylab="Visibility")
plot(clim$date[which(clim$year>2000)],clim$visib[which(clim$year>2000)], pch=16, cex=.1, type="p",xlab="",ylab="Visibility")
plot(clim$date[which(clim$year>2010)],clim$visib[which(clim$year>2010)], pch=16, cex=.1, type="p",xlab="",ylab="Visibility")
plot(clim$date[which(clim$year>2015)],clim$visib[which(clim$year>2015)], pch=16, cex=.1, type="p",xlab="",ylab="Visibility")

