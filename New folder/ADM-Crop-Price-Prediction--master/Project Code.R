library(factoextra)
require(dplyr)
require(lubridate)
require(zoo)
require(smooth)
require(Mcomp)
require(pracma)
require(ggplot2)
library(scales)
require(TTR)
library(reshape)
require(tidyverse)
require(tseries)
library(vars)
require(lmtest)
library(urca)
library(vars)
require(bindrcpp)
require(xts)



climate<-read.csv("C:/Users/x17157170/Desktop/climate.csv")
crop <- read.csv("C:/Users/x17157170/Desktop/demo2.csv")




str(climate)
summary(climate)
write.csv(file="climate.csv", climate)


climate_numeric <- (climate[6:20])
crop <- crop[c(4,10,17:19)]

climate_scale<-scale(climate_numeric)
climate_covaria<-cov(climate_scale)
climate_princomp<- princomp(climate_covaria)
fviz_eig(climate_princomp)



dat <- data.frame(day=as.Date(climate$Date,"%d/%m/%Y"),HighestTemp=climate$Highest.Temp,AvgTemp=climate$Avg.Temp,LowestTemp=climate$Lowest.Temp,DewPoint1=climate$Dew.Point...C..1,DewPoint2=climate$Dew.Point...C..2,LowestDewPoint=climate$Lowest.Dew.Point,HighestHumidity=climate$Highest.Humidity,AvgHumidity=climate$Avg.Humidity,LowestHumidity=climate$Lowest.Humidity,HighestSeaLevel=climate$Highest.Sea..Level.Pressure,AverageSeaLevel=climate$Avg.Sea.level.Pressure,LowestSeaLevel=climate$Lowest.Sea.Level.Pressure,HighestVisibility=climate$Highest.Visibility.km,AvgVisibility=climate$Avg.Visibility,LowestVisibilityKM=climate$Lowest.Visibility.KM,HighestWindSpeed=climate$Highest.Wind.Speed)
head(dat,31)

dateconversion <- dat %>% mutate(year = year(day), 
                               monthnum = month(day),
                               month = month(day, label=T)) %>%
  group_by(year, month) %>%
  arrange(year, monthnum) %>%
   summarise( HighestTemp=mean(HighestTemp), 
             AvgTemp=mean(AvgTemp),
             LowestTemp = mean(LowestTemp), 
             DewPoint1 = mean(DewPoint1),
             DewPoint2 = mean(DewPoint2), 
             LowestDewPoint = mean(LowestDewPoint),
             HighestHumidity = mean(HighestHumidity), 
             AvgHumidity = mean(AvgHumidity),
             LowestHumidity = mean(LowestHumidity), 
             HighestSeaLevel = mean(HighestSeaLevel),
             AverageSeaLevel = mean(AverageSeaLevel), 
             LowestSeaLevel = mean(LowestSeaLevel),
             HighestVisibility = mean(HighestVisibility), 
             AvgVisibility = mean(AvgVisibility),
             LowestVisibilityKM = mean(LowestVisibilityKM),
             HighestWindSpeed=mean(HighestWindSpeed))




write.csv(file="dateconversion.csv",dateconversion)
ricedata <- crop[ which(crop$Crop == 'Rice'),]
write.csv(file="ricedata.csv",ricedata)
wheatdata <- crop[ which(crop$Crop == 'Wheat'),]
Sugardata <- crop[ which(crop$Crop == 'Sugar'),]
oilmustardedata <- crop[ which(crop$Crop == 'Oil (mustard)'),]



datee <- as.yearmon(paste(ricedata$Year, ricedata$Month), "%Y %m")
str(ricedata<-cbind(ricedata,datee))
ricedata<-ricedata[,-7]
for(i in 1:nrow(ricedata)){
  if(nchar(ricedata$Month[i])==1){
    ricedata$Year[i] <- paste(ricedata$Year[i],ricedata$Month[i],sep="-0")
  }else{
    ricedata$Year[i] <- paste(ricedata$Year[i],ricedata$Month[i],sep="-")
  }
}
cropdateconversion <- data.frame(day=as.yearmon(ricedata$Year,"%Y-%m"),a=ricedata$Price)


ricedata <-  cropdateconversion %>% mutate(year = year(day), 
                                  monthnum = month(day),
                                  month = month(day, label=T)) %>%
                                  group_by(year, month) %>%
                                  arrange(year, monthnum) %>%
                                  
                                  summarise(a = mean(a))


tsPerformance <- function(actual, predicted) {
  ME = mean(actual - predicted, na.rm = TRUE)
  MAE = mean(abs(actual - predicted), na.rm = TRUE)
  MSE = mean((actual - predicted)^2, na.rm = TRUE)
  MAPE = mean(abs((actual - predicted)/actual), na.rm = TRUE)
  return(data.frame(ME, MAE, MSE, MAPE))
}

movingavs<-movavg(ricedata$a,18,type="s")
movingavghightemppredictions<-predict(movingavs,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictions$mean))

movingavt<-movavg(ricedata$a,18,type="t")
movingavghightemppredictiont<-predict(movingavt,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictiont$mean))

movingavw<-movavg(ricedata$a,18,type="w")
movingavghightemppredictionw<-predict(movingavw,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictionw$mean))

movingavm<-movavg(ricedata$a,18,type="m")
movingavghightemppredictionm<-predict(movingavm,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictionm$mean))

movingave<-movavg(ricedata$a,18,type="e")
movingavghightemppredictione<-predict(movingave,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictione$mean))

movingavr<-movavg(ricedata$a,18,type="r")
movingavghightemppredictionr<-predict(movingavr,h=3012)
(maP <- tsPerformance(ricedata$a, movingavghightemppredictionr$mean))

a<-SMA(ricedata$a,n=10)
(a <- c(NA, a[1: length(a) -1]))
avgclimateprediction<-predict(a,h=nrow(ricedata))
(maP <- tsPerformance(ricedata$a, avgclimateprediction$mean))



write.csv(file="ricedata1.csv",ricedata)
avgclimateprediction
ricedata_ts <- ts(ricedata$a,frequency=12,start = 1994,end =2017)
plot(ricedata_ts)
avgtemp_ts<-ts(dateconversion$AvgTemp,frequency=12,start = 2010,end =c(2018,1))
plot(avgtemp_ts)
devpoint_ts<-ts(dateconversion$DewPoint2,frequency=12,start = 2010,end =c(2018,1))
plot(devpoint_ts)
avghumidityt_ts<-ts(dateconversion$AvgHumidity,frequency=12,start = 2010,end =c(2018,1))
plot(avghumidityt_ts)
avgsealevel_ts<-ts(dateconversion$AverageSeaLevel,frequency=12,start = 2010,end =c(2018,1))
plot(avgsealevel_ts)
avgvisibility_ts<-ts(dateconversion$AvgVisibility,frequency=12,start = 2010,end =c(2018,1))
plot(avgvisibility_ts)
prediction<-rep(mean(ricedata_ts),length(ricedata))




class(ricedata)

ricedata
plot(ricedata$a,type="l")
abline(reg = lm(ricedata$a~time(ricedata$month)))
boxplot(climate$Avg.Temp~cycle(as.Date(climate$Date)))
class(ts(ricedata))
plot(log(ricedata$a),type="l")  # Make variance equal 
ndiffs(ricedata$a)
plot(diff(diff(log(ricedata$a))),type = "l") # Make mean constant


#ARIMA Autoregression Integrated Moving Average  (p,d,q)
# Autocorrelation Function graph


acf(ricedata$a)
acf(diff(diff(log(ricedata$a)))) #determines the value of q = 4
pacf(diff(diff(log(ricedata$a)) )) #determine value of p=3
# value of will be depend on how many times you have differentiated d=1

fit<- arima(log(ricedata[,3]),c(1,2,4),optim.method = "BFGS",method = c("CSS-ML", "ML", "CSS"))
predarimaSANN<-predict(fit,n.ahead = 279)
pred1<-2.718^predarimaSANN$pred



fit1<-auto.arima(ricedata$a,max.p = 4,max.q = 2,max.d = 2)
pred2<-forecast(fit1,h=279)
plot(pred2)
(maP <- tsPerformance(ricedata$a, pred2))


ts.plot(ricedata$a,2.718^predarimaSANN$pred)
plot(ricedata$a,type="l")
plot(pred1,type="l")
(maP <- tsPerformance(ricedata$a, pred1))

# Combine Both dataset by taking them on same rows numbers 

climatedata<- dateconversion[-c(95:99),]
clricedatacut<- ricedata[-c(1:185),]

final_dataset<- cbind(climatedata,clricedatacut)
write.csv(file="final_dataset.csv",final_dataset)
final<- final_dataset[,c(1:2,4,7,10,13,16,18,21)]
final<-read.csv("final_dataset.csv",stringsAsFactors = T)
str(final_dataset)



#Vector Autoregressive Model with multivariate 

ndiffs(final_dataset$HighestTemp) #0
ndiffs(final_dataset$AvgTemp) #0
ndiffs(final_dataset$LowestTemp) #0
ndiffs(final_dataset$DewPoint1) #0
ndiffs(final_dataset$DewPoint2) #0
ndiffs(final_dataset$LowestDewPoint) #0
ndiffs(final_dataset$HighestHumidity) #0
ndiffs(final_dataset$AvgHumidity) #0
ndiffs(final_dataset$LowestHumidity) #0
ndiffs(final_dataset$HighestSeaLevel) #0
ndiffs(final_dataset$AverageSeaLevel) #0
ndiffs(final_dataset$LowestSeaLevel) #0
ndiffs(final_dataset$HighestVisibility) #0
ndiffs(final_dataset$AvgVisibility) #1
ndiffs(final_dataset$LowestVisibilityKM) #1
ndiffs(final_dataset$HighestWindSpeed) #0

final_dataset$AvgVisibility<- diff(final_dataset$AvgVisibility)
final_dataset$LowestVisibilityKM<-diff(final_dataset$LowestVisibilityKM)

grangertest(final_dataset$a~final_dataset$HighestTemp)      # P value = 0.4857
grangertest(final_dataset$a~final_dataset$AvgTemp)          # P value = 0.4735
grangertest(final_dataset$a~final_dataset$LowestTemp)       # P value = 0.3156
grangertest(final_dataset$a~final_dataset$DewPoint1)        # P value = 0.07444
grangertest(final_dataset$a~final_dataset$DewPoint2)        # P value = 0.066
grangertest(final_dataset$a~final_dataset$LowestDewPoint)   # P value = 0.03889
grangertest(final_dataset$a~final_dataset$HighestHumidity)  # P value = 0.05993
grangertest(final_dataset$a~final_dataset$AvgHumidity)      # P value = 0.04944
grangertest(final_dataset$a~final_dataset$LowestHumidity)   # P value = 0.06037
grangertest(final_dataset$a~final_dataset$HighestSeaLevel)  # P value = 0.008063
grangertest(final_dataset$a~final_dataset$AverageSeaLevel)  # P value = 0.006814
grangertest(final_dataset$a~final_dataset$LowestSeaLevel)   # P value = 0.00948
grangertest(final_dataset$a~final_dataset$HighestVisibility)# P value = 0.3059
grangertest(final_dataset$a~final_dataset$AvgVisibility)    # P value = 0.2278
grangertest(final_dataset$a~final_dataset$LowestVisibilityKM)# P value = 0.5731
grangertest(final_dataset$a~final_dataset$HighestWindSpeed)  # P value = 0.05839




final_datasetvar<-final_dataset[c(6:14,18,21)]
final_datasetvar
firsth
firsth<-head(final_datasetvar,50)
fvarh<-VAR(firsth,p=3,type = c("const"))
vardemo<-predict(fvarh,h=50)

priceaaa<-getElement(vardemo,"fcst")
priceaaa1<-getElement(priceaaa,"a")
ssasa<-data.frame(priceaaa1)
priceaaabbbbb<-getElement(ssasa,"fcst")
(priceaaabbbbb - final_datasetvar[51:94,11])



fvar<-VAR(final_datasetvar,p=2,type="const")
predict(fvar,h=3)
accuracy(fvar$varresult[[11]])
summary(fvar)
iiirf<-irf(fvar,impulse = NULL,n.ahead = 20)
require(irf)
plot(iiirf)
roots(fvar)

VARselect(final_datasetvar,lag.max = 6,type="const")$select
coninst<-ca.jo(data.frame(final_datasetvar), type="trace", K=2, ecdet="none", spec="longrun")
summary(coninst)
n<- 1.0000*p-8.9382982*q+14.8736521*r
adf.test(n)
coninst
