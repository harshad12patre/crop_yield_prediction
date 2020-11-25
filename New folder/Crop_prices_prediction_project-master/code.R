setwd('~/Data')
temp=read.csv("prices4.csv",header=TRUE)
library(timeDate)
library(timeSeries)
library(fBasics)
myData=na.omit(temp)

#summary statistics
summary(temp)
basicStats(myData[,2])
IQR(temp[,2])
range(temp[,2])

#Decomposition using decompose and stl
Y = ts(myData[,2], start=c(1997,1), freq=12)
dd_dec = decompose(Y)
dd_stl = stl(Y, s.window=13)
plot(dd_dec)
plot(dd_stl, main="Decomposition by stl")

#Holt-Winters comparing multiplicative and additive decomposition
HW_past = window(Y, end=c(2016,12))
HW_hw = HoltWinters(HW_past, seasonal = "multiplicative")
HW_pred = predict(HW1_hw, n.ahead=28)
plot(HW_hw, HW_pred,
     ylim = range(Y),
     main = "Holt-Winters (multiplicative)")
lines(Y)
legend("topleft",
       legend = c("observed","HW prediction"),
       col = 1:2,
       lty = rep(1,2),
       ncol = 1)

#Calculate the error
library(forecast)
HW_fore = forecast(HW_hw, h=28)
accuracy(HW_fore, Y)


#Transform data into time series format for ARIMA model
y2 = as.timeSeries(temp)[,2]
myData2 = ts(temp, start = c(1997,01), end = c(2016,12), freq=12)
Y2 = myData2[,2]

#Regression model
#Monthly average prices at t-1 (MAPB) and quantity (Q) are the independent variables
#Monthly average prices (MAP) is the dependent variable
FH_2vlm = lm(MAP~MAPB + Q, data=Y)
summary(FH_2vlm)

#Estimate new prices using the regression model
FH_pred = predict(FH_2vlm, interval="confidence")
YHAT = FH_pred[,1]
YHATT = ts(FH_pred[,1], start=c(1997,1), end=c(2016,12), freq=12)
save(YHATT, file="YHATT")
load("YHATT")

#ACF & PACF
par(mfrow = c(2,1))
Acf(YHATT, main="Autocorrelation Function")
Pacf(YHATT, main="Partial Autocorrelation Function")

#ARIMA
data.fit = arima(YHATT, order=c(2,1,2), seasonal=list(order=c(1,1,1)))
Box.test(data.fit$residuals)
forecast = forecast(data.fit, h=28)
par(mfrow = c(1,1))
plot(forecast,
     col=2,
     xlab="year",
     ylab="prices(NT)",
     ylim=range(Y))
lines(Y, col=1)
tsdiag(data.fit)
summary(forecast)
accuracy(forecast, Y)
