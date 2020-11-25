# Crop Prices Prediction

This project aims to predict the price of cabbage in Taiwan's agricultural products market. The monthly price data between 01/1997 - 04/2019 are collected. The data between 01/1997-12/2016 are regarded as training data and the rest are regarded as testing data. Holt-Winters and ARIMA model are built for time series analysis.

## Objective
- Predict the price of cabbage using Holt-Winters and ARIMA model
- Decompose the price data into trend, seasonal and remainder/random
- Evaluate the result base on root-mean-square error (RMSE)

## Background

**Language:** R  
**Libraries:** timeSeries, fBasics, forecast, tseries

## Decomposition
- Transform the price data into time series format
- Decompose using *stl*
- s.window is set to 13 according to https://otexts.com/fpp2/stl.html

![alt text](https://github.com/auweiting/Crop_prices_prediction_project/blob/master/stl_decomposition.png "Decomposition by stl")


## Modelling

**1. Holt-Winters**  
Since the the variation in the trend-cycle appears to be proportional to the level of the time series, we will use multiplicative decomposition here (https://otexts.com/fpp2/components.html).
The prediction along the training set perfoms quite well. However, the predicition along the testing set seems become inaccurate especially the bottom.
The RMSE for the testing set is **14.284**. We need a more robust model.

![alt text](https://github.com/auweiting/Crop_prices_prediction_project/blob/master/HW.png "Holt-Winters Prediction (multiplicative)")


**2. ARIMA**  
We use autocorrelation function (ACF) and partial autocorrelation function (PACF) to observe the correlation between observations of the time series separated by k time units.

![alt text](https://github.com/auweiting/Crop_prices_prediction_project/blob/master/acf_pacf.png "ACF & PACF")

ARIMA(2,1,2)(1,1,1) is chosen as the optimal model after several attempts on trying different parameters. The Akaike Information Critera (AIC) score of this model is **1184.57**, while the RMSE is **6.604**.


![alt text](https://github.com/auweiting/Crop_prices_prediction_project/blob/master/forecast.png "ARIMA")

