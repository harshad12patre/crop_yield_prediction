library(knitr)
library(caret)
library(tidyverse)
library(data.table)
library(shiny)
library(tseries)
library(xts)
library(zoo)
library(quantmod)

suppressWarnings(set.seed(1, sample.kind = "Rounding"))

#read data
data <- read.csv("D:/r-projects/crop_yield_prediction/ds/agricultural_price.csv")

#convert dates from int to date
data$Price.Date <- as.Date(as.character(data$Price.Date), "%d%m%Y")
class(data$Price.Date)

#remove NA dates
data$Price.Date[as.character(data$Price.Date) == 'NA'] <- NA
na_index <- which(is.na(data$Price.Date))
data_new <- data[-na_index,]

#change colnames
data_new <- data_new %>%
  rename(Sr.No = ï..Sr..No,
         District = District.Name,
         Market = Market.Name,
         MinPrice = Min.Price..Rs..Quintal.,
         MaxPrice = Max.Price..Rs..Quintal.,
         ModalPrice = Modal.Price..Rs..Quintal.,
         Date = Price.Date) %>%
  mutate(Market = as.numeric(factor(Market))) %>%
  select(Market, MinPrice, MaxPrice, ModalPrice, Date) %>%
  arrange(Date)

# set target market
target_market <- 1 #Chandvad

#filter data according to target market
tm_data <- data_new %>%
  filter(Market == target_market)

#extract values
Modal_data <- (tm_data$ModalPrice)
Max_data <- (tm_data$MaxPrice)
Min_data <- (tm_data$MinPrice)

#make datasets
tm_df <- cbind(Max_data,Min_data,Modal_data)
tm_actual <- data.frame(tm_df)
train_price <- (tm_actual[-(nrow(tm_actual)),])
trainingoutput_price <- as.data.frame(tm_actual[-1,3])

#Column bind the data into one variable
trainingdata_price <- as.data.frame(cbind(train_price,trainingoutput_price))

#train_neural_net
net_price <- neuralnet(tm_actual[-1, 3] ~  Max_data + Min_data, data = trainingdata_price, err.fct = "sse", hidden = 15, threshold=0.001)

#testing
testdata <- as.data.frame(tm_actual[nrow(tm_actual),])
net.results <- neuralnet::compute(net_price, testdata)
Predicted_Modal_Rate<- net.results$net.result
Modal_rate<- Predicted_Modal_Rate[1,1]

#saving results
results <- data.frame(actual = testdata$Modal_data, prediction = net.results$net.result)
results