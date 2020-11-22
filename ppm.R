library(tidyverse)
library(caret)

data <- read.csv("D:/r-projects/crop_yield_prediction/ds/agricultural_price.csv")

head(data)
class(data$Price.Date)

#convert dates from int to date
data$Price.Date <- as.Date(as.character(data$Price.Date), "%d%m%Y")

class(data$Price.Date)

#remove NA dates
data$Price.Date[as.character(data$Price.Date) == 'NA'] <- NA
na_index <- which(is.na(data$Price.Date))
data_new <- data[-na_index,]

