library(knitr)
library(caret)
library(tidyverse)
library(data.table)

suppressWarnings(set.seed(123, sample.kind = "Rounding"))

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
         Date = Price.Date)

#test-train
testindex <- createDataPartition(data_new$ModalPrice, times = 1, p = 0.2, list = FALSE)
train <- data_new[-testindex,] %>%
  mutate(Market = as.numeric(factor(Market)), Day = mday(Date), Month = format(as.Date(Date), "%m"), Year = format(as.Date(Date), "%Y")) %>%
  select(Market, MinPrice, MaxPrice, ModalPrice, Day, Month, Year)
test <- data_new[testindex,] %>%
  mutate(Market = as.numeric(factor(Market)), Day = mday(Date), Month = format(as.Date(Date), "%m"), Year = format(as.Date(Date), "%Y")) %>%
  select(Market, MinPrice, MaxPrice, ModalPrice, Day, Month, Year)

#get market names calculate market means
mean_market <- train %>%
  group_by(Market) %>%
  summarise(MeanMP = mean(ModalPrice)) %>%
  mutate(Market_Name = unique(data_new$Market))

# set target market
target_market <- 2 #Devala

#filter train and test according to target market
tm_test <- test %>%
  filter(Market == target_market)
tm_train <- train %>%
  filter(Market == target_market)

# calculate market mean (target_market = tm) and rsquared loss
tm_mean <- mean_market$MeanMP[mean_market$Market==target_market]
mean((tm_mean - tm_test$ModalPrice))

#linear model for min price
fit_lm <- lm(ModalPrice ~ MinPrice, data = tm_train)
fit_lm$coef
yhat_lm <- predict(fit_lm, tm_test)
mean((yhat_lm - tm_test$ModalPrice))

#linear model for max price
fit_lm <- lm(ModalPrice ~ MaxPrice, data = tm_train)
fit_lm$coef
yhat_lm <- predict(fit_lm, tm_test)
mean((yhat_lm - tm_test$ModalPrice))

#linear model for max price and min price
fit_lm <- lm(ModalPrice ~ MinPrice + MaxPrice, data = tm_train)
fit_lm$coef
yhat_lm <- predict(fit_lm, tm_test)
mean((yhat_lm - tm_test$ModalPrice))
