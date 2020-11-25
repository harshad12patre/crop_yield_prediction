library(knitr)
library(caret)
library(tidyverse)
library(data.table)

data <- read.csv("D:/r-projects/crop_yield_prediction/ds/agricultural_price.csv")
suppressWarnings(set.seed(123, sample.kind = "Rounding"))

head(data)
class(data$Price.Date)

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
train <- data_new[-testindex,]
test <- data_new[testindex,]

#get market names and set target market
markets <- data.frame(Market = unique(data_new$Market), Factor = 1:length(unique(data_new$Market)))
train$Market <- as.numeric(factor(train$Market))

#calculate market means and get target market mean
mean_market <- train %>%
  group_by(Market) %>%
  summarise(MeanMP = mean(ModalPrice))
target_market <- 2

#target_market = tm
tm_mean <- mean_market$MeanMP[mean_market$Market==target_market]

#filter test by tm
tm_test <- test %>%
  filter(Market == as.character(target_market))

#calculate rmse of avg model and add to results
rmse_avg <- RMSE(tm_test$ModalPrice, tm_mean)
rmse_results <- data_frame(method = "Just the average (tm_test)", RMSE = rmse_avg)

tm_test_srno <- train %>%
  group_by(Market, Sr.No) %>%
  summarize(bi = mean(ModalPrice - tm_mean), .groups = 'drop')

pred_bi <- tm_mean + tm_test_srno$bi
pred_bi <- mean_tt + test %>%
  left_join(tm_test_srno, by = "Sr.No") %>%
  .$bi

# rmse_movie <- RMSE(pred_bi, tm_test$ModalPrice)