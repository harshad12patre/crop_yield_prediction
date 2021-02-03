library(knitr)
library(caret)
library(tidyverse)
library(data.table)
library(rpart)
library(randomForest)

suppressWarnings(set.seed(123, sample.kind = "Rounding"))

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
  # Market = as.numeric(factor(Market)),
  mutate( Day = mday(Date), Month = format(as.Date(Date), "%m"), Year = format(as.Date(Date), "%Y")) %>%
select(Market, MinPrice, MaxPrice, ModalPrice, Day, Month, Year)
test <- data_new[testindex,] %>%
  # Market = as.numeric(factor(Market)),   
  mutate(Day = mday(Date), Month = format(as.Date(Date), "%m"), Year = format(as.Date(Date), "%Y")) %>%
  select(Market, MinPrice, MaxPrice, ModalPrice, Day, Month, Year)

#get market names and set target market
markets <- data.frame(Market = unique(data_new$Market), Factor = 1:length(unique(data_new$Market)))




fit <- rpart(ModalPrice ~ ., data = train)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

mp_hat <- predict(fit)
mp_hat

# train %>%
#   mutate(mp_hat = predict(fit)) %>%
#   ggplot() +
#   geom_point(aes(Market, ModalPrice)) +
#   geom_step(aes(Market, mp_hat), col=2)



fitrf <- randomForest(ModalPrice ~ MinPrice, data = train)

# train %>% 
#   mutate(mp_hat = predict(fitrf)) %>% 
#   ggplot() +
#   geom_point(aes(MinPrice, ModalPrice)) +
#   geom_step(aes(MinPrice, mp_hat), col = "red")

plot(fitrf)



fitrf <- randomForest(ModalPrice ~ MaxPrice, data = train)

# train %>% 
#   mutate(mp_hat = predict(fitrf)) %>% 
#   ggplot() +
#   geom_point(aes(Market, ModalPrice)) +
#   geom_step(aes(Market, mp_hat), col = "red")

plot(fitrf)



fitrf <- randomForest(ModalPrice ~ MinPrice + MaxPrice, data = train)

# train %>% 
#   mutate(mp_hat = predict(fitrf)) %>% 
#   ggplot() +
#   geom_point(aes(Market, ModalPrice)) +
#   geom_step(aes(Market, mp_hat), col = "red")

plot(fitrf)


# fitrff <- randomForest(ModalPrice ~ Market, data = train, nodesize = 50, maxnodes = 25)
# 
# train %>% 
#   mutate(mp_hat = predict(fitrff)) %>% 
#   ggplot() +
#   geom_point(aes(Market, ModalPrice)) +
#   geom_step(aes(Market, mp_hat), col = "red")