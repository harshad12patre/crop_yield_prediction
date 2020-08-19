library(ggplot2)
library(dplyr)
library(tidyverse)

data <- read.csv("./ds/rainfall_monthwise(2010-2016).csv")

data <- data %>%
  mutate(rain = as.integer(rain)) 

sum(is.na(data$rain))

data %>%
  group_by(month) %>%
  summarize(meanm = mean(rain))

nskdat <- data %>% 
  filter(district == "Nasik") %>%
  group_by(month) %>%
  summarize(monthm = mean(rain))

nskdat <- nskdat %>%
  mutate(month)

nskdat %>%
  ggplot(aes(monthm, month), color = Blue) + geom_line()