library(dplyr)
library(tidyverse)

data <- read.csv("./ds/BigDatafin.csv")

datnew <- data %>%
  filter(district == "Nasik") %>%
  select(district, month, year, rain) %>%
  group_by(year, month)