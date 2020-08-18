library(stringr)
library(tidyverse)

data <- read.csv("C:/Users/harsh/Downloads/data.csv")
data <- str_split(data[,1], "\t", simplify = T)