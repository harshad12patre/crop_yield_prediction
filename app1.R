
setwd("D:/r-projects/crop_yield_prediction")

##########################################


suppressWarnings(set.seed(1234, sample.kind = "Rounding"))
start_date = "2013-01-01"
end_date = as.character(Sys.Date())

############################

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
tm_data <- rbind(tm_test, tm_train)

############################


GSPC_data <- as.data.frame(data_new)
#write.table(GSPC_data,file = "STOCKDATA.csv",sep="," ,row.names =TRUE)
date_range <- as.data.frame(GSPC_data$Date)
normal <- function(data){
  (data - min(data,na.rm = TRUE))/(max(data,na.rm=TRUE) - min(data,na.rm=TRUE))
}

# Open_data <- (GSPC_data$XXX)
Close_data <- (GSPC_data$ModalPrice)
High_data <- (GSPC_data$MaxPrice)
Low_data <- (GSPC_data$MinPrice)
# Volume_data <- (GSPC_data$Volume)
# AdjClose_data <- (GSPC_data$AdjClose)

GSPC_df <- cbind(High_data,Low_data,Close_data)

GSPC_Actual <- data.frame(GSPC_df)
train_stock <- (GSPC_Actual[-(nrow(GSPC_Actual)),])

trainingoutput_stock <- as.data.frame(GSPC_Actual[-1,3])


#Column bind the data into one variable
trainingdata_stock <- as.data.frame(cbind(train_stock,trainingoutput_stock))

net_stock <- neuralnet(GSPC_Actual[-1,3] ~  High_data + Low_data, data = trainingdata_stock, err.fct = "sse", hidden=13, threshold=0.001)

testdata <- as.data.frame(GSPC_Actual[nrow(GSPC_Actual),])
net.results <- neuralnet::compute(net_stock, testdata)
Predicted_Close_Rate<- net.results$net.result
Close_rate<- Predicted_Close_Rate[1,1]

rm(list = ls())

#install.packages("nnet")

library (tseries)
library (xts)
library (zoo)
library (quantmod)
library (neuralnet)

code_stock <- 2




##########################################


script_list <- list("start.R","ppm3.R")

for (loop_variable in 1:length(script_list)){
  try(source(script_list[[loop_variable]]))
}


library(shiny)
library (tseries)
library (xts)
library (zoo)
library (quantmod)

ui <- fluidPage(
  titlePanel(title = "Stock Prediction Model"),
  
  sidebarLayout(
    sidebarPanel( helpText("Select a stock to examine. 
                           Information will be collected from yahoo finance."),
                  
                  
                  dateRangeInput("dates", 
                                 "Date range",
                                 start = "2013-01-01", 
                                 end = as.character(Sys.Date())),
                  
                  br(),
                  br(),
                  
                  actionButton(inputId = "go",
                               label = "Predict")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput(outputId = "text_form")
    )
    
  )
)



server <- function(input, output){
  
  output$plot <- renderPlot({
    data <- tm_data
    
    chartSeries(data, theme = chartTheme("white"), 
                type = "line", TA = NULL)
  })
  
  data1 <- eventReactive(input$go, Close_rate)
  output$text_form <- renderText({paste("The Closing Rate for ",Sys.Date(),"will be around: ",data1())})
}

shinyApp(ui = ui, server = server)

