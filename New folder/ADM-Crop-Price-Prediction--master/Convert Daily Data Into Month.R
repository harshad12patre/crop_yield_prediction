require(dplyr)
require(lubridate)
head(climate)
dat <- data.frame(day=as.Date(climate$Date,"%d/%m/%Y"),HighestTemp=climate$Highest.Temp,AvgTemp=climate$Avg.Temp,LowestTemp=climate$Lowest.Temp,DewPoint1=climate$Dew.Point...C..1,DewPoint2=climate$Dew.Point...C..2,LowestDewPoint=climate$Lowest.Dew.Point,HighestHumidity=climate$Highest.Humidity,AvgHumidity=climate$Avg.Humidity,LowestHumidity=climate$Lowest.Humidity,HighestSeaLevel=climate$Highest.Sea..Level.Pressure,AverageSeaLevel=climate$Avg.Sea.level.Pressure,LowestSeaLevel=climate$Lowest.Sea.Level.Pressure,HighestVisibility=climate$Highest.Visibility.km,AvgVisibility=climate$Avg.Visibility,LowestVisibilityKM=climate$Lowest.Visibility.KM,HighestWindSpeed=climate$Highest.Wind.Speed)
head(dat,31)

dateconversion<-dat %>% mutate(year = year(day), 
                               monthnum = month(day),
                               month = month(day, label=T)) %>%
  group_by(year, month) %>%
  arrange(year, monthnum) %>%
  select(-monthnum) %>%
  summarise( HighestTemp=mean(HighestTemp), 
             AvgTemp=mean(AvgTemp),
             LowestTemp = mean(LowestTemp), 
             DewPoint1 = mean(DewPoint1),
             DewPoint2 = mean(DewPoint2), 
             LowestDewPoint = mean(LowestDewPoint),
             HighestHumidity = mean(HighestHumidity), 
             AvgHumidity = mean(AvgHumidity),
             LowestHumidity = mean(LowestHumidity), 
             HighestSeaLevel = mean(HighestSeaLevel),
             AverageSeaLevel = mean(AverageSeaLevel), 
             LowestSeaLevel = mean(LowestSeaLevel),
             HighestVisibility = mean(HighestVisibility), 
             AvgVisibility = mean(AvgVisibility),
             LowestVisibilityKM = mean(LowestVisibilityKM),
             HighestWindSpeed=mean(HighestWindSpeed))