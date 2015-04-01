# library(randomForest)
library(ggplot2)
library(Metrics)
library(randomForest)
library(formatR)

addFeatures <- function(df){
  names <- c("season","holiday","workingday","weather")
  df[,names] <- lapply(df[,names],factor)
  
  df$datetime <- as.character(df$datetime)
  df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T", tz = "EST")
  
  df$hour <- as.integer(substr(df$datetime, 12, 13))
  #df$hour <- as.factor(df$hour)
  
  df$weekday <- as.factor(weekdays(df$datetime))
  df$weekday <- factor(df$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

  df$month <- as.integer(substr(df$datetime,6,7))
  df$month <- as.factor(df$month)
  
  df$year <- as.integer(substr(df$datetime,1,4))
  df$year <- as.factor(df$year)

  df$yearmonth<-as.factor(format(df$datetime,"%Y%m",tz="EST"))
  
  df$weather[df$weather == 4] <- 3
  
  return(df)
}

trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)
trainData <- addFeatures(trainRaw)

## Year Data Visualization
YearCasual = aggregate(casual ~ hour + year, data = trainData, FUN = "mean")
ggplot(YearCasual, aes(x = hour, y = casual)) + geom_line(aes(group = year, color = year), size = 2, alpha = 0.5)
YearRegistered = aggregate(registered ~ hour + year, data = trainData, FUN = "mean")
ggplot(YearRegistered, aes(x = hour, y = registered)) + geom_line(aes(group = year, color = year), size = 2, alpha = 0.5)

## Weekday Data Visualization
WeekHourCount = aggregate(count ~ hour + weekday, data = trainData, FUN = "mean")
ggplot(WeekHourCount, aes(x=hour, y=count)) + geom_line(aes(group=weekday, color=weekday),size=2,alpha=0.5)
ggplot(WeekHourCount, aes(x=hour, y=weekday)) + geom_tile(aes(fill = count))+ scale_fill_gradient(low="blue", high="red")
WeekHourCasual = aggregate(casual ~ hour + weekday, data = trainData, FUN = "mean")
ggplot(WeekHourCasual, aes(x=hour, y=casual)) + geom_line(aes(group=weekday, color=weekday),size=2,alpha=0.5)
ggplot(WeekHourCasual, aes(x=hour, y=weekday)) + geom_tile(aes(fill = casual))+ scale_fill_gradient(low="blue", high="red")
WeekHourRegistered = aggregate(registered ~ hour + weekday, data = trainData, FUN = "mean")
ggplot(WeekHourRegistered, aes(x=hour, y=registered)) + geom_line(aes(group=weekday, color=weekday),size=2,alpha=0.5)
ggplot(WeekHourRegistered, aes(x=hour, y=weekday)) + geom_tile(aes(fill = registered))+ scale_fill_gradient(low="blue", high="red")

## Weather Data Visualization
WeatherCasual = aggregate(casual ~ hour + weather, data = trainData, FUN = "mean")
ggplot(WeatherCasual, aes(x= hour, y = casual)) + geom_line(aes(group = weather, color = weather), size = 2, alpha = 0.5)
WeatherRegistered = aggregate(registered ~ hour + weather , data = trainData, FUN = "mean")
ggplot(WeatherRegistered, aes(x= hour, y = registered)) + geom_line(aes(group = weather, color = weather), size = 2, alpha = 0.5)

## Season Data Visualization
SeasonCasual = aggregate(casual ~ hour + season, data = trainData, FUN = "mean")
ggplot(SeasonCasual, aes(x= hour, y = casual)) + geom_line(aes(group = season, color = season), size = 2, alpha = 0.5)
SeasonRegistered = aggregate(registered ~ hour + season , data = trainData, FUN = "mean")
ggplot(SeasonRegistered, aes(x= hour, y = registered)) + geom_line(aes(group = season, color = season), size = 2, alpha = 0.5)

## Holiday Data Visualization
HolidayCasual = aggregate(casual ~ hour + holiday, data = trainData, FUN = "mean")
ggplot(HolidayCasual, aes(x = hour, y = casual)) + geom_line(aes(group = holiday, color = holiday), size = 2, alpha = 0.5)
HolidayRegistered = aggregate(registered ~ hour + holiday, data = trainData, FUN = "mean")
ggplot(HolidayRegistered, aes(x = hour, y = registered)) + geom_line(aes(group = holiday, color = holiday), size = 2, alpha = 0.5)


## Temperature Visulization
ggplot(trainData, aes(x = temp, y = casual)) + geom_point()
ggplot(trainData, aes(x = temp, y = registered)) + geom_point()
ggplot(trainData, aes(x = atemp, y = casual)) + geom_point()
ggplot(trainData, aes(x = atemp, y = registered)) + geom_point()

## Prediction Algorithm
ind <- sample(c(1:nrow(trainData)),2500)
trainTrain <- trainData[-ind,]
trainTrue  <- trainData[ind,]

trainTrain <- subset(trainTrain, select = -c(datetime,count,registered))

bsNtree <- 500
bsMtry  <- 5
bsImportance <- TRUE
set.seed(21)

#casualFormula <- casual ~ hour + weekday + weather + season
#registeredFormula <- registered ~ hour + weekday + weather + season

## Evaluate predicions
#err = rmsle(actual, prediction)

####create output file from dataset test with predictions
submit <- data.frame (datetime = testData$datetime, count = testData$count)
write.csv(submit, file = "bikeshare_randomForest_submission.csv", row.names=FALSE)
