## Feature Engineering Function ##
addFeatures <- function(df){
    names <- c("season","holiday","workingday","weather")
    df[,names] <- lapply(df[,names],factor)
    
    df$datetime <- as.POSIXct(df$datetime)
    df$datetime <- strptime(df$datetime, format="%Y-%m-%d %T", tz = "EST")
    df$date     <- as.POSIXct(format(df$datetime,"%Y-%m-%d",tz="EST"))
    #df$time     <- format(df$datetime,"%T",tz="EST")
    
    df$hour    <- as.integer(substr(df$datetime, 12, 13))
    
    df$weekday <- as.factor(weekdays(df$datetime))
    df$weekday <- factor(df$weekday, 
                         levels = c("Monday",
                                    "Tuesday",
                                    "Wednesday",
                                    "Thursday",
                                    "Friday",
                                    "Saturday",
                                    "Sunday"))
    
    df$month <- as.integer(substr(df$datetime,6,7))
    
    df$day  <- as.integer(strftime(df$date, format="%j"))
    df$week <- as.integer(strftime(df$date, format="%W"))
    
    df$year <- as.integer(substr(df$datetime,1,4))
    df$year <- as.factor(df$year)
    
    #df$yearmonth <- as.integer(format(df$datetime,"%Y%m",tz="EST"))
    
    df$weather[df$weather == 4] <- 3
    
    ## Few 0% humidity hours shouldnt be 0% (its Washington DC) 
    # and looking at the data, they are all near high humidity cases, setting them == 100%
    df$humidity[df$humidity == 0] <- 100
    
    
    df <- subset(df, select = -c(datetime,windspeed,date))
    
    return(df)
}