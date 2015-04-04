library(ggplot2) # 2D Graphics
library(plot3D)  # 3D Graphics

source('bikeshare_functions.R')

## Load Training Data ##
trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)#, nrows = 2000)#431)

trainData <- addFeatures(trainRaw)
head(trainData)

scatter3D(trainData$day, trainData$hour, trainData$casual, 
          xlab = 'Day', ylab = 'Hour', zlab = 'Casual', 
          pch = 20, phi = 30, theta = 30, bty = 'g', type = 'h', ticktype = 'detailed')

ggplot(trainData, aes(x=day, y=hour)) + 
    geom_tile(aes(fill = casual))+ 
    scale_fill_gradient(low="blue", high="red")

scatter3D(trainData$day, trainData$hour, trainData$registered, 
          xlab = 'Day', ylab = 'Hour', zlab = 'Casual', 
          pch = 20, phi = 30, theta = 30, bty = 'g', type = 'h', ticktype = 'detailed')


scatter3D(trainData$humidity, trainData$temp, trainData$casual, 
          xlab = 'Humidity', ylab = 'Temp', zlab = 'Casual', 
          pch = 20, phi = 10, theta = 10, bty = 'g', type = 'h', ticktype = 'detailed')