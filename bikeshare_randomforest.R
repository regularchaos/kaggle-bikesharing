source('bikeshare_functions.R')

## Load Training Data ##
trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)

trainData <- addFeatures(trainRaw)

ind <- sort(sample(c(1:nrow(trainData)),1000)) # random sample, majority to train, rest to test the method on
trainTest  <- trainData[ind,]
trainData  <- trainData[-ind,]

trainCasual <- subset(trainData, select = -c(count,registered))
trainRegistered <- subset(trainData, select = -c(count,casual))

## Random Forest
library(randomForest)
rfNtree <- 1000
rfMtry  <- 7
rfImportance <- TRUE
set.seed(21)

casualFormula <- casual ~ hour + year + humidity + workingday + temp + atemp + weekday + month
registeredFormula <- registered ~ hour + year + month + workingday + humidity + weekday + atemp + temp + season + weather

err.count_RF <- 1

for (ii in 1:length(rfMtry)) {
    casualFit_RF <- randomForest(casualFormula, data = trainCasual, 
                                 ntree = rfNtree, mtry = rfMtry[ii]) #importance = rfImportance, 
    registeredFit_RF <- randomForest(registeredFormula, data = trainRegistered, 
                                     ntree = rfNtree, mtry = rfMtry[ii]) #importance = rfImportance, 
    
    # Evaulate
    library(Metrics) # For the Root Mean Squared Log Error (RMSLE) function
    casual_RF <- predict(casualFit_RF, trainTest)
    registered_RF <- predict(registeredFit_RF, trainTest)
    count_RF <- round((exp(casual_RF)-1) + (exp(registered_RF)-1),0)
    err.count_RF[ii] = rmsle(trainTest$count, count_RF)
}