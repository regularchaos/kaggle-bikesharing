## Load Packages ##
library(Metrics) # For the Root Mean Squared Log Error (RMSLE) function
library(randomForest) # Random Forest
library(rpart)
library(ada)

source('bikeshare_functions.R')

## Load Training Data ##
trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)

trainData <- addFeatures(trainRaw)

## Prediction Algorithm
ind <- sample(c(1:nrow(trainData)),2000) # random 1/4 sample, 3/4 to train, 1/4 to test the method on
trainSubset <- trainData[-ind,]
trainTest  <- trainData[ind,]

trainCasual <- subset(trainSubset, select = -c(count,registered))
trainRegistered <- subset(trainSubset, select = -c(count,casual))

## Random Forest Settings
rfNtree <- 500
rfMtry  <- 5
rfImportance <- TRUE
set.seed(21)

casualFormula <- casual ~ hour + year + humidity + temp + atemp + workingday + weekday
registeredFormula <- registered ~ hour + year + humidity + temp + atemp + workingday + weekday


## Random Forest ##
casualFit <- randomForest(casualFormula, data = trainCasual, 
                          ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
registeredFit <- randomForest(registeredFormula, data = trainRegistered, 
                              ntree = rfNtree, mtry = rfMtry, importance = rfImportance)

## ADA ##
#casualADAFit     <- ada(casualFormula, data = trainData)
#registeredADAFit <- ada(registeredFormula, data = trainData)

## Evaluate Fit to Training Data
trainTest$casualpredrf <- predict(casualFit,trainTest)
errCasualTrain = rmsle(trainTest$casual, trainTest$casualpredrf)
errCasualTrain

trainData$registeredpredrf <- predict(registeredFit,trainData)
errRegisteredTrain = rmsle(trainData$registered, trainData$registeredpredrf)
errRegisteredTrain

trainData$countpredrf <- round(trainData$casualpredrf + trainData$registeredpredrf,0)
errCountTrain = rmsle(trainData$count, trainData$countpredrf)
errCountTrain

## Evaluate Fit to Training Data
trainData$casualpredada <- predict(casualADAFit,trainData)
errCasualTrain = rmsle(trainData$casual, trainData$casualpredada)
errCasualTrain

trainData$registeredpredada <- predict(registeredADAFit,trainData)
errRegisteredTrain = rmsle(trainData$registered, trainData$registeredpredada)
errRegisteredTrain

trainData$countpredada <- round(trainData$casualpredada + trainData$registeredpredada,0)
errCountTrain = rmsle(trainData$count, trainData$countpredada)
errCountTrain

## Random Forest ##
trainCasual <- subset(trainData, select = -c(count,registered))
trainRegistered <- subset(trainData, select = -c(count,casual))
casualFit <- randomForest(casualFormula, data = trainCasual, 
                          ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
registeredFit <- randomForest(registeredFormula, data = trainRegistered, 
                              ntree = rfNtree, mtry = rfMtry, importance = rfImportance)

#testRaw <- read.csv('test.csv', header = TRUE, stringsAsFactors = FALSE)
#testData <- addFeatures(testRaw)
#testData$casual <- predict(casualFit,testData)
#testData$registered <- predict(registeredFit,testData)
#testData$count <- round(testData$casual + testData$registered,0)
#submit <- data.frame(datetime = testRaw$datetime, count = testData$count)
#write.csv(submit,file = "bikeshare_randomForest_submission.csv", row.names = FALSE)


## Load Packages ##
library(Metrics) # For the Root Mean Squared Log Error (RMSLE) function
library(randomForest) # Random Forest
library(party)
# library(rpart)
# library(ada)
# library(gbm)

source('bikeshare_functions.R')

## Load Training Data ##
trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)

trainData <- addFeatures(trainRaw)
# 
# ind <- sample(c(1:nrow(trainData)),2000) # random 1/4 sample, 3/4 to train, 1/4 to test the method on
# trainSubset <- trainData[-ind,]
# trainTest  <- trainData[ind,]

## Random Forest Settings
rfNtree <- 500
rfMtry  <- 7
rfImportance <- TRUE
set.seed(21)

trainCasual <- subset(trainData, select = -c(count,registered))
trainRegistered <- subset(trainData, select = -c(count,casual))
# 
# casualFullFit <- randomForest(casual ~., data = trainCasual, 
#                               ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# registeredFullFit <- randomForest(registered ~., data = trainRegistered, 
#                                   ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# 
# varImpPlot(casualFullFit)
# varImpPlot(registeredFullFit)

mtrys <- tuneRF(subset(trainCasual, select = -casual), trainCasual$casual, mtryStart = 6.8, stepFactor = 1, doBest = FALSE)

## Prediction Algorithm
# trainCasual <- subset(trainSubset, select = -c(count,registered))
# trainRegistered <- subset(trainSubset, select = -c(count,casual))
# 
# casualFormula <- casual ~ hour + year + workingday + weather + weekday + humidity + month + temp
# registeredFormula <- registered ~ hour + year + atemp + humidity + weekday + temp + workingday + month
# 
# 
# ## Random Forest ##
# casualFit <- randomForest(casualFormula, data = trainCasual, 
#                           ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# registeredFit <- randomForest(registeredFormula, data = trainRegistered, 
#                               ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# 
# ## Evaluate Fit to Training Data
# trainTest$casualpredrf <- predict(casualFit,trainTest)
# errCasualTrain = rmsle(trainTest$casual, trainTest$casualpredrf)
# errCasualTrain
# plot(trainTest$casual, trainTest$casualpredrf)
# 
# trainTest$registeredpredrf <- predict(registeredFit,trainTest)
# errRegisteredTrain = rmsle(trainTest$registered, trainTest$registeredpredrf)
# errRegisteredTrain
# plot(trainTest$registered, trainTest$registeredpredrf)
# 
# trainTest$countpredrf <- round(trainTest$casualpredrf + trainTest$registeredpredrf,0)
# errCountTrain = rmsle(trainTest$count, trainTest$countpredrf)
# errCountTrain
# plot(trainTest$count, trainTest$countpredrf)

## Random Forest ##
# trainCasual <- subset(trainData, select = -c(count,registered))
# trainRegistered <- subset(trainData, select = -c(count,casual))

casualFit <- randomForest(casual ~ ., data = trainCasual, 
                          ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
registeredFit <- randomForest(registered ~ ., data = trainRegistered, 
                              ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# casualFit <- randomForest(casualFormula, data = trainCasual, 
#                          ntree = rfNtree, mtry = rfMtry, importance = rfImportance)
# registeredFit <- randomForest(registeredFormula, data = trainRegistered, 
#                              ntree = rfNtree, mtry = rfMtry, importance = rfImportance)

testRaw <- read.csv('test.csv', header = TRUE, stringsAsFactors = FALSE)
testData <- addFeatures(testRaw)
testData$casual <- predict(casualFit,testData)
testData$registered <- predict(registeredFit,testData)
testData$count <- round(testData$casual + testData$registered,0)
submit <- data.frame(datetime = testRaw$datetime, count = testData$count)
write.csv(submit,file = "bikeshare_randomForest_submission3.csv", row.names = FALSE)


