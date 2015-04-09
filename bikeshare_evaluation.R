## Load Packages ##
# library(rpart)
# library(ada)
# library(gbm)

source('bikeshare_functions.R')

## Load Training Data ##
trainRaw <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)

trainData <- addFeatures(trainRaw)

ind <- sort(sample(c(1:nrow(trainData)),1000)) # random sample, majority to train, rest to test the method on
trainTest  <- trainData[ind,]
trainData  <- trainData[-ind,]
output <- trainTest

trainCasual <- subset(trainData, select = -c(count,registered))
trainRegistered <- subset(trainData, select = -c(count,casual))

## Random Forest
library(randomForest)
rfNtree <- 1000
rfMtry  <- 11 #7
rfImportance <- TRUE
set.seed(21)

casualFormula <- casual ~ hour + year + humidity + workingday + temp + atemp + weekday + month
registeredFormula <- registered ~ hour + year + month + workingday + humidity + weekday + atemp + temp + season + weather

casualFit_RF <- randomForest(casualFormula, data = trainCasual, 
                          ntree = rfNtree, mtry = rfMtry) #importance = rfImportance, 
registeredFit_RF <- randomForest(registeredFormula, data = trainRegistered, 
                              ntree = rfNtree, mtry = rfMtry) #importance = rfImportance, 

## Conditional Trees
library(party)
cfNtree <- 1000
# cfMtry  <- 7 # default one works better in this case
set.seed(27)

casualFormula <- casual ~ hour + year + humidity + workingday + temp + atemp + weekday + month
registeredFormula <- registered ~ hour + year + workingday + humidity + month + temp + atemp + weekday + season + weather

casualFit_CF <- cforest(casualFormula, data = trainCasual, 
                        controls=cforest_unbiased(ntree=cfNtree))
registeredFit_CF <- cforest(registeredFormula, data = trainRegistered, 
                            controls=cforest_unbiased(ntree=cfNtree))

## Boosted
library(gbm)
gbmNtree <- 2000
gbmDepth  <- 6
set.seed(51)

casualFormula <- casual ~ hour + year + humidity + workingday + temp + atemp + weekday + month
registeredFormula <- registered ~ hour + year + workingday + humidity + month + temp + atemp + weekday

casualFit_GBM <- gbm(casualFormula,data=trainCasual[,-c(1,3,11)],
                     var.monotone=NULL,
                     distribution="gaussian",
                     n.trees=gbmNtree,
                     shrinkage=0.05,
                     interaction.depth=gbmDepth,
                     bag.fraction = 0.5,
                     train.fraction = 1,
                     n.minobsinnode = 10,
                     cv.folds = 10,
                     keep.data=FALSE,
                     verbose=TRUE)

best.iter.casual <- gbm.perf(casualFit_GBM,method="cv")
print(pretty.gbm.tree(casualFit_GBM, best.iter.casual))
summary(casualFit_GBM, n.trees=best.iter.casual)

registeredFit_GBM <- gbm(registeredFormula,data=trainRegistered[,-c(1,3,11)],
                         var.monotone=NULL,
                         distribution="gaussian",
                         n.trees=gbmNtree,
                         shrinkage=0.05,
                         interaction.depth=gbmDepth,
                         bag.fraction = 0.5,
                         train.fraction = 1,
                         n.minobsinnode = 10,
                         cv.folds = 10,
                         keep.data=FALSE,
                         verbose=TRUE)

best.iter.registered <- gbm.perf(registeredFit_GBM,method="cv")
print(pretty.gbm.tree(registeredFit_GBM, best.iter.registered))
summary(registeredFit_GBM, n.trees=best.iter.registered)

## Evaulate
library(Metrics) # For the Root Mean Squared Log Error (RMSLE) function
output$casual_RF <- predict(casualFit_RF, trainTest)
output$casual_CF <- as.numeric(predict(casualFit_CF, trainTest, OOB=TRUE, type = "response"))
output$casual_GBM <- predict(casualFit_GBM, subset(trainTest, select = -c(day,casual,registered,count, season)), best.iter.casual, type = 'response')
# err.casual_RF = rmsle(trainTest$casual, trainTest$casual_RF)
# err.casual_CF = rmsle(trainTest$casual, trainTest$casual_CF)
# # plot(trainTest$casual, trainTest$casualpredrf)
output$registered_RF <- predict(registeredFit_RF, trainTest)
output$registered_CF <- as.numeric(predict(registeredFit_CF, trainTest, OOB=TRUE, type = "response"))
output$registered_GBM <- predict(casualFit_GBM, subset(trainTest, select = -c(day,casual,registered,count, season)), best.iter.registered, type = 'response')
# err.registered_RF = rmsle(trainTest$registered, trainTest$registered_RF)
# err.registered_CF = rmsle(trainTest$registered, trainTest$registered_CF)
# # plot(trainTest$registered, trainTest$registeredpredrf)
# trainTest$count_RF <- round(trainTest$casual_RF + trainTest$registered_RF,0)
# trainTest$count_CF <- round(trainTest$casual_CF + trainTest$registered_CF,0)
# err.count_RF = rmsle(trainTest$count, trainTest$count_RF)
# err.count_CF = rmsle(trainTest$count, trainTest$count_CF)
output$count_RF <- round((exp(output$casual_RF)-1) + (exp(output$registered_RF)-1),0)
output$count_CF <- round((exp(output$casual_CF)-1) + (exp(output$registered_CF)-1),0)
output$count_GBM <- round((exp(output$casual_GBM)-1) + (exp(output$registered_GBM)-1),0)
err.count_RF = rmsle(trainTest$count, output$count_RF)
err.count_CF = rmsle(trainTest$count, output$count_CF)
err.count_GBM = rmsle(trainTest$count, output$count_GBM)

#plot(trainTest$count, trainTest$countpredrf)
library(ggplot2)
countDelta_RF <- output$count_RF - trainTest$count
countDelta_CF <- output$count_CF - trainTest$count
ggplot(trainTest, aes(x=countDelta_RF, y=countDelta_CF)) + 
    geom_point(aes(color = weekday)) +
    xlim(-300,300) +
    ylim(-300,300) +
    coord_fixed(ratio = 1)

##
# testRaw <- read.csv('test.csv', header = TRUE, stringsAsFactors = FALSE)
# testData <- addFeatures(testRaw)
# testData$casual <- predict(casualFit,testData)
# testData$registered <- predict(registeredFit,testData)
# testData$count <- round(testData$casual + testData$registered,0)
# submit <- data.frame(datetime = testRaw$datetime, count = testData$count)
# write.csv(submit,file = "bikeshare_randomForest_submission3.csv", row.names = FALSE)

