#Using Principal Component Analysis to shrink down the number of predictors

pca <- prcomp(predictors, center = TRUE, scale. = FALSE)
pcaTest <- prcomp(testPredictors, center = TRUE, scale. = FALSE)

summary(pca)

#Here we can see that the first 2 components capture 100% of the variability

axes <- predict(pca, newdata = train)
axesTest <- predict(pcaTest, newdata = test)

pcaData <- cbind(train, axes)
pcaTestData <- cbind(test, axesTest)

pcaPredictors <- pcaData[, -2]
pcaTestPredictors <- pcaTestData[, -2]

#K-Neighbors Test
control <- trainControl(method = "cv", number = 2, p = .9)
pred <- as.factor(pcaData[, 2])
PCAtrain_knn <- caret::train(pcaPredictors, pred,
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,2,3)),
                   trControl = control)
PCAtrain_knn$bestTune
PCAfit_knn <- knn3(pcaPredictors, pred, k = PCAtrain_knn$bestTune)
PCAy_hat_knn <- predict(PCAfit_knn,
                     pcaTestPredictors,
                     type = "class")
testPred <- tinyTest[,2]
PCAcm_knn <- confusionMatrix(as.factor(PCAy_hat_knn), as.factor(testPred))
RMSE(as.numeric(testPred), as.numeric(PCAy_hat_knn))

#Tree 5-fold cross validation
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(2,5), predFixed = c(5,15,20))
pred <- tinyTrain[, 2]

train_rf <- caret::train(predictors,
                  pred,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid, 
                  nSamp = 5000)
train_rf$bestTune

fit_rf <- Rborist(predictors, pred, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)
testPredictors <- within(tinyTest, rm(rating))
testPred <- tinyTest[,2]
pred_rf <- predict(fit_rf, testPredictors)
y_hat_rf <- factor(levels(pred)[predict(fit_rf, testPredictors)$yPred])
testPred <- factor(testPred)
cm <- confusionMatrix(y_hat_rf, testPred)
cm$overall["Accuracy"]
RMSE(testPred, c(y_hat_rf))

#This code somehow crashes R Studio. 