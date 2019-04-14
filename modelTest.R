#logistic regression
fit_glm <- glm(Renewal ~., data = train, family = "binomial")
pred_glm <- predict(fit_glm, test) 
y_hat_glm <- ifelse(pred_glm >= 0.5,yes =  1, no = 0) 

cm_glm <- confusionMatrix(as.factor(y_hat_glm), as.factor(testPred))
cm_glm$overall["Accuracy"]
rmse_glm <- RMSE(as.numeric(testPred), as.numeric(y_hat_glm))
cm_glm
rmse_glm

#K-Neighbors Test
control <- trainControl(method = "cv", number = 5, p = .9)
pred <- as.factor(pred)
train_knn <- caret::train(predictors, pred,
                   method = "knn",
                   tuneGrid = data.frame(k = seq(10:30)),
                   trControl = control)
train_knn$bestTune
pred <- as.factor(pred)
fit_knn <- knn3(predictors, pred, k = train_knn$bestTune)
y_hat_knn <- predict(fit_knn,
                     testPredictors,
                     type = "class")
cm_knn <- confusionMatrix(y_hat_knn, factor(testPred))
cm_knn$overall["Accuracy"]
rmse_knn <- RMSE(as.numeric(testPred), as.numeric(y_hat_knn))
cm_knn
rmse_knn


#Here we can see that the K nearest neighbors model simply predicts that no one would renew due to prevelance. 
## Now, let's try the tree function. 

#Tree 5-fold cross validation with random forest. 
library(randomForest)
control <- trainControl(method = "cv", number = 5, p = 0.6)
grid <- expand.grid(mtry = 10)
pred <- as.factor(pred)
train_rf <- caret::train(predictors,
                         pred,
                         method = "cforest",
                         trControl = control,
                         tuneGrid = grid)
train_rf$bestTune

fit_rf <- randomForest(predictors, pred, 
                       minNode = train_rf$bestTune$minNode,
                       predFixed = train_rf$bestTune$predFixed)
pred_rf <- predict(fit_rf, testPredictors)
y_hat_rf <- factor(levels(pred)[predict(fit_rf, testPredictors)$yPred])
testPred <- factor(testPred)
cm_rf <- confusionMatrix(y_hat_rf, testPred)
cm_rf$overall["Accuracy"]
rmse_rf <- RMSE(testPred, c(y_hat_rf))
cm_rf
rmse_rf
imp_rf <- importance(fit_rf)
imp_rf

#The Rborist codes 
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.6)
grid <- expand.grid(minNode = c(2,5), predFixed = c(15,20,25,30,35))
pred <- as.factor(pred)
train_rborist <- caret::train(predictors,
                         pred,
                         method = "Rborist",
                         trControl = control,
                         tuneGrid = grid)
train_rborist$bestTune

fit_rborist <- Rborist(predictors, pred, 
                  minNode = train_rborist$bestTune$minNode,
                  predFixed = train_rborist$bestTune$predFixed)
pred_rborist <- predict(fit_rborist, testPredictors)
y_hat_rborist <- as.factor(levels(pred)[predict(fit_rborist, testPredictors)$yPred])
testPred <- as.factor(testPred)
cm_rborist <- confusionMatrix(y_hat_rborist, testPred)
cm_rborist$overall["Accuracy"]
rmse_rborist <- RMSE(as.numeric(testPred), as.numeric(y_hat_rborist))
cm_rborist
rmse_rborist


#Using the rpart function. 
library(rpart)
fit_tree <- rpart(Renewal ~., data = train, method = "class")
pred_tree <- predict(fit_tree, test, type = "class")
y_hat_tree <- as.factor(pred_tree)
cm_tree <- confusionMatrix(y_hat_tree, as.factor(testPred))
cm_tree$overall["Accuracy"]
rmse_tree <- RMSE(as.numeric(testPred), as.numeric(y_hat_tree))
cm_tree
rmse_tree

#Validating all the valid models with the validation set for the final evaluation. 
##Create data frame for predictors and actual values. 
valiPredictors <- validation[, -2]
valiPred <- validation[,2]


#Logistics
pred_glm_vali <- predict(fit_glm, validation) 
y_hat_glm_vali <- ifelse(pred_glm_vali >= 0.5,yes =  1, no = 0) 
cm_glm_vali <- confusionMatrix(as.factor(y_hat_glm_vali), as.factor(valiPred))
rmse_glm_vali <- RMSE(as.numeric(valiPred), as.numeric(y_hat_glm_vali))

#Random Forest
pred_rborist_vali <- predict(fit_rborist, valiPredictors)
y_hat_rborist_vali <- as.factor(levels(pred)[predict(fit_rborist, valiPredictors)$yPred])
valiPred <- as.factor(valiPred)
cm_rborist_vali <- confusionMatrix(y_hat_rborist_vali, valiPred)
rmse_rborist_vali <- RMSE(as.numeric(valiPred), as.numeric(y_hat_rborist_vali))

#Rpart
pred_tree_vali <- predict(fit_tree, validation, type = "class")
y_hat_tree_vali <- as.factor(pred_tree_vali)
cm_tree_vali <- confusionMatrix(y_hat_tree_vali, as.factor(valiPred))
rmse_tree_vali <- RMSE(as.numeric(valiPred), as.numeric(y_hat_tree_vali))

#Check the sensitivity and RMSE of results.
names <- c("glm", "Rborist", "rpart")

sensitivities <- c(cm_glm_vali$byClass["Sensitivity"], 
                   cm_rborist_vali$byClass["Sensitivity"],
                   cm_tree_vali$byClass["Sensitivity"])

rmses <- c(rmse_glm_vali,
           rmse_rborist_vali,
           rmse_tree_vali)

result <- data.frame("Name" = names,
                     "Sensitivity" = sensitivities,
                     "RMSE" = rmses)
result
