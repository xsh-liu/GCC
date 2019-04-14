library(tidyverse)
library(purrr)
library(caret)
library(mlr)

gcc <- read.csv("GCC.csv")
ncol(gcc)

#The .xlsx file's first sheet contains description of each item. 

## Check the overall renewal rate. 
mean(gcc$Renewal)

#Check if the data contains NA values. 
any(is.na.data.frame(gcc))

#Remove NAs from the set. 
gcc <- na.omit(gcc)

#Transform character values to numerical values
chaCol <- c("DwellingType", "Gender", "Marital", "ChildPresent", "Occupation","MagazineStatus","LastPaymentType","GiftDonor")
gcc <- createDummyFeatures(gcc, cols = chaCol)

#Examine the new dataframe.
##Remove one dummy variable from each category to avoid dummy traps. 
view(gcc)
gcc$DwellingType.U <- NULL
gcc$Gender.U <- NULL
gcc$Marital.U <- NULL
gcc$ChildPresent.U <- NULL
gcc$Occupation.U <- NULL
gcc$MagazineStatus.S <- NULL
gcc$LastPaymentType.0 <- NULL
gcc$GiftDonor.N <- NULL

#Remove the original columns.
gcc$DwellingType <- NULL
gcc$Gender <- NULL
gcc$Marital <- NULL
gcc$ChildPresent <- NULL
gcc$Occupation <- NULL
gcc$MagazineStatus <- NULL
gcc$LastPaymentType <- NULL
gcc$GiftDonor <- NULL


#Create train, test, and validation sets
renewal <- gcc[,2]
valiIndex <- createDataPartition(renewal, p = 0.5, times = 1, list = FALSE)
validation <- gcc[valiIndex,]
gccModelData <- gcc[-valiIndex,]

renewalTest <- gccModelData[,2]
testIndex <- createDataPartition(renewalTest, p = 0.4, times = 1, list = FALSE)
train <- gccModelData[-testIndex,]
test <- gccModelData[testIndex,]

#Remove objects no longer needed. 
rm(renewal)
rm(renewalTest)
rm(valiIndex)
rm(testIndex)
rm(gcc)
rm(gccModelData)
rm(chaCol)

#Create a dataframe of predictors
predictors <- train[,-2]
pred <- train[,2]

testPredictors <- test[, -2]
testPred <- test[,2]



