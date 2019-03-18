library(tidyverse)
library(purrr)
library(caret)
library(mlr)

gcc <- read.csv("GCC.csv")

#Transform character values to numerical values
chaCol <- c("Gender", "Marital", "ChildPresent", "Occupation","MagazineStatus","LastPaymentType","GiftDonor")
dummies <- createDummyFeatures(gcc, cols = chaCol)
gcc <- cbind(gcc, dummies)
rm(dummies)
gcc$Gender <- NULL
gcc$Marital <- NULL
gcc$ChildPresent <- NULL
gcc$Occupation <- NULL
gcc$MagazineStatus <- NULL
gcc$LastPaymentType <- NULL
gcc$GiftDonor <- NULL

#Create train, test, and validation sets
renewal <- gcc$Renewal
valiIndex <- createDataPartition(renewal, p = 0.5, times = 1, list = FALSE)
validation <- gcc[valiIndex,]
gccModelData <- gcc[-valiIndex,]

renewalTest <- gccModelData$Renewal
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
