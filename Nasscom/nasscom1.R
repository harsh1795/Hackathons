# Author: Black_viper
## changing directory
initial.dir=getwd()
setwd(dir = "/home/mohit/Desktop/Nasscom")
library(rpart)
library(randomForest)
## loading datasets
library(xlsx)
data <- read.xlsx("/home/mohit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 2, rowIndex = 6:1395, colIndex = 1:62)
View(data)
attach(data)
## cleaning data
miss_data <- lapply(data, function(x){sum(is.na(x))})
names(data)
table(data$Lung_Cancer)/nrow(data)
## checking classes
cols = c(2,3,7,9,11,13,15,16,18,19,20,41,42,43,45,47,48,49,50,51,52,53)
data[,cols] = lapply(data[,cols],function(x){as.factor(x)})
class_data = lapply(data, function(x){class(x)})
class_data
## train and validation data
sample.ind = sample(2,nrow(data),replace = T,prob = c(0.6,0.4))
train_data = data[sample.ind==1,]
test_data = data[sample.ind==2,]
nrow(train_data)
nrow(test_data)
table(train_data$Lung_Cancer)/nrow(train_data)
table(test_data$Lung_Cancer)/nrow(test_data)
## Running Random forest
fit = randomForest(Lung_Cancer~.-Patient_ID,data = train_data,importance=TRUE,ntree=500)
plot(fit)
varImpPlot(fit,sort=TRUE,main='Variable Importance',n.var = 10)
## predicting value
train_data$prediction = predict(fit,train_data,type = 'prob')
## confusion matrix
library(e1071)
library(caret)
confusionMatrix(data=train_data$prediction,reference=train_data$Lung_Cancer,positive='yes')
test_data$prediction = predict(fit,test_data,type = '')
confusionMatrix(data=test_data$prediction,reference=test_data$Lung_Cancer,positive='yes')
train_data$predict_response = 0
train_data$predict_response[train_data$prediction>=0.5]=1
test_data$predict_response = 0
test_data$predict_response[test_data$prediction>0.5]=1
table(test_data$predict_response,test_data$Lung_Cancer)
##################
varImpPlot(fit,sort=TRUE,main='Variable Importance',n.var = 10)
RFpred <- predict(fit, train_data, type = "prob")
ObsPred <- data.frame(cbind(train_data), Predicted=RFpred)
library(PresenceAbsence)