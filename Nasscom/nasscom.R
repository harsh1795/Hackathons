# Author: Black_viper
##################################################################
################################################################
################################################################
###############################################################
########## RANDOMFOREST ################################
## changing directory
initial.dir=getwd()
setwd(dir = "/home/harshit/Desktop/Nasscom")
library(rpart)
library(randomForest)
## loading datasets
library(xlsx)
data <- read.xlsx("/home/harshit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 2, rowIndex = 6:1395, colIndex = 1:62)
View(data)
attach(data)
test <- read.xlsx("/home/harshit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 3, rowIndex = 6:602, colIndex = 1:62)
View(test)
## cleaning data
miss_data <- lapply(data, function(x){sum(is.na(x))})
names(data)
table(data$Lung_Cancer)/nrow(data)
## checking classes
cols = c(2,3,7,9,11,13,15,16,18,19,20,41,42,43,45,47,48,49,50,51,52,53)
data[,cols] = lapply(data[,cols],function(x){as.factor(x)})
colstest = c(3,7,9,11,13,15,16,18,19,20,41,42,43,45,47,48,49,50,51,52,53)
test[,cols] = lapply(test[,cols],function(x){as.factor(x)})
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
fit = randomForest(Lung_Cancer~.-Patient_ID,data = train_data,importance=TRUE,ntree=500,classwt=c(1,100))
plot(fit)
varImpPlot(fit,sort=TRUE,main='Variable Importance',n.var = 40)
## predicting value
##train_data$prediction = predict(fit,train_data)
## confusion matrix
library(e1071)
library(caret)
#####
RFpred <- predict(fit, train_data, type = "prob")
ObsPred <- data.frame(cbind(train_data), Predicted=RFpred)
train_data <- ObsPred
#####
RFpredtest = predict(fit,test_data,type = "prob")
ObsPredtest <- data.frame(cbind(test_data), Predicted=RFpredtest)
test_data <- ObsPredtest
###########
library(pROC)
roc(train_data$Lung_Cancer, train_data$pred)
###########################################################################
##########################################################################
fit = randomForest(Lung_Cancer~.-Patient_ID,data = data,importance=TRUE,ntree=500,classwt=c(1,100))
plot(fit)
varImpPlot(fit,sort=TRUE,main='Variable Importance',n.var = 40)
## predicting value
##train_data$prediction = predict(fit,train_data)
## confusion matrix
library(e1071)
library(caret)
#####
RFpred <- predict(fit, data, type = "prob")
ObsPred <- data.frame(cbind(data), Predicted=RFpred)
data <- ObsPred
#####
RFpredtest = predict(fit,test,type = "prob")
ObsPredtest <- data.frame(cbind(test), Predicted=RFpredtest)

#####
data$pred=0
data$pred[data$Predicted.1>=0.4]=1
print("-------------------------------------------------------")
print(confusionMatrix(data=data$pred,reference=data$Lung_Cancer))
####
test$Lung_Cancer=0
test$Lung_Cancer = ObsPredtest$Predicted.1
###########################################################################
############################################################################
##############################################################################
#############################################################################
###################### XGBOOST ############################################
library(xgboost)
library(xlsx)
data <- read.xlsx("/home/harshit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 2, rowIndex = 6:1395, colIndex = 1:62)
View(data)
attach(data)
test <- read.csv("/home/harshit/Desktop/Nasscom/g.xlsx")
View(test)
class_data = lapply(data, function(x){class(x)})
class_data
###
xgdata = data
xgdata$Lung_Cancer = NULL
View(xgdata)
xgtest = test
xgtest$Lung_Canacer = NULL
View(xgtest)

bst = xgboost(data=data.matrix(xgdata[,-1]),label=data$Lung_Cancer,max.depth=15,eta=0.3,nrounds = 25,objective="binary:logistic",nthread=3)
y_pred= predict(bst,data.matrix(xgtest[,-1]))
write.csv(y_pred, "/home/harshit/Desktop/Nasscom/h.xlsx")

############################################################################
############################################################################
#################### DECISION TREE #########################################
library(rpart)
library(randomForest)

bag.cancer = randomForest(Lung_Cancer~.-Patient_ID,data = data,mtry=8,importance=T,strata=data$Lung_Cancer,sampsize=c(400,175))
bag.cancer
bag.cancer1 = randomForest(Lung_Cancer~.-Patient_ID,data = data,mtry=9,importance=T,strata=data$Lung_Cancer,sampsize=c(500,170))
bag.cancer1
#########
RFpredtest = predict(bag.cancer,test,type = "prob")
ObsPredtest <- data.frame(cbind(test), Predicted=RFpredtest)
test$Lung_Cancer=0
test$Lung_Cancer = ObsPredtest$Predicted.1
###########
RFpredtest = predict(bag.cancer,test,type = "prob")
ObsPredtest <- data.frame(cbind(test), Predicted=RFpredtest)
test$Lung_Cancer=0
test$Lung_Cancer = ObsPredtest$Predicted.1
write.csv(test,"/home/harshit/Desktop/Nasscom/h.xlsx")



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
##### LOGISTIC REGRESSION #############################################################
data <- read.xlsx("/home/harshit/Desktop/Nasscom/Health_care_Dataset_for_probelm.xlsx.xlsx", sheetIndex = 2, rowIndex = 6:1395, colIndex = 1:62)
View(data)
attach(data)
test <- read.csv("/home/harshit/Desktop/Nasscom/g.xlsx")
View(test)
View(cor(data))
pairs(~Factor2+Factor4+Factor1+Factor3+Factor5+Factor6)
names = names(data)
pairs(~Patient_ID+Lung_Cancer+DiseaseHis1+DiseaseHis1Times++DiseaseHis2+DiseaseHis2Times+DiseaseHis3+DiseaseHis3Times+DiseaseHis4+DiseaseHis5+DiseaseHis6+DiseaseHis7)
####################
glm.fit = glm(Lung_Cancer~.-(Ques4+Ques3+Disease7+Disease6Treat+Disease4+DiseaseHis1+LungFunct4+LungFunct11+LungFunct20+LungFunct6+LungFunct5),data=data,family='binomial')
summary(glm.fit)

glm.fit2 = glm(Lung_Cancer~.-(Ques4+Ques3+Disease7+Disease6Treat+Disease4+DiseaseHis1+LungFunct4+LungFunct11+LungFunct20+LungFunct6+LungFunct5),data=train_data,family='binomial')
summary(glm.fit2)
###
glm.prob = predict(glm.fit,test[-Patient_ID],type="response")
glm.pred=rep(0,562)
glm.pred[glm.prob>0.4]=1
table(glm.pred,test_data$Lung_Cancer)
mean(glm.pred==test_data$Lung_Cancer)