# Author: Black_Viper
#############################
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
## feature engineering
data_copy = data
test_copy = test
#data= data_copy
#test=test_copy
# Factor2 is related with Factor4 
#data$Patient_ID = NULL
#test$Patient_ID = NULL
print(unique(data$Smoke4))
vector = c("DiseaseHis1Times","DiseaseHis2Times","DiseaseHis3Times","DiseaseHis6","DiseaseStage1","DiseaseStage2","LungFunct20","Disease1","Disease2","Disease3","Disease3Times","Disease4","Disease5","Disease6","Ques5")
for (s in vector){
  for(level in unique(data[,s])){
    data[paste(s, level, sep = "_")] <- ifelse(data[,s] == level, 1, 0)
    test[paste(s, level, sep = "_")] <- ifelse(test[,s] == level, 1, 0)}
  data[,s]=NULL
  test[,s]=NULL
}
null_vector = c("DiseaseHis1","DiseaseHis2","DiseaseHis3")
for(s in null_vector){
  data[,s]=NULL
  test[,s]=NULL
}
y=as.factor(data$Lung_Cancer)
data$Lung_Cancer=NULL
test$Lung_Cancer=NULL
n= c(44:147)
nn=c(2,6,8,9,10,29,30,32,33,34,35,n)
data[,nn] = lapply(data[,nn],function(x){as.factor(x)})
test[,nn] = lapply(test[,nn],function(x){as.factor(x)})

full_data <- rbind(data,test)
train_data = full_data[1:1389,]
test_data = full_data[1390:1985,]
###########################
## Random Forest
bag.model = randomForest(y~.,data=train_data,mtry=15, importance=T,ntree=500)
bag.model
yhat.model = predict(bag.model,test_data,type='prob')
write.csv(yhat.model,"/home/harshit/Desktop/Nasscom/h.xlsx")
## GBM
library(gbm)
library(caret)
data2 = data_copy
data2$Lung_Cancer = as.factor(data2$Lung_Cancer)
data2$Lung_Cancer = ifelse(data2$Lung_Cancer==1,"Yes","No")
outcome = "Lung_Cancer"
objControl <- trainControl(method='adaptive_cv',repeats=5,returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
objModel <- train(train_data, data2[,outcome], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))
summary(objModel)
print(objModel)
predictions <- predict(object=objModel, test_data, type='prob')
write.csv(predictions,"/home/harshit/Desktop/Nasscom/h.xlsx")
## ADABOOST

library(adabag)
library(fastAdaboost)

