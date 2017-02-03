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
#data$Lung_Cancer=as.factor(data$Lung_Cancer)
#data$Lung_Cancer=NULL
#test$Lung_Cancer=NULL
n= c(45:148)
nn=c(2,3,7,9,11,10,30,31,33,36,34,35,n)
data[,nn] = lapply(data[,nn],function(x){as.factor(x)})
test[,nn] = lapply(test[,nn],function(x){as.factor(x)})
class_data = lapply(data, function(x){class(x)})
View(class_data)
#full_data <- rbind(data,test)
#train_data = full_data[1:1389,]
#test_data = full_data[1390:1985,]
dat = data
dat$Lung_Cancer=NULL
tes = test
tes$Lung_Cancer=NULL
############
library(rpart)
library(adabag)
library(fastAdaboost)
library(ada)
mod.adaboost  = ada(factor(Lung_Cancer)~.,data = data,iter=100,nu=0.2)
pred = predict(mod.adaboost, test,type = "probs")
write.csv(pred,"/home/harshit/Desktop/Nasscom/h.xlsx")
##
adaboost = boosting(factor(Lung_Cancer)~.,data=data,boos=TRUE,mfinal = 5,coeflearn = "Breiman")
###
library(extraTrees)
et = extraTrees(dat,data$Lung_Cancer,ntree = 500,mtry=18,numRandomCuts = 3)
yhat = predict(et,tes)
write.csv(yhat,"/home/harshit/Desktop/Nasscom/h.xlsx")