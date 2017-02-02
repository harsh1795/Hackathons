#setting working directory...
setwd("C:/Users/Harshit/Desktop/EXL")
b = read.csv("final.csv")
b$X=NULL

library(h2o)
h2o.init(nthreads=-1,max_mem_size = "4G")
h2o.removeAll()
library(pROC)
library(caret)
library(plyr)
library(dplyr)
# K fold cross validation.....................................................
k = 10
b$Account.Number = sample(1:k, nrow(b), replace = TRUE)
list <- 1:k
prediction <- c()
#function for k fold
for(i in 1:k){
  cat('iteration started........')
  trainingset <- subset(b, Account.Number %in% list[-i])
  testset <- subset(b, Account.Number %in% c(i))
  df1 = as.h2o(trainingset)
  df2 = as.h2o(testset)
  rf1 <- h2o.randomForest(        
    training_frame = df1,        
    balance_classes = TRUE,
    # changing column no.s................................
    x=c(2:12,14:55),                      
    y=13,
    ntrees = 1000,#............................................
    mtries = 6,#..........................................................
    score_each_iteration = T)
  finalRf_predictions<-h2o.predict(
    object = rf1
    ,newdata = df2)
  df2$predict = finalRf_predictions$predict
  #h2o.varimp_plot(rf1)
  final = as.data.frame(df2)
  final$predictclass = final$predict>0.17#...........................................
  final$predictclass = as.numeric(final$predictclass)
  recdata=table(final$Categ,final$predictclass)
  correct_ones = recdata[[4]]
  incorrect_ones = recdata[[2]]
  correct_zeros = recdata[[1]]
  incorrect_zeros = recdata[[3]]
  confusionmatrix = c(correct_zeros,incorrect_zeros,correct_ones,incorrect_ones)
  a=roc(final$Categ,final$predict,percent = TRUE,plot = TRUE)
  j = as.numeric(a$auc)
  kk = c(confusionmatrix,j)
  prediction <- c(prediction, kk)
}







bound <- floor((nrow(b)*0.8))
set.seed(123)#define % of training and test set
b <- b[sample(nrow(b)), ]           #sample rows 
b.train <- b[1:bound, ]              #get training set
b.test <- b[(bound+1):nrow(b), ]  
sum(b.train$Categ)
sum(b.test$Categ)
################################################################################
b.tra = b.train[b.train$Categ==1,]
b.tra = b.tra[rep(1:nrow(b.tra),each=0),]
new_b.train = rbind(b.train,b.tra)
df1 = as.h2o(new_b.train)
df2 = as.h2o(b.test)
rf1 <- h2o.randomForest(        
  training_frame = df1,        
  balance_classes = TRUE,
  x=c(1:7,9:11,13:15,19:48,52:54),                      
  y=12,
  ntrees = 1000,
  mtries = 6,
  score_each_iteration = T)
finalRf_predictions<-h2o.predict(
  object = rf1
  ,newdata = df2)
df2$predict = finalRf_predictions$predict
h2o.varimp_plot(rf1)
final = as.data.frame(df2)
final$predictclass = final$predict>0.17
final$predictclass = as.numeric(final$predictclass)
table(final$Categ,final$predictclass)
mean(final$Categ==final$predictclass)











