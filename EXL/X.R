#setting working directory...
setwd("C:/Users/Harshit/Desktop/EXL")
#
#
# Reading datasets.....
a = read.csv("Finaltest.csv",na.strings = "")
a$X=NULL
a$Unnamed..0=NULL
a$Account.Number = as.character(a$Account.Number)
a$Equipment.Warranty.Expiry.Date = as.character(a$Equipment.Warranty.Expiry.Date)
a$Commence.Date=NULL
a$Equipment.Warranty.Expiry.Date=NULL
a$Account.Number=NULL
a$YYYYMM=NULL
a$Churn.Date=NULL
a$Closetdefaultdate=NULL
#
nn = c(7,23:27,29,31:50)
mm = c(1:6,8:22,28,30)
b[,nn] = lapply(b[,nn], function(x){as.numeric(x)})
b[,mm] = lapply(b[,mm], function(x){as.factor(x)})



library(missForest)
missdata = missForest(missing,ntree = 5,verbose = TRUE,maxiter = 10)
missing = missdata$ximp
View(missing)
write.csv(missing,"exltrain.csv")


#
b = read.csv("TrainEXL2l.csv")
b$X=NULL
b$Account.Number=NULL
b$Commence.Date=NULL
b$Equipment.Warranty.Expiry.Date=NULL
b$Churn.Date=NULL
b$YYYYMM=NULL
b$Closetdefaultdate=NULL

missing=b
testdata = read.csv("TestEXL2.csv",na.strings = "")
testdata$X=NULL
testdata$Unnamed..0=NULL
testdata$usage_number=as.factor(testdata$usage_number)
b$query_days.1=NULL
b$usage_number.1=NULL
b$X=NULL
b$Account.Number=NULL
##   checking outliers.....
bb = b
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
par(mar = rep(2, 4))
outlierKD(bb,Age)
bb$defaultsum = scale(bb$defaultsum)
bb$defaultsum = as.factor(bb$defaultsum)
bb$schemecost = scale(bb$schemecost)
bb$schemecost = as.factor(bb$schemecost)
bb$Posthours = scale(bb$Posthours)
bb$Posthours=as.factor(bb$Posthours)
bb$Hours = scale(bb$Hours)
bb$Hours = as.factor(bb$Hours)
bb$daraGB = scale(bb$daraGB)
bb$daraGB = as.factor(bb$daraGB)
### checking correlation.....      ############################################################
bound <- floor((nrow(b)/2))         #define % of training and test set
b <- b[sample(nrow(b)), ]           #sample rows 
b.train <- b[1:bound, ]              #get training set
b.test <- b[(bound+1):nrow(b), ]  
vector = c()
cordf = c(1:54)
for (i in cordf){
  for (j in cordf){
    a = table(b.train[,i],b.train[,j])
    bb = chisq.test(a)
    c = as.numeric(bb$p.value)
    vector = c(vector , c)
  }
}
cor_matrix = matrix(vector , nrow = 54, ncol = 54,dimnames = list(cordf))



## splitting dataframes for train and test...
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

library("ROSE")
table(b.train$Categ)
prop.table(table(b.train$Categ))
data_balanced_over <- ovun.sample(Categ ~ ., data = b.train, method = "over",N = 15140)$data
################################################################################
##Starting H2O
library(h2o)
h2o.init(nthreads=-1,max_mem_size = "4G")
h2o.removeAll()
df1 = as.h2o(b)
df2 = as.h2o(testdata)
#    1     #####################################################################################
rf1 <- h2o.randomForest(        
  training_frame = df1,        
  balance_classes = TRUE,
  x=c(1:7,9:11,13:15,19:46,50:52),    #(2:8,10:12,14:16,20:49,53:55)                  
  y=12,
  ntrees = 1000,
  mtries = 6,
  score_each_iteration = T)
##   3     ########################################################################################
gbm2 <- h2o.gbm(
  training_frame = df1,       
  x=c(2:7,9:11,13:54),
  y=12, 
  ntrees = 1000,                
  learn_rate = 0.01,             
  max_depth = 50, 
  balance_classes = TRUE,
  score_each_iteration = T)
##   2     ########################################################################################
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
df_train = new_b.train
df_train$id = 1
y = df_train$Categ
df_train$Categ=NULL
df_test = b.test
df_test$id = 0
y_test = df_test$Categ
df_test$Categ=NULL
df_all = rbind(df_train,df_test)
ohe_feats = c('Address', 'Scheme', 'Staus','Gender','Region','Sale.of.Equipment.Status','Equipment.Warranty','Realization_monnos','Cardtype')
dummies <- dummyVars(~Address+Scheme+Staus+Gender+Region+Sale.of.Equipment.Status+Equipment.Warranty+Realization_monnos+Cardtype, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)
 
# split train and test
X = df_all_combined[df_all_combined$id ==1,]
X_test = df_all_combined[df_all_combined$id ==0,]
X$id=NULL
X_test$id=NULL
xgb <- xgboost(data = data.matrix(X), 
               label = y, 
               eta = 0.1,
               max_depth = 500, 
               nround=100, 
               #subsample = 0.5,
               #colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "auc",
               objective = "binary:logistic"
               #num_class = 2
               #nthread = 3
)
y_pred <- predict(xgb, data.matrix(X_test))
names <- dimnames(data.matrix(X))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:30,])

y_pred_class = y_pred>0.2
y_pred_class = as.numeric(y_pred_class)
table(y_test,y_pred_class)
mean(y_test==y_pred_class)

##  Predictions....            ######################################################################################################
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
a=roc(final$Categ,final$predict,percent = TRUE,plot = TRUE)
##   Tuning Parameters....        ###################################################################################################
library(pROC)
library(caret)
outlist = c()
sampling_parameter = c(0,1)
threshold = c(0.17,0.18)
n_trees = c(500,1000,1500)
mtries = c(0.01,0.1,0.3)
tuning = function(sampling_parameter,threshold,n_trees,mtries,b.train,b.test){
  for(sampling in sampling_parameter){
    for(mtry in mtries){
      for(n_tree in n_trees){
        sprintf('iteration started...')
        b.tra = b.train[b.train$Categ==1,]
        b.tra = b.tra[rep(1:nrow(b.tra),each=sampling),]
        new_b.train = rbind(b.train,b.tra)
        df1 = as.h2o(new_b.train)
        df2 = as.h2o(b.test)
        gbm2 <- h2o.gbm(
          training_frame = df1,       
          x=c(1:11,13:54),
          y=12, 
          ntrees = n_tree,                
          learn_rate = mtry,             
          max_depth = 50, 
          balance_classes = TRUE,
          score_each_iteration = T)
        finalRf_predictions<-h2o.predict(
          object = gbm2
          ,newdata = df2)
        df2$predict = finalRf_predictions$predict
        #h2o.varimp_plot(gbm2)
        final = as.data.frame(df2)
        for(thresh in threshold){
          final$predictclass = final$predict>thresh
          final$predictclass = as.numeric(final$predictclass)
          recdata=table(final$Categ,final$predictclass)
          #mean(final$Categ==final$predictclass)
          recall = recdata[[4]]/(recdata[[2]]+recdata[[4]])
          precision = mean(final$Categ==final$predictclass)
          F_meas = (2*recall*precision)/(recall+precision)
          cat(c(sampling,mtry,n_tree,thresh,recall,precision,F_meas),"next..............")
          outlist = append(outlist,c(sampling,mtry,n_tree,thresh,recall,precision,F_meas))
        }
      }
    }
  }
  outlist = as.data.frame(outlist)
  return(outlist)
}

gg = tuning(sampling_parameter,threshold,n_trees,mtries,b.train,b.test )


##    Cross Validation    ################################################################
library(plyr)
library(dplyr)
k = 10
b$Account.Number = sample(1:k, nrow(b), replace = TRUE)
list <- 1:k
prediction <- c()
#function for k fold
for(i in 1:k){
  cat('gwudgudg')
  trainingset <- subset(b, Account.Number %in% list[-i])
  testset <- subset(b, Account.Number %in% c(i))
  cat('gwudgudg')
  df1 = as.h2o(trainingset)
  df2 = as.h2o(testset)
  rf1 <- h2o.randomForest(        
    training_frame = df1,        
    balance_classes = TRUE,
    x=c(2:8,10:12,14:16,20:49,53:55),         
    y=13,
    ntrees = 1000,
    mtries = 4,
    score_each_iteration = T)
  finalRf_predictions<-h2o.predict(
    object = rf1
    ,newdata = df2)
  df2$predict = finalRf_predictions$predict
  #h2o.varimp_plot(rf1)
  final = as.data.frame(df2)
  final$predictclass = final$predict>0.17
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
prediction = as.vector(prediction)
predi = matrix(prediction,nrow = 10,ncol=5,byrow = TRUE)
predi = as.data.frame(predi)
colnames(predi) =  c('correct_zeros','Incorrect Zeros','Correct Ones','Incorrect Ones','AUC')
avg = lapply(predi,FUN = mean)
avg





