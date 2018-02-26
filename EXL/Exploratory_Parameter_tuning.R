#######################################
#    Author : Harshit Saxena          #
#   (https://github.com/harsh1795)    #
#######################################

#-------------------------------------------------------------------------------------------------------------------------#
# Setting Working Directory
setwd("C:/Users/Harshit/Desktop/EXL")

#-------------------------------------------------------------------------------------------------------------------------#
# Importing Libraries and Data
library(missForest)                                                              # Handling missing data
library(ROSE)                                                                    # For Over/UnderSampling
library(xgboost)                                                                 # XGBoost Model
library(readr)
library(stringr)
library(caret)
library(car)
library(pROC)
library(plyr)
library(dplyr)


library(h2o)                                                                     # Initializaing H2O session
h2o.init(nthreads=-1,max_mem_size = "4G")
h2o.removeAll()

# Importing Data
a = read.csv("Finaltest.csv",na.strings = "")
b = read.csv("TrainEXL2l.csv")
testdata = read.csv("TestEXL2.csv",na.strings = "")

#-------------------------------------------------------------------------------------------------------------------------#
# Data Preprocessing
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

b$X=NULL
b$Account.Number=NULL
b$Commence.Date=NULL
b$Equipment.Warranty.Expiry.Date=NULL
b$Churn.Date=NULL
b$YYYYMM=NULL
b$Closetdefaultdate=NULL


testdata$X=NULL
testdata$Unnamed..0=NULL
testdata$usage_number=as.factor(testdata$usage_number)
b$query_days.1=NULL
b$usage_number.1=NULL
b$X=NULL
b$Account.Number=NULL

# converting data-types
nn = c(7,23:27,29,31:50)
mm = c(1:6,8:22,28,30)
b[,nn] = lapply(b[,nn], function(x){as.numeric(x)})
b[,mm] = lapply(b[,mm], function(x){as.factor(x)})

# Handling missing values in data
missing=b
missdata = missForest(missing,ntree = 5,verbose = TRUE,maxiter = 10)
missing = missdata$ximp
#View(missing)
write.csv(missing,"exltrain.csv")                                               # Storing data with imputed missing values


# Checking Outliers
bb = b
outlier_detection = function(dt, var) {                                         # Outlier detection function
  var_name = eval(substitute(var),eval(dt))
  
  # Statistics
  na1 = sum(is.na(var_name))
  m1 = mean(var_name, na.rm = T)
  
  # Visualization
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)                        # Histogram plot
  
  outlier = boxplot.stats(var_name)$out
  mo = mean(outlier)
  var_name = ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")                                    # Box-Plot
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  
  title("Outlier Check", outer=TRUE)
  na2 = sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  
  m2 = mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response = readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] = invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } 
  else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}

par(mar = rep(2, 4))
outlier_detection(bb,Age)                                                             # Performing outlier detection

# Scaling features
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

# Correlation Statistics
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

# Train-Test Split
bound = floor((nrow(b)*0.8))
set.seed(123)
b = b[sample(nrow(b)), ]       
b.train = b[1:bound, ]            
b.test = b[(bound+1):nrow(b), ]  

sum(b.train$Categ)                                                                 # Number of '1' samples
sum(b.test$Categ)                                                                  # Number of '0' samples

# OverSampling
b.tra = b.train[b.train$Categ==1,]
b.tra = b.tra[rep(1:nrow(b.tra),each=0),]
new_b.train = rbind(b.train,b.tra)

table(b.train$Categ)                                                               # Number of '1' & '0' samples
prop.table(table(b.train$Categ))                                                   # Ratios of '1' & '0' samples

data_balanced_over = ovun.sample(Categ~., data=b.train, method = "over", N=15140)$data  # Oversampling by ROSE Package

#-------------------------------------------------------------------------------------------------------------------------#
# Data Modelling

df1 = as.h2o(b)                                                                    # Training H2O Frame
df2 = as.h2o(testdata)                                                             # Testing H2O Frame

#--------------------------------------------------------------------
# Model 1 (Random Forest)
rf1 <- h2o.randomForest(training_frame = df1,        
                        balance_classes = TRUE,
                        x=c(1:7,9:11,13:15,19:46,50:52),                    
                        y=12,
                        ntrees = 1000,
                        mtries = 6,
                        score_each_iteration = T)

# Model 2 (Gradient Boosting Algorithm(GBM))
gbm2 <- h2o.gbm(
  training_frame = df1,       
  x=c(2:7,9:11,13:54),
  y=12, 
  ntrees = 1000,                
  learn_rate = 0.01,             
  max_depth = 50, 
  balance_classes = TRUE,
  score_each_iteration = T)

# Predictions
finalRf_predictions<-h2o.predict(object = rf1,
                                 newdata = df2)

df2$predict = finalRf_predictions$predict

# plotting variable importance
h2o.varimp_plot(rf1)

final = as.data.frame(df2)
final$predictclass = final$predict>0.17
final$predictclass = as.numeric(final$predictclass)

# checking accuracy
table(final$Categ,final$predictclass)
mean(final$Categ==final$predictclass)

# calculating AUC
a=roc(final$Categ,final$predict,percent = TRUE,plot = TRUE)

#------------------------------------------------------------------------------
# Model 3 (Xtreme Gradient Boosting (XGBoost))

df_train = new_b.train
df_train$id = 1
df_test = b.test
df_test$id = 0

y = df_train$Categ
y_test = df_test$Categ

df_train$Categ=NULL
df_test$Categ=NULL

df_all = rbind(df_train,df_test)

ohe_feats = c('Address', 'Scheme', 'Staus','Gender','Region',
              'Sale.of.Equipment.Status','Equipment.Warranty',
              'Realization_monnos','Cardtype')

# Creating dummy variables for One Hot Encoding
dummies <- dummyVars(~Address+Scheme+Staus+Gender+Region+
                     Sale.of.Equipment.Status+Equipment.Warranty+
                     Realization_monnos+Cardtype, data = df_all)

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
               subsample = 0.8,
               colsample_bytree = 0.75,
               seed = 1,
               eval_metric = "auc",
               objective = "binary:logistic")

# Predictions
y_pred <- predict(xgb, data.matrix(X_test))
names <- dimnames(data.matrix(X))[[2]]

# plotting variable importance
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:30,])

y_pred_class = y_pred>0.2                                                           # Threshold = 0.2
y_pred_class = as.numeric(y_pred_class)

# Checking Accuracy
table(y_test,y_pred_class)
mean(y_test==y_pred_class)

#-------------------------------------------------------------------------------------------------------------------------#
# Parameter Tuning

outlist = c()

# Paramter Initialization
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
        
        gbm2 <- h2o.gbm(training_frame = df1,       
                        x=c(1:11,13:54),
                        y=12, 
                        ntrees = n_tree,                
                        learn_rate = mtry,             
                        max_depth = 50, 
                        balance_classes = TRUE,
                        score_each_iteration = T)
        
        finalRf_predictions<-h2o.predict(object = gbm2,
                                         newdata = df2)
        
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
write.csv(gg, "tuning.csv", row.names = FALSE)

#-------------------------------------------------------------------------------------------------------------------------#
# K-Fold Cross Validation

k = 10
b$Account.Number = sample(1:k, nrow(b), replace = TRUE)
list <- 1:k
prediction <- c()
#function for k fold
for(i in 1:k){
  
  trainingset <- subset(b, Account.Number %in% list[-i])
  testset <- subset(b, Account.Number %in% c(i))
  
  df1 = as.h2o(trainingset)
  df2 = as.h2o(testset)
  
  rf1 <- h2o.randomForest(training_frame = df1,        
                          balance_classes = TRUE,
                          x=c(2:8,10:12,14:16,20:49,53:55),         
                          y=13,
                          ntrees = 1000,
                          mtries = 4,
                          score_each_iteration = T)
  
  finalRf_predictions<-h2o.predict(object = rf1,
                                   newdata = df2)
  
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

#-------------------------------------------------------------------------------------------------------------------------#

