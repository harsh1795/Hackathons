setwd("C:/Users/Harshit/Desktop/Analytics Competitions/ZS Associates")


df = read.csv("train.csv")
test_df = read.csv("test.csv")

#library(ROSE)
#data_balance_over = ovun.sample(target_bin~.,data=df,method = "over")$data
#table(data_balance_over$target_bin)
library(mice)
View(md.pattern(df))

#library(missForest)
#imp_df = missForest(df)

n1 = c(3,4,35,44,51,55,56,57,58,59,60)
df[,n1] = lapply(df[,n1],FUN = function(x){as.factor(x)})
test_df[,n1] = lapply(test_df[,n1],FUN = function(x){as.factor(x)})
n2 = c(1,2,5:34,36:43,45:50,52:54,61,62)
df[,n2] = lapply(df[,n2], function(x){as.numeric(x)})
test_df[,n2] = lapply(test_df[,n2], function(x){as.numeric(x)})
d2 = df[,n2]
d1 = df[,n1]
d3 = test_df[,n1]
d4 = test_df[,n2]

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}
for(i in 1:ncol(d4)){
  d4[is.na(d4[,i]), i] <- mean(d4[,i], na.rm = TRUE)
}
test_df[,n2]=d4
for(i in 1:ncol(d2)){
  d2[is.na(d2[,i]), i] <- mean(d2[,i], na.rm = TRUE)
}
df[,n2]=d2
for(i in 1:ncol(d3)){
  d3[is.na(d3[,i]), i] <- Mode(d3[,i], na.rm = TRUE)
}
test_df[,n1]=d3
for(i in 1:ncol(d1)){
  d1[is.na(d1[,i]), i] <- Mode(d1[,i], na.rm = TRUE)
}
df[,n1]=d1

df$weekend = 0
df$weekend[df$day_of_week=="sunday"] =1 
df$weekend[df$day_of_week=="saturday"] = 1

test_df$weekend = 0
test_df$weekend[test_df$day_of_week=="sunday"] =1 
test_df$weekend[test_df$day_of_week=="saturday"] = 1


dft = df[df$target_bin == 1,]
dft = dft[rep(1:nrow(dft),each=2.1),]

new_df1 = rbind(df,dft)
new_df = as.h2o(new_df1)
test_df = as.h2o(test_df)
#####################################################################################
library(Matrix)
yy = new_df1$target_bin
dataa = new_df1
dataa$target_bin=NULL
comb = rbind(dataa,test_df)

sparse_matrix <- sparse.model.matrix(content_id ~ .-1, data = comb)
a = data.matrix(sparse_matrix)
dtrain = a[1:37586,]
dtest = a[!dtrain]
output_vector = new_df1$target_bin
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
xgb <- xgboost(data = data.matrix(sparse_matrix[,-2]), 
               label = output_vector, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)
y_pred <- predict(xgb, data.matrix(test_df[,-1]))
dat1 = cbind(test_df$content_id,y_pred)
dat = as.data.frame(dat1)
write.csv(dat,"final28.csv")





library(h2o)
## starting cluster
h2o.init(
  nthreads=-1,            ## -1: use all available threads
  max_mem_size = "4G")
h2o.removeAll()
#df <- h2o.importFile(path = "train.csv")
#tes <- h2o.importFile(path = "test.csv")
#View(df)
#View(tes)




rf1 <- h2o.randomForest(         ## h2o.randomForest function
  training_frame = new_df,        ## the H2O frame for training
  #validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(2:62,64),                        ## the predictor columns, by column index
  y=63,                          ## the target index (what we are predicting)
  model_id = "rf_v1",            ## name the model in H2O
  ntrees = 500,
  mtries = 8,
  nbins = 100,                    ## use a maximum of 200 trees to create the
  #stopping_rounds = 3,           ## Stop fitting new trees when the 2-tree
  score_each_iteration = T)
h2o.varimp_plot(rf1)


#######################################################################################

finalRf_predictions<-h2o.predict(
  object = rf1
  ,newdata = test_df)

dat1 = h2o.cbind(test_df$content_id,finalRf_predictions$predict)
dat = as.data.frame(dat1)
write.csv(dat,"final27.csv")


