#author = Black_Viper::Harshit Saxena
setwd("C:/Users/212630200/Desktop/New folder/Hackathon/AV_Fractal/")
###  Importing Libraries
library(xts)
library(forecast)
library(lubridate)
library(ggplot2)
library(tseries)
library(plyr)
library(data.table)
library(pracma)
library(cluster)

##   Importing Data...

dat = read.csv("train.csv", stringsAsFactors =TRUE)
final = read.csv("test.csv", stringsAsFactors = TRUE)

## Data Preprocessing...
##
## >> dat = Training Data
## >> final = Leaderboard Data
dat$Category_2[is.na(dat$Category_2)==T]=2        ## Replacing Missing values with 2 (Mode)
final$Category_2[is.na(final$Category_2)==T]=2    ## Replacing Missing values with 2 (Mode)
final$Datetime = as.Date(final$Datetime)          ## Creating features from Datetime column
dat$Datetime = as.Date(dat$Datetime)                                                  
# final$year = year(final$Datetime)
# final$month = month(final$Datetime)
# final$date = day(final$Datetime)
# final$weekday = wday(final$Datetime)
# final$weekend = 0                                 ## Creating Indicator for weekend.
# final$weekend[final$weekday==1]=1                 ## 1 : Sunday , 7 : Saturday
# final$weekend[final$weekday==7]=1
# sa = rep(as.Date("2014-01-01"),times = nrow(final))## Creating feature telling difference in 
#                                                    ## days from 01-01-2014 till date.
# final$diff_days = difftime(final$Datetime, sa,tz = "days")
# final$diff_days = as.numeric(final$diff_days)/86400
# dat$Datetime = as.Date(dat$Datetime)
# dat$year = year(dat$Datetime)
# dat$month = month(dat$Datetime)
# dat$date = day(dat$Datetime)
# dat$weekday = wday(dat$Datetime)
# dat$weekend = 0
# dat$weekend[dat$weekday==1]=1
# dat$weekend[dat$weekday==7]=1
# sa = rep(as.Date("2014-01-01"),times = nrow(dat))
# dat$diff_days = difftime(dat$Datetime, sa,tz = "days")
# dat$diff_days = as.numeric(dat$diff_days)/86400

datap = dat

#####  Training with exponential smoothing and Arima Models..
##
## >> exponential smoothing is done basically for products with very less data.(later on It is 
##    done for all data - giving better accuracy)
## >> Arima models are used to forecast for products with large data.(accuracy was going down with this approach)
## >> Linear models and XGBoost models are also tried but these didn't perform well, one possible reason for this:-
##    1) forecasting period is very big where regression might perform bad.
##    2) all the products doesn't have same behavior in target variables (Price, Number_Of_Sales), thereby leading 
##       very small data and less robustness in our models
## >> Since, all the products are not having same distributions for price and Number_Of_Sales. Itmakes more sense to
##    apply models differently but this reduces data size, so to overcome that I tried clustering different products.
## >> Clustering was done and with 11 clusters giving optimal Gap statistic value, but still giving different 
##    distributions for price and Number_Of_Sales in a cluster.
## >> Finally, I choose to model each product separatly which gave good results.

product_list = as.character(as.vector(unique(dat$Item_ID)))     ## List of unique Item_IDs 
product_list_count = length(product_list)
iter =1
size=0
predicted_dff_lin = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric()) ## Dataframe to store results after each iteration
predicted_df = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric())
for(i in product_list){
  cat('process starting... item_ID',i,'\n')
  train = datap[datap$Item_ID==as.numeric(i),]
  finalee = final[final$Item_ID==as.numeric(i),]
  test_seq = as.character(finalee$Datetime)  ## Creating ID sequence as given in final dataset
  test_seq = gsub('-',"",test_seq)
  id_seq = paste(i,"_",test_seq,sep = "")
  class(id_seq) = as.vector(id_seq)
  nn = nrow(finalee)
  if(nn>0){
    if(nrow(train)<5000){
      price_ts = ts(train[, c('Price')])  ## creating datetime as time series object.
      train$clean_price = tsclean(price_ts) ## Removing outliers 
      sales_ts = ts(train[, c('Number_Of_Sales')])
      train$clean_sales = tsclean(sales_ts)
      y = ses(ts(train$Price),h = nn,alpha = 0.9,initial = "optimal",exponential=TRUE)  ##Simple exponential smoothing(Giving best results)
      z = ses(ts(train$Number_Of_Sales),h = nn,alpha = 0.9,initial = "optimal",exponential=TRUE) 
      y = as.data.frame(y)
      y=y$`Point Forecast`
      z = as.data.frame(z)
      z=z$`Point Forecast`
      
      cat('predictions number : ',length(y),'\n')
      ass2 = z
      ass3 = y
      cat("finished iteration",iter,'\n')
      
    }
    else{
      cat("case 2:::)",'\n')
      price_ts = ts(train[, c('Price')])
      train$clean_price = tsclean(price_ts)
      train$price_ma7 = ma(train$clean_price, order=7)             ## creating weekly moving averages
      price_ma = ts(na.omit(train$price_ma7), frequency=30)
      decomp = stl(price_ma, s.window="periodic")                  ## calculating seasonal behavior in time series object
      deseasonal_cnt <- seasadj(decomp)                            ## Removing seasonal behavior
      tryCatch(fit<-auto.arima(deseasonal_cnt, seasonal = FALSE),warning = function(w){ ## Fitting ARIMA models for forecasting.
        cat("your model fails here.")},error = function(e){
          fit<-auto.arima(deseasonal_cnt,seasonal = TRUE)})
      
      fcast <- forecast(fit, h=nn)
      fcast_df = as.data.frame(fcast)
      ass3 = as.vector(fcast_df$`Point Forecast`)
      
      sales_ts = ts(train[, c('Number_Of_Sales')])        ## Same approach for Number_Of_Sales data
      train$clean_sales = tsclean(sales_ts)
      train$sales_ma7 = ma(train$clean_sales, order=7)
      sales_ma = ts(na.omit(train$sales_ma7), frequency=30)
      decomp_2 = stl(sales_ma, s.window="periodic")
      deseasonal_cnt_2 <- seasadj(decomp_2)
      tryCatch(fit_2<-auto.arima(deseasonal_cnt_2,seasonal=FALSE),error = function(e){
        fit2<-auto.arima(deseasonal_cnt_2,seasonal = TRUE)})
      
      fcast_2 <- forecast(fit_2, h=nn)
      fcast_df_2 = as.data.frame(fcast_2)
      ass2 = as.vector(fcast_df_2$`Point Forecast`)
      cat("finished iteration",iter,'\n')
    }
    iter=iter+1
    predicted_df = data.frame(ID = as.character(id_seq),Number_Of_Sales = as.numeric(ass2), Price = as.numeric(ass3))
    predicted_dff_lin = rbind(predicted_dff_lin,predicted_df)
    size=size+nrow(finalee)
    cat("size of predicted_df : ",size,"\n",'\n')
    }
 } 


##### Storing results in csv format for leaderboard data.
submission = merge.data.frame(x = final,y = predicted_dff_lin,by = 'ID',all.y = FALSE) 
submission[,c('Item_ID','Datetime','Category_1','Category_2','Category_3')]=NULL
write.csv(predicted_dff_lin,"sclean_5000_0.95simpleexpgam.csv",row.names = F)

#####  cLUSTERING
# #### clustering
# ggplot(dat, aes(Datetime, c(Price))) + geom_point() + scale_x_date('month')  + ylab("Price") +
#   xlab("")
# clus_df = data.frame(ID = numeric(),means_val=  numeric(),medians_val= numeric(),
#                      stds_val = numeric(),maxs_val = numeric(),
#                      mins_val = numeric(),meanp_val=  numeric(),medianp_val= numeric(),
#                      stdp_val = numeric(),maxp_val = numeric(),
#                      minp_val = numeric(),start_val=character(),end_val=character(),
#                      diff_val = numeric(),cat1_val = numeric(),cat2_val=numeric(),cat3_val=numeric())
# iter=0
# for(i in product_list){
#   cat("iteration :",iter,'\n')
#   train = dat[dat$Item_ID==as.numeric(i),]
#   means_val = mean(train$Number_Of_Sales)
#   medians_val = median(train$Number_Of_Sales)
#   stds_val = std(train$Number_Of_Sales)
#   maxs_val = max(train$Number_Of_Sales)
#   mins_val = min(train$Number_Of_Sales)
# 
#   meanp_val = mean(train$Price)
#   medianp_val = median(train$Price)
#   stdp_val = std(train$Price)
#   maxp_val = max(train$Price)
#   minp_val = min(train$Price)
#   start_val = as.character(min(train$Datetime))
#   end_val = as.character(max(train$Datetime))
#   diff_val = as.numeric(difftime(end_val,start_val,tz = "days"))
#   cat1_val = train$Category_1[1]
#   cat2_val = train$Category_2[1]
#   cat3_val = train$Category_3[1]
#   clusdf = data.frame(ID = i,means_val=  means_val,medians_val= medians_val,
#                       stds_val = stds_val,maxs_val = maxs_val,
#                       mins_val = mins_val,meanp_val=  meanp_val,medianp_val= medianp_val,
#                       stdp_val = stdp_val,maxp_val = maxp_val,
#                       minp_val = minp_val,start_val=start_val,end_val=end_val,
#                       diff_val = diff_val,cat1_val = cat1_val,cat2_val=cat2_val,cat3_val=cat3_val)
#   clus_df = rbind(clus_df,clusdf)
#   iter=iter+1
# }
# clus_df$start_val = as.Date(clus_df$start_val)
# clus_df$end_val = as.Date(clus_df$end_val)
# clus_df$start_val_year = year(clus_df$start_val)
# clus_df$start_val_month = month(clus_df$start_val)
# clus_df$start_val_day = day(clus_df$start_val)
# clus_df$end_val_year = year(clus_df$end_val)
# clus_df$end_val_month = month(clus_df$end_val)
# clus_df$end_val_day = day(clus_df$end_val)
# clus_df$start_val=NULL
# clus_df$end_val=NULL
# clus_df$cat2_val[is.na(clus_df$cat2_val)==TRUE]=2
# 
# gap_stat <- clusGap(clus_df[,c(2:21)], FUN = kmeans, nstart = 5,
#                     K.max = 20)
# k=11
# clus_dat = kmeans(clus_df[,c(2,21)],11,nstart = 5)
# clus_df$cluster = clus_dat$cluster


#### LINEAR MODEL/ XGBOOST MODEL
## Training xgboost...
#dataxg = datap
#datap$Category_2[is.na(datap$Category_2)==TRUE]=2
#final$Category_2[is.na(final$Category_2)==TRUE]=2

## creating features...
# dataxg$Datetime = as.Date(dataxg$Datetime)
# dataxg$year = year(dataxg$Datetime)
# dataxg$month = month(dataxg$Datetime)
# dataxg$date = day(dataxg$Datetime)
# dataxg$weekday = wday(dataxg$Datetime)
# dataxg$weekend = 0
# dataxg$weekend[dataxg$weekday==1]=1
# dataxg$weekend[dataxg$weekday==7]=1
# sa = rep(as.Date("2014-01-01"),times = nrow(dataxg))
# dataxg$diff_days = difftime(dataxg$Datetime, sa,tz = "days")
# dataxg$diff_days = as.numeric(dataxg$diff_days)
# nn = c(4,5,12,13)
# dataxg[,nn] = lapply(dataxg[,nn],FUN =function(x){as.factor(x)})
# 
# final$Datetime = as.Date(final$Datetime)
# final$year = year(final$Datetime)
# final$month = month(final$Datetime)
# final$date = day(final$Datetime)
# final$weekday = wday(final$Datetime)
# final$weekend = 0
# final$weekend[final$weekday==1]=1
# final$weekend[final$weekday==7]=1
# sa = rep(as.Date("2014-01-01"),times = nrow(final))
# final$diff_days = difftime(final$Datetime, sa,tz = "days")
# final$diff_days = as.numeric(final$diff_days)
# nn = c(4,5,10,11)
# final[,nn] = lapply(final[,nn],FUN =function(x){as.factor(x)})
# 
# ## Training and testing dataset...
# train = dataxg[,c(2,4:14)]
# test = final[,c(1,3:5,7:12)]
# 
# train_label1 = train$Price
# train_label2 = train$Number_Of_Sales
# #test_label1 = test$Price
# #test_label2 = test$Number_Of_Sales
# train$Price=NULL
# train$Number_Of_Sales=NULL
# #test$Price=NULL
# #test$Number_Of_Sales=NULL
# new_tr <- model.matrix(~.+0,data = train) 
# new_ts <- model.matrix(~.+0,data = test)
# 
# library(xgboost)
# dtrain <- xgb.DMatrix(data = new_tr,label = train_label2) 
# dtest <- xgb.DMatrix(data = new_ts)
# 
# params <- list(booster = "gblinear", objective = "reg:linear", eta=0.02, gamma=0, silent=1,
#                                   max_depth=8, min_child_weight=8, subsample=0.9,
#                                     colsample_bytree=0.8)
# 
# #xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T, 
# #                                  stratified = T, print.every.n = 10, early.stop.round = 20,
# #                                maximize = F)
# 
# 
# xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 500, eval_metric = "rmse")
# 
# 
# xgbpred <- predict (xgb1,dtest)
# final$Number_Of_Sales = xgbpred
# submission = final[,c(6,13,14)]
# #losss <- function(error){
# #  loss =  sqrt(mean(error^2))
# #  return(loss)
# #}
# #error = xgbpred - test_label1
# #losss(error)