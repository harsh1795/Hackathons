#author = Black_Viper
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

##   Importing Data.

dat = read.csv("train.csv", stringsAsFactors =TRUE)
final = read.csv("test.csv", stringsAsFactors = TRUE)
dat$Datetime = as.Date(dat$Datetime)
##   Pre-Processing Data
datap = dat[1:600000,]
datap_t = dat[600001,nrow(dat)]
#Making subsets for all the products.
product_list = as.character(as.vector(unique(dat$Item_ID)))
product_list_count = length(product_list)

# #### plotting
# iter = 0
# setwd("C:/Users/212630200/Desktop/New folder/Hackathon/AV_Fractal/images/clus_2/")
# for(i in tra){
#   train = dat[dat$Item_ID==as.numeric(i),]
#   myplot = ggplot(train, aes(Datetime, Number_Of_Sales)) + geom_line() + scale_x_date('month')  + ylab("Daily Number of sales") +
#     xlab(paste(i,"_Number_Of_Sales",sep=""))
#   myplot2 = ggplot(train, aes(Datetime, Price)) + geom_line() + scale_x_date('month')  + ylab("Daily Number of sales") +
#     xlab(paste(i,"_Number_Of_Price",sep=""))
#   ggsave(paste("salesplot_",i,".jpeg",sep=""),myplot)
#   ggsave(paste("priceplot_",i,".jpeg",sep=""),myplot2)
# 
#   cat("iter :", iter,"done")
#   iter= iter+1
# }
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
# ggplot(clus_df,aes(means_val,end_val_day,color = clus_df$cluster)) + geom_point()
#####
iter =1
test_seq = seq(as.Date("2016-07-01"),as.Date("2016-12-31"),by = "day")
test_seq = gsub('-',"",test_seq)
predicted_dff = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric())
for(i in product_list){
  cat('process starting... item_ID',i,'\n')
  id_seq = paste(i,"_",test_seq,sep = "")
  class(id_seq) = as.vector(id_seq)
  train = datap[datap$Item_ID==as.numeric(i),]
  if(nrow(train)<104){
    price_ts = ts(train[, c('Price')])
    train$clean_price = tsclean(price_ts)
    sales_ts = ts(train[, c('Number_Of_Sales')])
    train$clean_sales = tsclean(sales_ts)
    y = sma(train$clean_price,h = 184) 
    z = sma(train$clean_sales,h = 184)
    cat('predictions number : ',length(y$forecast),'\n')
    ass2 = z$forecast
    ass3 = y$forecast
    cat("finished iteration",iter,'\n')
    
  }
  else{
    price_ts = ts(train[, c('Price')])
    train$clean_price = tsclean(price_ts)
    train$price_ma7 = ma(train$clean_price, order=7)
    price_ma = ts(na.omit(train$price_ma7), frequency=30)
    decomp = stl(price_ma, s.window="periodic")
    deseasonal_cnt <- seasadj(decomp)
    fit<-arima(deseasonal_cnt,order = c(2,1,7), seasonal=FALSE)
    fcast <- forecast(fit, h=184)
    fcast_df = as.data.frame(fcast)
    ass3 = as.vector(fcast_df$`Point Forecast`)
    
    sales_ts = ts(train[, c('Number_Of_Sales')])
    train$clean_sales = tsclean(sales_ts)
    train$sales_ma7 = ma(train$clean_sales, order=7)
    sales_ma = ts(na.omit(train$sales_ma7), frequency=30)
    decomp_2 = stl(sales_ma, s.window="periodic")
    deseasonal_cnt_2 <- seasadj(decomp_2)
    fit_2<-arima(deseasonal_cnt_2,order = c(2,1,7), seasonal=FALSE)
    fcast_2 <- forecast(fit_2, h=184)
    fcast_df_2 = as.data.frame(fcast_2)
    ass2 = as.vector(fcast_df_2$`Point Forecast`)
    cat("finished iteration",iter,'\n')
  }
  iter=iter+1
  predicted_df = data.frame(ID = as.character(id_seq),Number_Of_Sales = as.numeric(ass2), Price = as.numeric(ass3))
  predicted_dff = rbind(predicted_dff,predicted_df)
} 

## Training linear regression...




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


submission = merge.data.frame(x = final,y = predicted_dff,by = 'ID',all.y = FALSE)
submission[,c('Item_ID','Datetime','Category_1','Category_2','Category_3')]=NULL
write.csv(submission,"submit_1_xgb.csv",row.names = F)
