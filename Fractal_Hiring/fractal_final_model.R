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
dat$Category_2[is.na(dat$Category_2)==T]=2
final$Category_2[is.na(final$Category_2)==T]=2
final$Datetime = as.Date(final$Datetime)
final$year = year(final$Datetime)
final$month = month(final$Datetime)
final$date = day(final$Datetime)
final$weekday = wday(final$Datetime)
final$weekend = 0
final$weekend[final$weekday==1]=1
final$weekend[final$weekday==7]=1
sa = rep(as.Date("2014-01-01"),times = nrow(final))
final$diff_days = difftime(final$Datetime, sa,tz = "days")
final$diff_days = as.numeric(final$diff_days)/86400
dat$Datetime = as.Date(dat$Datetime)
dat$year = year(dat$Datetime)
dat$month = month(dat$Datetime)
dat$date = day(dat$Datetime)
dat$weekday = wday(dat$Datetime)
dat$weekend = 0
dat$weekend[dat$weekday==1]=1
dat$weekend[dat$weekday==7]=1
sa = rep(as.Date("2014-01-01"),times = nrow(dat))
dat$diff_days = difftime(dat$Datetime, sa,tz = "days")
dat$diff_days = as.numeric(dat$diff_days)/86400

##   Pre-Processing Data
datap = dat

#####  Training with exponential smoothing..

product_list = as.character(as.vector(unique(dat$Item_ID)))
product_list_count = length(product_list)
iter =1
size=0
predicted_dff_lin = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric())
predicted_df = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric())
for(i in product_list){
  cat('process starting... item_ID',i,'\n')
  train = datap[datap$Item_ID==as.numeric(i),]
  finalee = final[final$Item_ID==as.numeric(i),]
  test_seq = as.character(finalee$Datetime)
  test_seq = gsub('-',"",test_seq)
  id_seq = paste(i,"_",test_seq,sep = "")
  class(id_seq) = as.vector(id_seq)
  nn = nrow(finalee)
  if(nn>0){
    if(nrow(train)<5000){
      price_ts = ts(train[, c('Price')])
      train$clean_price = tsclean(price_ts)
      sales_ts = ts(train[, c('Number_Of_Sales')])
      train$clean_sales = tsclean(sales_ts)
      y = ses(ts(train$Price),h = nn,alpha = 0.9,initial = "optimal",exponential=TRUE) 
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
      train$price_ma7 = ma(train$clean_price, order=7)
      price_ma = ts(na.omit(train$price_ma7), frequency=30)
      decomp = stl(price_ma, s.window="periodic")
      deseasonal_cnt <- seasadj(decomp)
      tryCatch(fit<-auto.arima(deseasonal_cnt, seasonal = FALSE),warning = function(w){
        cat("your model fails here.")},error = function(e){
          fit<-auto.arima(deseasonal_cnt,seasonal = TRUE)})
      
      fcast <- forecast(fit, h=nn)
      fcast_df = as.data.frame(fcast)
      ass3 = as.vector(fcast_df$`Point Forecast`)
      
      sales_ts = ts(train[, c('Number_Of_Sales')])
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


#####
submission = merge.data.frame(x = final,y = predicted_dff_lin,by = 'ID',all.y = FALSE)
submission[,c('Item_ID','Datetime','Category_1','Category_2','Category_3')]=NULL
write.csv(predicted_dff_lin,"sclean_5000_0.95simpleexpgam.csv",row.names = F)
