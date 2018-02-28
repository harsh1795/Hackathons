


###############################################
##   Author : Harshit Saxena (Black_Viper)   ##
##      (https://github.com/harsh1795)       ##
###############################################


#-----------------------------------------------------------------------------------------------------------------#
#####  .........................Training with exponential smoothing and Arima Models...............................
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

#-----------------------------------------------------------------------------------------------------------------#
# Importing Libraries and Data

## Setting working directory
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

##   Importing Data
dat = read.csv("train.csv", stringsAsFactors =TRUE)
final = read.csv("test.csv", stringsAsFactors = TRUE)

#-----------------------------------------------------------------------------------------------------------------#
# Data Preprocessing

dat$Category_2[is.na(dat$Category_2)==T]=2                               ## Replacing Missing values with 2 (Mode)
final$Category_2[is.na(final$Category_2)==T]=2                           ## Replacing Missing values with 2 (Mode)
final$Datetime = as.Date(final$Datetime)                                 ## Creating features from Datetime column
dat$Datetime = as.Date(dat$Datetime)  

datap = dat


product_list = as.character(as.vector(unique(dat$Item_ID)))                         ## List of unique Item_IDs 
product_list_count = length(product_list)

iter =1
size=0

predicted_dff_lin = data.frame(ID = character(),
                               Number_Of_Sales = numeric(),
                               Price = numeric())                                   ## Dataframe to store results
predicted_df = data.frame(ID = character(),Number_Of_Sales = numeric(),Price = numeric())

for(i in product_list){
  cat('process starting... item_ID',i,'\n')
  train = datap[datap$Item_ID==as.numeric(i),]
  finalee = final[final$Item_ID==as.numeric(i),]
  test_seq = as.character(finalee$Datetime)                      ## Creating ID sequence as given in final dataset
  test_seq = gsub('-',"",test_seq)
  id_seq = paste(i,"_",test_seq,sep = "")
  class(id_seq) = as.vector(id_seq)
  nn = nrow(finalee)
  if(nn>0){
    if(nrow(train)<5000){
      price_ts = ts(train[, c('Price')])                                ## creating datetime as time series object.
      train$clean_price = tsclean(price_ts)                             ## Removing outliers 
      sales_ts = ts(train[, c('Number_Of_Sales')])
      train$clean_sales = tsclean(sales_ts)
      y = ses(ts(train$Price),
              h = nn,alpha = 0.9,
              initial = "optimal",
              exponential=TRUE)                                 ##Simple exponential smoothing(Giving best results)
      z = ses(ts(train$Number_Of_Sales),
              h = nn,
              alpha = 0.9,
              initial = "optimal",
              exponential=TRUE) 
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
      train$price_ma7 = ma(train$clean_price, order=7)        ## creating weekly moving averages
      price_ma = ts(na.omit(train$price_ma7), frequency=30)
      decomp = stl(price_ma, s.window="periodic")             ## calculating seasonal behavior in time series object
      deseasonal_cnt <- seasadj(decomp)                       ## Removing seasonal behavior
      tryCatch(fit<-auto.arima(deseasonal_cnt,
                               seasonal = FALSE),
               warning = function(w){                          ## Fitting ARIMA models for forecasting.
        cat("your model fails here.")},
               error = function(e){
        fit<-auto.arima(deseasonal_cnt,seasonal = TRUE)})
      
      fcast <- forecast(fit, h=nn)
      fcast_df = as.data.frame(fcast)
      ass3 = as.vector(fcast_df$`Point Forecast`)
      
      sales_ts = ts(train[, c('Number_Of_Sales')])             ## Same approach for Number_Of_Sales data
      train$clean_sales = tsclean(sales_ts)
      train$sales_ma7 = ma(train$clean_sales, order=7)
      sales_ma = ts(na.omit(train$sales_ma7), frequency=30)
      decomp_2 = stl(sales_ma, s.window="periodic")
      deseasonal_cnt_2 <- seasadj(decomp_2)
      tryCatch(fit_2<-auto.arima(deseasonal_cnt_2,seasonal=FALSE),
               error = function(e){
        fit2<-auto.arima(deseasonal_cnt_2,seasonal = TRUE)})
      
      fcast_2 <- forecast(fit_2, h=nn)
      fcast_df_2 = as.data.frame(fcast_2)
      ass2 = as.vector(fcast_df_2$`Point Forecast`)
      cat("finished iteration",iter,'\n')
    }
    
    iter=iter+1
    
    predicted_df = data.frame(ID = as.character(id_seq),
                              Number_Of_Sales = as.numeric(ass2),
                              Price = as.numeric(ass3))
    predicted_dff_lin = rbind(predicted_dff_lin,predicted_df)
    
    size=size+nrow(finalee)
    cat("size of predicted_df : ",size,"\n",'\n')
    
    }
 } 


##Storing results in csv format for leaderboard data.
submission = merge.data.frame(x = final,y = predicted_dff_lin,by = 'ID',all.y = FALSE) 
submission[,c('Item_ID','Datetime','Category_1','Category_2','Category_3')]=NULL
write.csv(predicted_dff_lin,"sclean_5000_0.95simpleexpgam.csv",row.names = F)

#----------------------------------------------------------------------------------------------------------------------#
