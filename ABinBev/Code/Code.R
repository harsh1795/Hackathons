

#----------------------------------------------------------------------------------------------------------------------#

## Competition  -  Data Science Hackathon by AbinBev & Analytics Vidya
#
# Author: HARSHIT SAXENA

#----------------------------------------------------------------------------------------------------------------------#

#####################
##    Contents     ##
#####################

#      (1)  IMPORTING LIBRARIES .............................................................
#      (2)  ASSUMPTIONS & APPROACH ..........................................................
#      (3)  FORECASTING VOLUME ..............................................................
#      (4)  SKU RECOMMENDATION ..............................................................
#      (5)  DATA VISUALIZATION ..............................................................
#      (6)  OTHER MODELS ....................................................................
#      (7)  TRAINING - VALIDATION - TESTING .................................................

#-----------------------------------------------------------------------------------------------------------------------#
setwd("/home/harshit/Desktop/Analytics/ABinBev")
# ...Importing libraries...
library(data.table)
library(ggplot2)
library(xts)
library(forecast)
library(lubridate)
library(tseries)
library(plyr)
library(pracma)
library(cluster)
library(ggthemes)
library(FNN)
#-----------------------------------------------------------------------------------------------------------------------#

# ...Importing datasets...
f_hist_vol = fread("/home/harshit/Desktop/Analytics/ABinBev/historical_volume.csv")
f_evnt_cal = fread("/home/harshit/Desktop/Analytics/ABinBev/event_calendar.csv")
f_demo = fread("/home/harshit/Desktop/Analytics/ABinBev/demographics.csv")
f_soda_sls = fread("/home/harshit/Desktop/Analytics/ABinBev/industry_soda_sales.csv")
f_ind_vol = fread("/home/harshit/Desktop/Analytics/ABinBev/industry_volume.csv")
f_prc_sls = fread("/home/harshit/Desktop/Analytics/ABinBev/price_sales_promotion.csv")
f_wthr = fread("/home/harshit/Desktop/Analytics/ABinBev/weather.csv")
f_vlm_for_test = fread("/home/harshit/Desktop/Analytics/ABinBev/volume_forecast_test.csv")
f_sku_reco_test = fread("/home/harshit/Desktop/Analytics/ABinBev/sku_recommendation_test.csv")

#-----------------------------------------------------------------------------------------------------------------------#

################################
#   Assumptions and Approach   #
################################

#   (1)    VOLUME_FORECASTING 

#             (a)  ASSUMPTION : I assumed forecast volume as 0 for Agency-SKU combinations
#                  for which there is no past data available.
#             (b)  For time series, I assumed dates to be 1st of all months. (Example: 01/012013,
#                  01/06/2017, 01/12/2017) 
#             (c)  I used time series models for January forecasting like (Simple Exponential 
#                  Smoothing (SES), ARIMA, ETS).
#             (d)  SES (Simple Exponential Smoothing) gave the best results for my validation
#                  data (DECEMBER 2017) compared to other models.
#             (e)  I took ensemble of rwf model and ses model by averging the predictions.

#   (2)    SKU RECOMMENDATION

#             (a)  ASSUMPTION : data used for recommendation task are- "price_sales_promotion.csv",
#                  "historical_volume.csv" and "demographics.csv".
#             (b)  I calculated 5 nearest neighbours for Agency_06 & Agency_14 using K-Nearest
#                  Neighbours Algorithm based on "demographics.csv" file.
#             (c)  I forecasted Sales, Price, Promotions for January 2018 given in 
#                  "price_sales_promotion.csv" file.
#             (d)  I then formulate PROFIT as (SALES - PROMOTIONS)*VOLUME
#             (e)  I then select top SKUs for both Agencies based on neighbours.     

#----------------------------------------------------------------------------------------------------------------------#

##############################
##    Forecasting Volume    ##
##############################


f_vlm_for_test$YearMonth = 201801
vol_data = rbind(f_hist_vol, f_vlm_for_test)

# Converting datetime object.
vol_data$YearMonth = as.character(vol_data$YearMonth)
vol_data$YearMonth = as.Date(paste0(vol_data$YearMonth, '01'), format='%Y%m%d')
vol_data$ID = paste0(vol_data$Agency, vol_data$SKU)
summary(vol_data)
vol_data = vol_data[complete.cases(vol_data),]

# ...Simple Exponential Method...
agency_list = as.character(as.vector(unique(vol_data$Agency)))     ## List of unique Agencies 
agency_list_count = length(agency_list)

SKU_list = as.character(as.vector(unique(vol_data$SKU)))           ## List of unique SKUs 
SKU_list_count = length(SKU_list)

iter =1
empty_SKU = c()

predicted_df = data.frame(Agency = character(), 
                          SKU = character(), 
                          Volume = numeric())
predicted_dff_lin = data.frame(Agency = character(),              ## Dataframe to store results
                               SKU = character(),
                               Volume = numeric())         
vol_data$Volume = as.numeric(vol_data$Volume)

# .... SIMPLE EXPONENTIAL SMOOTHING ....

cat('Starting Simple Exponential Smoothing...','\n','\n')
for(i in agency_list){
  cat('process starting... ',i,'\n')
  for(j in SKU_list)
    {
    cat("Modelling ", j, '\n')
    train = vol_data[vol_data$Agency == i,]
    train = train[train$SKU == j,]
    nn = nrow(train)
    cat(nn, '\n')
    if(nrow(train)>0){
      vol_ts = ts(train[, c('Volume')], frequency = 12)     ## creating datetime as time series object.
      y = ses(vol_ts,
              h = 1,
              alpha = 0.9,
              initial = "simple",
              exponential=TRUE
              )                                                 ## SES (Giving best results)
      y = as.data.frame(y)
      y=y$`Point Forecast`
      ass3 = y
      cat("finished iteration : ",iter,'\n')
    }
    else{
      ass3 = 0
      empty_SKU = c(empty_SKU, paste0(i, j))
    }
    iter=iter+1
    predicted_df = data.frame(Agency = as.character(i),SKU = as.character(j), Volume = as.numeric(ass3))
    predicted_dff_lin = rbind(predicted_dff_lin,predicted_df)
  }
}

summary(predicted_dff_lin)


# ... Random Walk Forecast ...

iter =1
empty_SKU = c()

predicted_df = data.frame(Agency = character(), 
                          SKU = character(), 
                          Volume = numeric())
predicted_rwf_lin = data.frame(Agency = character(),              ## Dataframe to store results
                               SKU = character(),
                               Volume = numeric()) 
cat('Starting Random Walk Forecast...','\n','\n')
for(i in agency_list){
  cat('process starting... ',i,'\n')
  for(j in SKU_list)
  {
    cat("Modelling ", j, '\n')
    train = vol_data[vol_data$Agency == i,]
    train = train[train$SKU == j,]
    nn = nrow(train)
    cat(nn, '\n')
    if(nrow(train)>0){
      vol_ts = ts(train[, c('Volume')], frequency = 12)        ## creating datetime as time series object.
      y <- rwf(vol_ts, h = 1, level = c(80, 95, 99))           ## Random Walk Forecast
      y = as.data.frame(y)
      y=y$`Point Forecast`
      ass3 = y
      cat("finished iteration : ",iter,'\n')
    }
    else{
      ass3 = 0
      empty_SKU = c(empty_SKU, paste0(i, j))
    }
    iter=iter+1
    predicted_df = data.frame(Agency = as.character(i),SKU = as.character(j), Volume = as.numeric(ass3))
    predicted_rwf_lin = rbind(predicted_rwf_lin,predicted_df)
  }
}

summary(predicted_rwf_lin)

# ...ensembling...
ensemble = merge(predicted_dff_lin, predicted_rwf_lin, by = c("Agency", "SKU"), all = T, sort = F)
ensemble$Volume = (ensemble$Volume.x + ensemble$Volume.y)/2
ensemble$Volume.x = NULL
ensemble$Volume.y = NULL
write.csv(ensemble, "volume_forecast.csv", row.names = F)


#----------------------------------------------------------------------------------------------------------------------#
##############################
##    SKU Recommendation    ##
##############################

#   Recommendation Strategy:
#   ------------------------
#        (a) Price column has a pattern with Sales and Promotions column. There are two scenarios for 
#            a Agency-SKU Combination: 
#                   (1)  Price is equal to (Sales - Promotions)
#                   (2)  Price is equal to (Sales + Promotions)
#
#        (b) Agency will get best profit in the nearest neighbours when:
#                   Profit = (Sales - Promotion)*Volume in January Month

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

f_demo_n = f_demo
f_demo_n$Avg_Population_2017 = normalize(f_demo_n$Avg_Population_2017)
f_demo_n$Avg_Yearly_Household_Income_2017 = normalize(f_demo_n$Avg_Yearly_Household_Income_2017)

f_demo_knn = get.knn(f_demo_n[,c(2:3)], k = 6)
#  After running KNN , we get the nearest 5 neighbours based on the average population and 
#  average income standard for 2017. This helps us to locate the new agency which may behave
#  in similar fashion.

#  list of Agencies for Agency_06 are : Agency_55, Agency_60, Agency_51, Agency_56, Agency_46
#  list of Agencies for Agency_14 are : Agency_56, Agency_60, Agency_48, Agency_57, Agency_46

#  Forecasting Sales and Promotions for January 2018

# -- Sales-- #

iter =1
empty_SKU = c()

predicted_sales_df = data.frame(Agency = character(), 
                                SKU = character(), 
                                Sales = numeric())
predicted_sales_lin = data.frame(Agency = character(),                       
                                 SKU = character(),
                                 Sales = numeric())         

cat('Starting Simple Exponential Smoothing for Sales, Promotions and Price...','\n','\n')
for(i in agency_list){
  cat('process starting... ',i,'\n')
  for(j in SKU_list)
  {
    cat("Modelling ", j, '\n')
    train = f_prc_sls[f_prc_sls$Agency == i,]
    train = train[train$SKU == j,]
    nn = nrow(train)
    cat(nn, '\n')
    if(nrow(train)>0){
      vol_ts = ts(train[, c('Sales')], frequency = 12)                     
      y = ses(vol_ts,
              h = 1,
              alpha = 0.9,
              initial = "simple",
              exponential=TRUE
      )                                                            
      y = as.data.frame(y)
      y=y$`Point Forecast`
      ass3 = y
      predicted_sales_df = data.frame(Agency = as.character(i),SKU = as.character(j), Sales = as.numeric(ass3))
      predicted_sales_lin = rbind(predicted_sales_lin,predicted_sales_df)
      cat("finished iteration : ",iter,'\n')
    }
    else{
      empty_SKU = c(empty_SKU, paste0(i, j))
    }
    iter=iter+1
  }
}

# -- Promotions-- #

iter =1
empty_SKU = c()

predicted_Promotions_df = data.frame(Agency = character(), 
                                     SKU = character(), 
                                     Promotions = numeric())
predicted_Promotions_lin = data.frame(Agency = character(),                        
                                      SKU = character(),
                                      Promotions = numeric())         

cat('Starting Simple Exponential Smoothing for Sales and Promotions...','\n','\n')
for(i in agency_list){
  cat('process starting... ',i,'\n')
  for(j in SKU_list)
  {
    cat("Modelling ", j, '\n')
    train = f_prc_sls[f_prc_sls$Agency == i,]
    train = train[train$SKU == j,]
    nn = nrow(train)
    cat(nn, '\n')
    if(nrow(train)>0){
      vol_ts = ts(train[, c('Promotions')], frequency = 12)                    
      y = ses(vol_ts,
              h = 1,
              alpha = 0.9,
              initial = "simple",
              exponential=TRUE
      )                                                              
      y = as.data.frame(y)
      y=y$`Point Forecast`
      ass3 = y
      predicted_Promotions_df=data.frame(Agency=as.character(i),SKU=as.character(j), Promotions=as.numeric(ass3))
      predicted_Promotions_lin = rbind(predicted_Promotions_lin,predicted_Promotions_df)
      cat("finished iteration : ",iter,'\n')
    }
    else{
      empty_SKU = c(empty_SKU, paste0(i, j))
    }
    iter=iter+1
  }
}

# -- Price-- #

iter =1
empty_SKU = c()

predicted_Price_df = data.frame(Agency = character(), 
                                SKU = character(), 
                                Price = numeric())
predicted_Price_lin = data.frame(Agency = character(),                        
                                 SKU = character(),
                                 Price = numeric())         

cat('Starting Simple Exponential Smoothing for Sales and Promotions...','\n','\n')
for(i in agency_list){
  cat('process starting... ',i,'\n')
  for(j in SKU_list)
  {
    cat("Modelling ", j, '\n')
    train = f_prc_sls[f_prc_sls$Agency == i,]
    train = train[train$SKU == j,]
    nn = nrow(train)
    cat(nn, '\n')
    if(nrow(train)>0){
      vol_ts = ts(train[, c('Price')], frequency = 12)                     
      y = ses(vol_ts,
              h = 1,
              alpha = 0.9,
              initial = "simple",
              exponential=TRUE
      )                                                              
      y = as.data.frame(y)
      y=y$`Point Forecast`
      ass3 = y
      predicted_Price_df = data.frame(Agency = as.character(i),SKU = as.character(j), Price = as.numeric(ass3))
      predicted_Price_lin = rbind(predicted_Price_lin,predicted_Price_df)
      cat("finished iteration : ",iter,'\n')
    }
    else{
      empty_SKU = c(empty_SKU, paste0(i, j))
    }
    iter=iter+1
  }
}

reco_data = merge(predicted_sales_lin, predicted_Promotions_lin, by = c("Agency", "SKU"), all.x = T, sort = FALSE)
reco_data = merge(reco_data, predicted_Price_lin, by = c("Agency", "SKU"), all.x = T, sort = FALSE)
reco_data = merge(reco_data, predicted_dff_lin, by = c("Agency", "SKU"), all.x = T, sort = FALSE)

# Now we will subset the Agencies lying in nearest neighbours list.
agency_nn = c("Agency_55", "Agency_60", "Agency_48", "Agency_57",
              "Agency_46", "Agency_56", "Agency_51")
reco_data = reco_data[reco_data$Agency %in% agency_nn, ]

# Now, we have got forecasted values of sales, promotions and volume of Agency-SKU Combinations for January 2018.
# Now, We calculate Profit by the below given formula:
#              
#          Profit = (Sales - Promotions)*Volume Dollars

reco_data$profit_without_price  = (reco_data$Sales - reco_data$Promotions)*reco_data$Volume
reco_data$profit_sales_only = ((reco_data$Sales)*reco_data$Volume)

# After sorting the values by profit in decreasing order we see SKU_01 & SKU_03 performing best for Agency_06 neighbours
# & SKU_01 & SKU_02 performing best for Agency_14
recommendations = c("SKU_03","SKU_01","SKU_01", "SKU_02")
f_sku_reco_test$SKU = recommendations
write.csv(f_sku_reco_test, "sku_recommendation.csv", row.names = FALSE)
#----------------------------------------------------------------------------------------------------------------------#

#Plotting SKU-Agency Combinations



# iter = 0
# setwd("/home/harshit/Desktop/Analytics/ABinBev/images/")
# for(i in agency_list[1:4]){
#   for (j in SKU_list[1:4]){
#     train = vol_data[vol_data$Agency == i,]
#     train = train[train$SKU == j,]
#     if(nrow(train) != 0){
#       myplot = ggplot(train, aes(YearMonth, Volume)) + geom_point()+ geom_line() + 
#                       scale_x_date('YearMonth',date_breaks = "1 month", date_labels = "%m")  + ylab("Volume") +
#                       xlab(paste(i,j,sep="-")) +theme(axis.text.x = element_text(size = 5, angle = 90))
#       ggsave(paste("Volumeplot_",i,"  ",j,".jpeg",sep=""),myplot)
#       cat("iter :", iter,"done")
#       iter= iter+1
#     }
#   }
# }
# agency_nn = c("Agency_55", "Agency_60", "Agency_48", "Agency_57", 
#               "Agency_46", "Agency_56", "Agency_51", "Agency_14", "Agency_06")
# ggplot(f_demo[f_demo$Agency %in% agency_nn,], aes(Avg_Population_2017, Avg_Yearly_Household_Income_2017))+
#        geom_point(aes(col = Agency))+ xlim(min(f_demo$Avg_Population_2017),max(f_demo$Avg_Population_2017))+ 
#        ylim(min(f_demo$Avg_Yearly_Household_Income_2017),max(f_demo$Avg_Yearly_Household_Income_2017))

#----------------------------------------------------------------------------------------------------------------------#
############################
##   Training - Testing   ##
############################

# train_data = vol_data[vol_data$YearMonth < as.Date("20171201", "%Y%m%d"),]
# test_data = vol_data[vol_data$YearMonth ==  as.Date("20171201", "%Y%m%d"),]
# 
# iter =1
# size=0
# 
# empty_SKU = c()
# 
# predicted_df = data.frame(Agency = character(), 
#                           SKU = character(), 
#                           Volume_pred = numeric())
# predicted_train_lin = data.frame(Agency = character(),                        
#                                SKU = character(),
#                                Volume_pred = numeric()) 
# 
# cat('Training : Starting Simple Exponential Smoothing...','\n','\n')
# for(i in agency_list){
#   cat('process starting... ',i,'\n')
#   for(j in SKU_list)
#   {
#     cat("Modelling ", j, '\n')
#     train = train_data[train_data$Agency == i,]
#     train = train[train$SKU == j,]
#     nn = nrow(train)
#     cat(nn, '\n')
#     if(nrow(train)>0){
#       vol_ts = ts(train[, c('Volume')], frequency = 12)                     
#       # Simple Exponential Smoothing
#       y = ses(vol_ts,biasadj = T,
#               h = 1,
#               alpha = 0.85,
#               initial = "simple",
#               exponential=TRUE
#       )                                                              
#       # # Random Walk Forecast
#       # y <- rwf(vol_ts, h = 1, level = c(80, 95, 99))
#       # # ETS
#       # y = ets(vol_ts, model = "ZZZ", allow.multiplicative.trend = F, alpha = 0.9)
#       # y = forecast(y, h=1)
#       y = as.data.frame(y)
#       y=y$`Point Forecast`
#       ass3 = y
#       cat("finished iteration : ",iter,'\n')
#     }
#     else{
#       ass3 = 0
#       empty_SKU = c(empty_SKU, paste0(i, j))
#     }
#     
#     predicted_df = data.frame(Agency = as.character(i),SKU = as.character(j), Volume_pred = as.numeric(ass3))
#     predicted_train_lin = rbind(predicted_train_lin,predicted_df)
#   }
#   iter=iter+1
# }
# final_data = merge(test_data, predicted_train_lin,by = c("SKU","Agency"), all.x = T, sort = F)
# loss = error(dat = final_data)
# cat("loss is : ", loss)
# 
# error = function(dat){
#   diff = sum(abs(dat$Volume - dat$Volume_pred))
#   test_tot = sum(abs(dat$Volume))
#   err = 1-(diff/test_tot)
#   return(err)
# }
#
#----------------------------------------------------------------------------------------------------------------------#

