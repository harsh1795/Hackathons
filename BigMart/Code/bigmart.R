'''
------------------------------------
|Author: Harshit Saxena(BlackViper)|
------------------------------------
'''

#---------------------------------------------------------------------------------------------------------#
# Setting ditectory
setwd("C:/Users/Harshit/Desktop/Analytics Competitions/AV/BigMart")

# Loading libraries
library(h2o)
h2o.init(nthreads=-1, max_mem_size = "4G")
h2o.removeAll()
#---------------------------------------------------------------------------------------------------------#
# Loading Data
df = read.csv("Train.csv")
test_df = read.csv("Test.csv")

new_df <- h2o.importFile(path = "Train.csv")
test_df <- h2o.importFile(path = "Test.csv")


View(new_df)
View(test_df)

nn = as.data.frame(new_df)
nn1 = as.data.frame(test_df)

#---------------------------------------------------------------------------------------------------------#
# Data Preprocessing

nn$Outlet_Size[is.na(nn$Outlet_Size) ==1] = 'Small'
nn1$Outlet_Size[is.na(nn1$Outlet_Size) ==1] = 'Small'
pred_weight = tapply(nn$Item_Weight, nn$Item_Type, mean,na.rm = T)
pred_weight1 = tapply(nn1$Item_Weight, nn1$Item_Type, mean,na.rm = T)
u = as.data.frame.array(pred_weight)
u1 = as.data.frame.array(pred_weight1)
u$Item_Type = rownames(u)
u1$Item_Type = rownames(u1)
rownames(u) = NULL
rownames(u1) = NULL

nn_19 = nn[nn$Outlet_Identifier=="OUT019",]
fin = merge(nn_19,y = u,by = 'Item_Type')
fin$Item_Weight = fin$pred_weight
fin$pred_weight = NULL
nn = nn[!(nn$Outlet_Identifier=="OUT019"),]
nn = rbind(nn,fin)

nn1_19 = nn1[nn1$Outlet_Identifier=="OUT019",]
fin1 = merge(nn1_19,y = u1,by = 'Item_Type')
fin1$Item_Weight = fin1$pred_weight1
fin1$pred_weight1 = NULL
nn1 = nn1[!(nn1$Outlet_Identifier=="OUT019"),]
nn1 = rbind(nn1,fin1)

nn_27 = nn[nn$Outlet_Identifier=="OUT027",]
fin = merge(nn_27,y = u,by = 'Item_Type')
fin$Item_Weight = fin$pred_weight
fin$pred_weight = NULL
nn = nn[!(nn$Outlet_Identifier=="OUT027"),]
nn = rbind(nn,fin)

nn1_27 = nn1[nn1$Outlet_Identifier=="OUT027",]
fin1 = merge(nn1_27,y = u1,by = 'Item_Type')
fin1$Item_Weight = fin1$pred_weight1
fin1$pred_weight1 = NULL
nn1 = nn1[!(nn1$Outlet_Identifier=="OUT027"),]
nn1 = rbind(nn1,fin1)

nn$year = 2016-nn$Outlet_Establishment_Year
nn1$year = 2016-nn1$Outlet_Establishment_Year
nn$price = nn$Item_MRP*nn$Item_Visibility
nn1$price = nn1$Item_MRP*nn1$Item_Visibility
nn$pp = nn$price*nn$year
nn1$pp = nn1$price*nn1$year
### Sales category index
nn$sales_cat = 4
nn$sales_cat[ nn$Item_MRP<70] = 1
nn$sales_cat[nn$Item_MRP>=70 & nn$Item_MRP<130] = 2
nn$sales_cat[nn$Item_MRP>=130 & nn$Item_MRP<200] = 3
nn1$sales_cat = 4
nn1$sales_cat[ nn1$Item_MRP<70] = 1
nn1$sales_cat[nn1$Item_MRP>=70 & nn1$Item_MRP<130] = 2
nn1$sales_cat[nn1$Item_MRP>=130 & nn1$Item_MRP<200] = 3
### Mean sales of each item
nn$Mean_Item_Sales = 0
for (i in 1:nrow(nn)){
  k = nn$Item_Identifier[i]
  l = mean(nn$Item_Outlet_Sales[nn$Item_Identifier == k,])
  nn$Mean_Item_Sales[i] = l
}

summary(nn[nn$sales_cat == 1,])
#---------------------------------------------------------------------------------------------------------#
# Data Modelling
rf1 <- h2o.randomForest(        
  training_frame = new_df,       
  #validation_frame = valid,      
  x=c(1:11,13,14,15,16),                      
  y=12,                      
  model_id = "rf_v1",          
  ntrees = 1500,
  mtries = 2,
  nbins = 1000,
  #stopping_rounds = 3,          
  score_each_iteration = T)
h2o.varimp_plot(rf1)

#---------------------------------------------------------------------------------------------------------#
# Data Prediction & Submission
finalRf_predictions<-h2o.predict(
  object = rf1
  ,newdata = test_df)

dat = h2o.cbind(test_df$Item_Identifier,test_df$Outlet_Identifier,finalRf_predictions$predict)
dat = as.data.frame(dat)
write.csv(dat,"final19.csv")

#---------------------------------------------------------------------------------------------------------#

