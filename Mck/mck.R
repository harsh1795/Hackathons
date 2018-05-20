#
# Author: Harshit Saxena
#


setwd("/home/harshit/Desktop/Analytics/Mck")

# Loading Libraries
library(data.table)
library(xgboost)
library(ggplot2)
library(caret)
library(DescTools)
library(lubridate)

# Loading data
train = fread("train.csv", na.strings = "")
test = fread("test.csv", na.strings = "")



# Data Combining
label = as.factor(train$Approved)
combi = rbind(train[,-c("Approved")], test)

# Data Preprocessing
combi$Gender = as.factor(combi$Gender)
combi$DOB = as.Date(combi$DOB,format = "%d/%m/%y")
combi$DOB = as.Date(ifelse(combi$DOB > "2017-12-31", format(combi$DOB, "19%y-%m-%d"), format(combi$DOB)))
combi$DOB[is.na(combi$DOB) == TRUE] = mean(combi$DOB, na.rm = TRUE)
combi$Lead_Creation_Date = as.Date(combi$Lead_Creation_Date, format = "%d/%m/%y")
combi$age_at_lead = year(combi$Lead_Creation_Date) - year(combi$DOB)

combi$City_Code[is.na(combi$City_Code) == TRUE] = "C10001"
citycodetable = as.data.frame(Desc(combi$City_Code)[[1]][[14]])
citycodetable = citycodetable[,c(1,3)]
colnames(citycodetable) = c("City_Code", "City_Code_Perc")
combi = merge(combi, citycodetable, by = "City_Code", all.x = TRUE,  sort = FALSE)

combi$City_Category[is.na(combi$City_Category) == TRUE] = "A"     ## Can try creating missing category 
combi$Employer_Category1[is.na(combi$Employer_Category1) == TRUE] = "A"
## Dropping Employer_Code

combi$Employer_Category2[is.na(combi$Employer_Category2) == TRUE] = 3.723
combi$Monthly_Income[is.na(combi$Monthly_Income) == TRUE] = 2500
combi$Customer_Existing_Primary_Bank_Code[is.na(combi$Customer_Existing_Primary_Bank_Code) == TRUE] = "B001"
combi$Primary_Bank_Type[is.na(combi$Primary_Bank_Type) == TRUE] = "P"
combi$Existing_EMI[is.na(combi$Existing_EMI) == TRUE] = 357
combi$Loan_Amount[is.na(combi$Loan_Amount)==TRUE] = 39446
combi$Loan_Period[is.na(combi$Loan_Period) == TRUE] = 4


# Converting Factors
combi$City_Code = as.factor(combi$City_Code)
combi$Gender = as.factor(combi$Gender)
combi$City_Category = as.factor(combi$City_Category)
combi$Employer_Code = as.factor(combi$Employer_Code)
combi$Employer_Category1 = as.factor(combi$Employer_Category1)
combi$Employer_Category2 = as.factor(combi$Employer_Category2)
combi$Customer_Existing_Primary_Bank_Code = as.factor(combi$Customer_Existing_Primary_Bank_Code)
combi$Primary_Bank_Type = as.factor(combi$Primary_Bank_Type)
combi$Contacted = as.factor(combi$Contacted)
combi$Source = as.factor(combi$Source)
combi$Source_Category = as.factor(combi$Source_Category)
combi$Var1 = as.factor(combi$Var1)

new_train = combi[1:69713, ]
new_test = combi[69714:99750, ]
new_train$target = label

# Modelling
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "28G")

trainh2o = as.h2o(new_train)
testh2o = as.h2o(new_test)

rf1 = h2o.randomForest(x = c(1,3:23),
                       y = 24,
                       training_frame = trainh2o,
                       nfolds = 5,
                       balance_classes = TRUE,
                       ntrees = 200,
                       max_depth = 10,
                       stopping_metric = "AUC",
                       mtries = 5,
                       col_sample_rate_per_tree = 0.9 )

h2o.varimp_plot(rf1)
rf1

##GBM
gbm_params2 <- list(learn_rate = seq(0.01, 0.1, 0.01),
                    max_depth = seq(2, 10, 1),
                    sample_rate = seq(0.5, 1.0, 0.1),
                    col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 36)

# Train and validate a grid of GBMs
gbm_grid2 <- h2o.grid("gbm", x = c(1,3:23), y = 24,
                      grid_id = "gbm_grid2",
                      training_frame = trainh2o,
                      balance_classes = TRUE,
                      ntrees = 200,
                      seed = 1,
                      hyper_params = gbm_params2,
                      search_criteria = search_criteria2)

gbm_gridperf2 <- h2o.getGrid(grid_id = "gbm_grid2", 
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf2)

gbm_params <- list(learn_rate = seq(0.01, 0.1, 0.01),
                   max_depth = seq(5, 15, 1),
                   sample_rate = seq(0.8, 1.0, 0.05),  #updated
                   col_sample_rate = seq(0.1, 1.0, 0.1))
search_criteria <- list(strategy = "RandomDiscrete" 
                        )  #updated


gbm_grid <- h2o.grid("gbm", x = c(1,3:23), y = 24,
                     grid_id = "gbm_grid2",
                     training_frame = trainh2o,
                     ntrees = 200,
                     seed = 1,
                     hyper_params = gbm_params,
                     search_criteria = search_criteria2)

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid2", 
                            sort_by = "auc", 
                            decreasing = TRUE)
print(gbm_gridperf)

best_gbm_model_id <- gbm_gridperf@model_ids[[1]]
best_gbm <- h2o.getModel(best_gbm_model_id)

predictions <- as.data.frame(h2o.predict(object = best_gbm,
                                 newdata = testh2o))










predictions = as.data.frame(h2o.predict(object = rf1, newdata = testh2o))
submit = as.data.frame(new_test$ID)
submit$Approved = predictions$predict  
colnames(submit) = c("ID", "Approved")

write.csv(submit, "gbm gridsearch.csv", row.names = FALSE)






















































