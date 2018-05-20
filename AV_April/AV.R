
#-----------------------------------#
#-  Harshit Saxena (Blackviper42)  -#
#-----------------------------------#

#--------------------------             Setting library          --------------------------#

setwd("/home/harshit/Desktop/Analytics/AV_April/")

library(data.table)
library(xgboost)
library(ggplot2)
library(reshape2)
library(lubridate)
library(caret)
library(pROC)
library(dplyr)
#library(lightgbm)
library(Matrix)

#----------------------------------------                data loading                                  --------------------------------#

train = fread("train_ajEneEa.csv")
test = fread("test_v2akXPA.csv")
sample_submission = fread("sample_submission_1.csv")

#---------------------------------------                    Data Cleaning                            -----------------------------------#

train$gender = as.factor(train$gender)
train$ever_married = as.factor(train$ever_married)
train$work_type = as.factor(train$work_type)
train$Residence_type = as.factor(train$Residence_type)

train$smoking_status[train$smoking_status == ""] = "unknown"
train$smoking_status = as.factor(train$smoking_status)

test$gender = as.factor(test$gender)
test$ever_married = as.factor(test$ever_married)
test$work_type = as.factor(test$work_type)
test$Residence_type = as.factor(test$Residence_type)
test$smoking_status[test$smoking_status == ""] = "unknown"
test$smoking_status = as.factor(test$smoking_status)


#---------------------------------------                  Feature Generation                 -----------------------------------------#

# gender hit rate

b = table(train$gender, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("gender", "gender_p0", "gender_p1", "total")
b$total = NULL
b$gender_p0 = NULL


train = merge(train, b, by = "gender", all.x = T, sort = F)
test = merge(test,  b, by = "gender", all.x = T, sort = F)

# age

## age category

train$age_category = "child"
train$age_category[train$age>18] = "men"
train$age_category[train$age>=60] = "old"

test$age_category = "child"
test$age_category[test$age>18] = "men"
test$age_category[test$age>=60] = "old"

## age_category hit rate

b = table(train$age_category, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("age_category", "age_category_p0", "age_category_p1", "total")
b$total = NULL
b$age_category_p0 = NULL

train = merge(train, b, by = "age_category", all.x = T, sort = F)
test = merge(test,  b, by = "age_category", all.x = T, sort = F)

## hypertension hit rate

b = table(train$hypertension, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("hypertension", "hypertension_p0", "hypertension_p1", "total")
b$total = NULL
b$hypertension_p0 = NULL
train$hypertension = as.factor(train$hypertension)
test$hypertension = as.factor(test$hypertension)
train = merge(train, b, by = "hypertension", all.x = T, sort = F)
test = merge(test,  b, by = "hypertension", all.x = T, sort = F)

## heart_disease hit rate

b = table(train$heart_disease, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("heart_disease", "heart_disease_p0", "heart_disease_p1", "total")
b$total = NULL
b$heart_disease_p0 = NULL
train$heart_disease = as.factor(train$heart_disease)
test$heart_disease = as.factor(test$heart_disease)
train = merge(train, b, by = "heart_disease", all.x = T, sort = F)
test = merge(test,  b, by = "heart_disease", all.x = T, sort = F)

## ever_married hit rate

b = table(train$ever_married, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("ever_married", "ever_married_p0", "ever_married_p1", "total")
b$total = NULL
b$ever_married_p0 = NULL

train = merge(train, b, by = "ever_married", all.x = T, sort = F)
test = merge(test,  b, by = "ever_married", all.x = T, sort = F)

## work_type hit rate

b = table(train$work_type, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("work_type", "work_type_p0", "work_type_p1", "total")
b$total = NULL
b$work_type_p0 = NULL

train = merge(train, b, by = "work_type", all.x = T, sort = F)
test = merge(test,  b, by = "work_type", all.x = T, sort = F)


## Residence_type hit rate

b = table(train$Residence_type, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("Residence_type", "Residence_type_p0", "Residence_type_p1", "total")
b$total = NULL
b$Residence_type_p0 = NULL

train = merge(train, b, by = "Residence_type", all.x = T, sort = F)
test = merge(test,  b, by = "Residence_type", all.x = T, sort = F)


## smoking_status hit rate

b = table(train$smoking_status, train$stroke)
b = as.matrix(b)
b = as.data.frame(b)
b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
colnames(b) = c("smoking_status", "smoking_status_p0", "smoking_status_p1", "total")
b$total = NULL
b$smoking_status_p0 = NULL

train = merge(train, b, by = "smoking_status", all.x = T, sort = F)
test = merge(test,  b, by = "smoking_status", all.x = T, sort = F)

#----------------------------------                   Missing Values                  -----------------------------------#
## Mean Imputation
train$bmi[is.na(train$bmi) == T] = mean(train$bmi, na.rm = T)
test$bmi[is.na(test$bmi) == T] = mean(test$bmi, na.rm = T)


#----------------------------------                   Data Modelling                  -----------------------------------#











