
#-----------------------------------#
#-  Harshit Saxena (Blackviper42)  -#
#-----------------------------------#

#--------------------------             Setting library          --------------------------#

setwd("/home/harshit/Desktop/Analytics/AV_Lord_of_Machines/")

library(data.table)
library(xgboost)
library(ggplot2)
library(reshape2)
library(lubridate)
library(caret)
library(stringr)
library(tm)
library(text2vec)
library(SnowballC)
library(wordcloud)
library(pROC)
library(dplyr)

#----------------------------------------                data loading                                  --------------------------------#

train = fread("train.csv")
test = fread("test_BDIfz5B.csv")
campaign_data  = fread("campaign_data.csv")
sample_submission = fread("sample_submission_4fcZwvQ.csv")

#---------------------------------------                    Data Cleaning                            -----------------------------------#

## Train data
train$is_open = as.factor(train$is_open)
train$is_click = as.factor(train$is_click)
train$send_day = as.vector(sapply(X = train$send_date ,FUN = function(x){strsplit(x,split = " ")[[1]][1]}))
train$send_time = as.vector(sapply(X = train$send_date ,FUN = function(x){strsplit(x,split = " ")[[1]][2]}))
train$send_hour = as.vector(sapply(X = train$send_time ,FUN = function(x){strsplit(x,split = ":")[[1]][1]}))
train$send_min = as.vector(sapply(X = train$send_time ,FUN = function(x){strsplit(x,split = ":")[[1]][2]}))
train$send_time = NULL
train$send_dat = as.vector(sapply(X = train$send_day ,FUN = function(x){strsplit(x,split = "-")[[1]][1]}))
train$send_month = as.vector(sapply(X = train$send_day ,FUN = function(x){strsplit(x,split = "-")[[1]][2]}))
train$send_day = as.Date(train$send_day, format = "%d-%m-%Y")


## Test data
test$send_day = as.vector(sapply(X = test$send_date ,FUN = function(x){strsplit(x,split = " ")[[1]][1]}))
test$send_time = as.vector(sapply(X = test$send_date ,FUN = function(x){strsplit(x,split = " ")[[1]][2]}))
test$send_hour = as.vector(sapply(X = test$send_time ,FUN = function(x){strsplit(x,split = ":")[[1]][1]}))
test$send_min = as.vector(sapply(X = test$send_time ,FUN = function(x){strsplit(x,split = ":")[[1]][2]}))
test$send_time = NULL
test$send_dat = as.vector(sapply(X = test$send_day ,FUN = function(x){strsplit(x,split = "-")[[1]][1]}))
test$send_month = as.vector(sapply(X = test$send_day ,FUN = function(x){strsplit(x,split = "-")[[1]][2]}))
test$send_day = as.Date(test$send_day, format = "%d-%m-%Y")

## Combining for rank feature
data = rbind(train[,c("id", "user_id", "send_day")], test[,c("id", "user_id", "send_day")])
data = data %>% group_by(user_id) %>%
  mutate(rank = rank(send_day)) %>%
  arrange(send_day)

data$user_id = NULL
data$send_day = NULL
data_tr = data[1:nrow(train),]
data_ts = data[(nrow(train)+1):nrow(data),]

train = merge(train, data_tr, by = "id", all.x = T, sort = F)
test = merge(test, data_ts, by = "id", all.x = T, sort = F)

## Train data
train$send_wday = wday(train$send_day)
train$send_weekend = 0
train$send_weekend[train$send_wday == 7] = 1
train$send_weekend[train$send_wday == 1] = 1
train$send_date = NULL
train$send_day = NULL

## Test data
test$send_wday = wday(test$send_day)
test$send_weekend = 0
test$send_weekend[test$send_wday == 7] = 1
test$send_weekend[test$send_wday == 1] = 1
test$send_date = NULL
test$send_day = NULL



#----------------------------------             Text Analytics           ------------------------------------#
## Campaign data

campaign_data$communication_type = as.factor(campaign_data$communication_type)

campaign_data$sub_word_count = str_count(campaign_data$subject,pattern = "\\w+")
campaign_data$email_word_count = str_count(campaign_data$email_body,pattern = "\\w+")

campaign_data$sub_word_count_full = str_count(campaign_data$subject)
campaign_data$email_word_count_full = str_count(campaign_data$email_body)

# n-gram model for subject

prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(campaign_data$subject, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = campaign_data$campaign_id, 
                  progressbar = FALSE)

stop_words = c("i", "me", "my", "on", "at", "is", "we", "our", "ours", "in", "you",
               "your", "yours", "and", "for", "with", "the", "to", "a", "s")

vocab = create_vocabulary(it_train, ngram = c(1L, 2L),stopwords = stop_words)
vocab = prune_vocabulary(vocab, term_count_min = 3, doc_proportion_min = 0.05)

bigram_vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, bigram_vectorizer)
dtm_train = as.data.frame.matrix(dtm_train)

dtm_train$campaign_id = rownames(dtm_train)
rownames(dtm_train) = NULL
dtm_train$campaign_id = as.numeric(dtm_train$campaign_id)
campaign_data = merge(campaign_data, dtm_train, by = "campaign_id", all.x = T, sort = F)

campaign_data$email_body = NULL
campaign_data$subject  = NULL
campaign_data$email_url = NULL

colnames(campaign_data)[11:90] = paste0("s_", colnames(campaign_data)[11:90])


# n-gram model for email_body

# prep_fun = tolower
# tok_fun = word_tokenizer
# 
# it_train = itoken(campaign_data$email_body, 
#                   preprocessor = prep_fun, 
#                   tokenizer = tok_fun, 
#                   ids = campaign_data$campaign_id, 
#                   progressbar = FALSE)
# 
# stop_words = c("i", "me", "my", "on", "at", "is", "we", "our", "ours", "in", "you",
#                "your", "yours", "and", "for", "with", "the", "to", "a", "s", "if", "what",
#                "of", "this", "are", "be", "all", "dear", "have", "can", "an", "will", "as")
# 
# vocab = create_vocabulary(it_train, ngram = c(1L, 2L),stopwords = stop_words)
# vocab = prune_vocabulary(vocab, term_count_min = 2, doc_proportion_min = 0.3)
# 
# bigram_vectorizer = vocab_vectorizer(vocab)
# dtm_train = create_dtm(it_train, bigram_vectorizer)
# dtm_train = as.data.frame.matrix(dtm_train)
# 
# dtm_train$campaign_id = rownames(dtm_train)
# rownames(dtm_train) = NULL
# dtm_train$campaign_id = as.numeric(dtm_train$campaign_id)
# campaign_data = merge(campaign_data, dtm_train, by = "campaign_id", all.x = T, sort = F)



## Campaign-NLP
# campaign_data_nlp = fread("Campaign_NLP.csv")
# 
# campaign_data_nlp$communication_type = as.factor(campaign_data_nlp$communication_type)
# 
# campaign_data_nlp$sub_word_count = str_count(campaign_data_nlp$subject,pattern = "\\w+")
# campaign_data_nlp$email_word_count = str_count(campaign_data_nlp$email_body,pattern = "\\w+")
# 
# campaign_data_nlp$sub_word_count_full = str_count(campaign_data_nlp$subject)
# campaign_data_nlp$email_word_count_full = str_count(campaign_data_nlp$email_body)
# 
# campaign_data = campaign_data_nlp



train = merge(train, campaign_data, by = "campaign_id", all.x = T, sort = F)
test = merge(test, campaign_data, by = "campaign_id", all.x = T, sort = F)




a0 = which(colSums(test[,c(20:99)]) == 0)
a0 = as.vector(a0)
a0 = a0+19
at = a0+2
train[,at] = NULL
test[,a0] = NULL

#-----------------------------              Feature Engineering       -------------------------------------------------#



train$send_month = as.numeric(train$send_month)
train$send_hour = as.numeric(train$send_hour)
train$send_min = as.numeric(train$send_min)
train$send_dat = as.numeric(train$send_dat)

test$send_month = as.numeric(test$send_month)
test$send_hour = as.numeric(test$send_hour)
test$send_min = as.numeric(test$send_min)
test$send_dat = as.numeric(test$send_dat)



b = table(train$communication_type, train$is_click)
b = as.matrix(b)
b = as.data.frame(b)

b = dcast(b, Var1 ~ Var2,fill = 0)
b$total = b$`0` + b$`1`
b$`0` = b$`0`/b$total
b$`1` = b$`1`/b$total
b$`1` = b$`1`*100
b$`0` = b$`0`*100
b$`0` = round(b$`0`, digits = 2)
b$`1` = round(b$`1`, digits = 2)


colnames(b) = c("communication_type", "p_0", "ct_p1", "total")
b$cp_1 = b$ct_p1/sum(b$ct_p1)
b$total = NULL
b$p_0 = NULL


train = merge(train, b, by = "communication_type", all.x = T, sort = F)
test = merge(test,  b, by = "communication_type", all.x = T, sort = F)

# is_open
e = table(train$user_id, train$is_open)
e = as.data.frame.matrix(e)

e$user_id = rownames(e)
rownames(e) = NULL
colnames(e) = c("no_0", "no_1", "user_id")
e$avg_open_1 = e$no_1/(e$no_0 +e$no_1)
e$user_id = as.numeric(e$user_id)

train = merge(train, e, by = "user_id",all.x = T,sort = F)
test = merge(test, e, by = "user_id",all.x = T,sort = F)


# is_click mean prediction
d = table(train$user_id, train$is_click)
d = as.data.frame.matrix(d)

d$user_id = rownames(d)
rownames(d) = NULL
colnames(d) = c("n_0", "n_1", "user_id")
d$avg_1 = d$n_1/(d$n_0 +d$n_1)
d$user_id = as.numeric(d$user_id)
train = merge(train, d, by = "user_id",all.x = T,sort = F)
test = merge(test, d, by = "user_id",all.x = T,sort = F)

#-----------------------------------------------       Is_open Predictions           ---------------------------------------------------------#



test[is.na(test)] = 0


setDT(train)
setDT(test)

train$is_open = as.character(train$is_open)
train$is_open = as.numeric(train$is_open)

label = train$is_open

natest = test[is.na(test$avg_1) == T,]
nontest = test[is.na(test$avg_1) != T,]
new_tr <- model.matrix(~.+0,data = train[,-c("id", "campaign_id", "user_id", "is_click", "is_open", "send_month",
                                                   "total_links", "no_of_internal_links", "no_of_images", "no_of_sections",
                                                   "n_0", "n_1", "avg_1", "email_word_count", "email_word_count_full"),with=F]) 
new_ts <- model.matrix(~.+0,data = nontest[,-c("id", "campaign_id", "user_id", "send_month",
                                                  "total_links", "no_of_internal_links", "no_of_images", "no_of_sections",
                                                   "n_0", "n_1", "avg_1", "email_word_count", "email_word_count_full"),with=F])


da = xgb.DMatrix(data = new_tr,label = label) 
dtest = xgb.DMatrix(data = new_ts)

params <- list(booster = "gbtree",
               objective = "binary:logistic",
               eta=0.1,
               gamma=0,
               max_depth=6,
               min_child_weight=1,
               subsample=0.75, 
               colsample_bytree=0.8)

xgbcv <- xgb.cv( params = params,
                 metrics = "auc",
                 data = da,
                 nrounds = 200,
                 nfold = 5, 
                 #showsd = T, 
                 #stratified = T, 
                 print_every_n = 10, 
                 maximize = T)

eval = xgbcv$evaluation_log
min_rounds = which.max(eval$test_auc_mean)
min_rounds

xgb1 <- xgb.train (params = params,
                   metrics = "auc",
                   data = da, 
                   nrounds = min_rounds, 
                   print_every_n = 10,
                   maximize = T , 
                   eval_metric = "auc")

xgbpred <- predict (xgb1,dtest)
train_pred = predict(xgb1, da)

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])


roc_training <- roc(label, train_pred, algorithm = 2)
plot(roc_training )   
auc(roc_training)



xgbpred = ifelse(xgbpred>0.08, 1, 0)
xgbpred2 = ifelse(xgbpred>0.4, 1, 0)
test$is_open_1 = xgbpred
test$is_open_2 = xgbpred2
nontest$is_open_1 = xgbpred
natest$is_open_1 = NA

test2 = rbind(nontest, natest)
# train.train = train[train$send_month <= 9,]
# train.test = train[train$send_month > 9, ]
# a0 = which(colSums(test[,c(25:104)]) == 0)
# a0 = as.vector(a0)
# a0 = a0+24
# train.train[,a0] = NULL
# train.test[,a0] = NULL
# setDT(train.train)
# setDT(train.test)
# train.train$is_open = as.character(train.train$is_open)
# train.train$is_open = as.numeric(train.train$is_open)
# label = train.train$is_open
# ts_label = train.test$is_open
# roc_test <- roc(ts_label, xgbpred, algorithm = 2)
# plot(roc_test ) 
# auc(roc_test )
# table(train.test$is_open, xgbpred)



#------------------------------------------------     is_click by mean predictions       ------------------------------------------------#
opentrain = train
h = table(opentrain$rank, opentrain$is_click)
h = as.data.frame.matrix(h)
h$rank = rownames(h)
h$rank = as.numeric(h$rank)
h$click_rate = h$`1`/(h$`0`+h$`1`)
h = h[h$rank<=17,]

fit1 = lm(formula = click_rate~rank, data = h)
h_t = test[,c("rank")]
h_p = predict(fit1, h_t)
h_t$click_rate = h_p

h_t = h_t[!duplicated(h_t), ]

final = test[,c("id", "avg_1", "is_open_1", "rank")]
final$avg_1[final$is_open_1 == 1 & final$avg_1 <0.1] = 0.05
final$avg_1[final$is_open_2 == 0 & final$avg_1 >0.2] = 0.121

# final$click_rate=  NA
# final$click_rate = h_t$click_rate[match(final$rank, h_t$rank)]
final = merge(final, h_t, by = "rank", all.x = T, sort=F)
finalna = final[is.na(final$avg_1),]
finalnotna = final[is.na(final$avg_1)!=T,]
finalna = finalna[,c("id", "click_rate")]
colnames(finalna) = c("id", "is_click")
finalnotna = finalnotna[,c("id", "avg_1")]
colnames(finalnotna) = c("id", "is_click")
finalmax = rbind(finalnotna, finalna)


# final$avg_1[is.na(final$avg_1)==T & final$communication_type == "Corporate"] = 0.0097+0.11
# final$avg_1[is.na(final$avg_1)==T & final$communication_type == "Hackathon"] = 0.03184+0.11
# final$avg_1[is.na(final$avg_1)==T & final$communication_type == "Newsletter"] = 0.01254+0.11
# final$avg_1[is.na(final$avg_1)==T & final$communication_type == "Upcoming Events"] = 0.01374+0.11


final$avg_1[is.na(final$avg_1)==T] = 0.112

final$is_open_1 = NULL
final$is_open_2 = NULL
colnames(final) = c("id", "is_click")

write.csv(finalmax, "mean_prediction_with_openxgb_0.08_0.4_avg<0.1=0.112_na_rank.csv", row.names = F)





#--------------------------------------           is_click predictions              -------------------------------------------------------#


a0 = which(colSums(test[,c(23:102)]) == 0)
a0 = as.vector(a0)
a0 = a0+22
at = a0+2

train[,at] = NULL
test[,a0] = NULL

test$n_1[is.na(test$n_1)==T] = 0.01
test$avg_1[is.na(test$avg_1)==T] = 0.01
test$n_0[is.na(test$n_0)==T] = 7

new_test = test[is.na(test$avg_1) == T,]

setDT(train)
setDT(test)
setDT(new_test)

train$is_click = as.character(train$is_click)
train$is_click = as.numeric(train$is_click)

label = train$is_click


new_tr <- model.matrix(~.+0,data = train[,-c("id", "campaign_id", "user_id", "is_click", "is_open", "send_month",
                                             "email_body", "subject", "email_url", "avg_1", "n_1", "n_0"),with=F]) 
new_ts <- model.matrix(~.+0,data = test[,-c("id", "campaign_id", "user_id", "send_month",
                                            "email_body", "subject", "email_url"),with=F])
new_tst <- model.matrix(~.+0,data = new_test[,-c("id", "campaign_id", "user_id", "send_month",
                                            "email_body", "subject", "email_url", "avg_1", "n_1", "n_0"),with=F])


da = xgb.DMatrix(data = new_tr,label = label) 
dtest = xgb.DMatrix(data = new_tst)

params <- list(booster = "gbtree",
               objective = "binary:logistic",
               eta=0.1,
               gamma=0,
               max_depth=6,
               min_child_weight=1,
               subsample=0.75, 
               colsample_bytree=0.8)

xgbcv <- xgb.cv( params = params,
                 metrics = "auc",
                 data = da,
                 nrounds = 200,
                 nfold = 5, 
                 #showsd = T, 
                 #stratified = T, 
                 print_every_n = 10, 
                 maximize = T)

eval = xgbcv$evaluation_log
min_rounds = which.max(eval$test_auc_mean)
min_rounds

xgb1 <- xgb.train (params = params,metrics = "auc",
                   data = da, 
                   nrounds = min_rounds, 
                   print_every_n = 10,
                   maximize = T , 
                   eval_metric = "auc")

xgbpred <- predict (xgb1,dtest)
train_pred = predict(xgb1, da)

mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])

new_test$is_click = xgbpred


final2 = new_test[,c("id", "is_click")]
final2$is_click = 0.01+final2$is_click
write.csv(final, "xgb_with_nlp.csv", row.names = F)

fin = rbind(final, final2)
write.csv(fin, "xgb_average_0.01_add.csv", row.names = F)























#-------------------------------------------                 Ensembling            -----------------------------------------------------------#


sol1 = fread("mean_prediction.csv")
sol2 = fread("glm_alpha_05.csv")

sol = cbind(sol1, sol2)
sol[,3] = NULL
colnames(sol) = c("id", "a", "b")
sol$b = as.numeric(sol$b)
sol$is_click = (sol$a + sol$b) / 2
sol$a = NULL
sol$b = NULL  

write.csv(sol, "genensemble.csv", row.names = F)





#--------------------------------------------------------------------------------------------------------------------------------------------
file = fread("best_mean_prediction_with_openxgb_0.08_avg<0.1=0.05_na_0.112.csv")



tail(names(sort(table(file$is_click))), 5)




#---------------------------------------------------------------------------------------------------------------------------------------------
## Best Solution

xgbpred <- predict (xgb1,dtest)
xgbpred = ifelse(xgbpred>0.08, 1, 0)

prev_test = test[is.na(test$avg_1)==F,]

final = prev_test[,c("id", "avg_1")]
final$avg_1[final$is_open == 1 & final$avg_1 <0.1] = 0.09
final[is.na(final)] = 0.112
final$is_open = NULL
colnames(final) = c("id", "is_click")


#-------------------------------------------                       Trash                     ------------------------------------------------#



#------------------------------------         Mean Prediction           --------------------------------------#
# a = train
# a = a[a$communication_type != "Conference",]
# a = a[a$communication_type != "Others",]
# a = a[a$communication_type != "Webinar",]
# 
# # is_click
# d = table(a$user_id, a$is_click)
# d = as.data.frame.matrix(d)
# 
# d$user_id = rownames(d)
# rownames(d) = NULL
# colnames(d) = c("n_0", "n_1", "user_id")
# d$avg_1 = d$n_1/(d$n_0 +d$n_1)
# d$user_id = as.numeric(d$user_id)
# a = merge(a, d, by = "user_id",all.x = T,sort = F)
# test = merge(test, d, by = "user_id",all.x = T,sort = F)
# 
# # is_open
# e = table(a$user_id, a$is_open)
# e = as.data.frame.matrix(e)
# 
# e$user_id = rownames(e)
# rownames(e) = NULL
# colnames(e) = c("no_0", "no_1", "user_id")
# e$avg_open_1 = e$no_1/(e$no_0 +e$no_1)
# e$user_id = as.numeric(e$user_id)
# a = merge(a, e, by = "user_id",all.x = T,sort = F)
# test = merge(test, e, by = "user_id",all.x = T,sort = F)
# 
# test[is.na(test)] = 0
#------------------------------------            Data Modelling           ------------------------------------#

#-------------------------------------        XGBOOST         -----------------------------------------------#
# setDT(a)
# setDT(test)
# 
# a$is_click = as.character(a$is_click)
# a$is_click = as.numeric(a$is_click)
# label = a$is_click
# 
# new_tr <- model.matrix(~.+0,data = a[,-c("id", "campaign_id", "is_click"),with=F]) 
# new_ts <- model.matrix(~.+0,data = test[,-c("id",  "campaign_id"),with=F])
# 
# 
# da = xgb.DMatrix(data = new_tr,label = label) 
# dtest = xgb.DMatrix(data = new_ts)
# 
# params <- list(booster = "gbtree",
#                metrics = "auc",
#                objective = "binary:logistic",
#                eta=0.01,
#                gamma=2,
#                max_depth=10,
#                min_child_weight=1,
#                subsample=0.75, 
#                colsample_bytree=0.8)
# 
# xgbcv <- xgb.cv( params = params,
#                  data = da,
#                  nrounds = 100,
#                  nfold = 5, 
#                  showsd = T, 
#                  stratified = T, 
#                  print_every_n = 10, 
#                  maximize = F)
# eval = xgbcv$evaluation_log
# min_rounds = which.min(eval$test_error_mean)
# min_rounds
# xgb1 <- xgb.train (params = params,
#                    data = da, 
#                    nrounds = min_rounds, 
#                    print_every_n = 10,
#                    maximize = F , 
#                    eval_metric = "auc")
# 
# xgbpred <- predict (xgb1,dtest)
# 
# 
# mat <- xgb.importance (feature_names = colnames(new_tr),model = xgb1)
# xgb.plot.importance (importance_matrix = mat[1:20])
# 
# test$is_click = xgbpred
# 
# summary(xgbpred)
# 


#-----------------------------------               Predictions            --------------------------------------#

## Mean Prediction
# 
# final = test[,c("id", "avg_1")]
# final[is.na(final)] = 0.5
# colnames(final) = c("id", "is_click")
# 
# write.csv(final, "mean_prediction.csv", row.names = F)
# 
# 
# 
# 
# ## XGBOOST Prediction
# final = test[,c("id", "is_click")]
# 
# write.csv(final, "xgb_bigram_depth-6_eta-001.csv", row.names = F)





#---------------------------------              GEN            ---------------------------------------#
# library(ggplot2)  
# library(ggcorrplot)
# 
# ggplot(a, aes(send_dat, is_click)) + geom_count() 
# 
# 
# library(h2o)
# h2o.init(nthreads = -1, max_mem_size = "28G")
# 
# a$is_click = as.factor(a$is_click)
# test$is_click = NULL
# trainh2o = as.h2o(a)
# testh2o = as.h2o(test)
# 
# glm1 = h2o.glm(x = c(1,2,6:84),family = "binomial",
#         y = 5,
#         training_frame = trainh2o,
#         nfolds = 10,
#         balance_classes = F,
#         alpha = 0.9,
#         solver = "IRLSM"
#         )
# 
# pred = h2o.predict(glm1, testh2o)
# pred = as.data.frame(pred)
# final = test
# final$is_click = pred$predict
# 
# 
# final = final[,c("id", "is_click")]
# 
# write.csv(final, "glm_alpha_05.csv", row.names = F)
