#######################################
#    Author : Harshit Saxena          #
#   (https://github.com/harsh1795)    #
#######################################

#-------------------------------------------------------------------------------------------------------------------------#
# Setting Working Directory
setwd('C:/Users/Harshit/Desktop/Analytics Competitions/ML_HACKEREARTH')

#-------------------------------------------------------------------------------------------------------------------------#
# Importing Libraries and Data
library(data.table)
library(ggplot2)
library(readr)
library(DescTools)
library(tm)
library(RColorBrewer)
library(wordcloud)

library(h2o)
h2o.init(nthreads = -1,max_mem_size = '4G')

# Importing data
train = fread('train_indessa.csv', stringsAsFactors = TRUE,na.strings = "")
test = fread('test_indessa.csv', stringsAsFactors = TRUE,na.strings = "")
df1 <- fread('train_indessa.csv', stringsAsFactors = TRUE,na.strings = "")
train_1 = fread('train_1.csv', stringsAsFactors = TRUE,na.strings = "NA")
train_1 <- read_csv("C:/Users/Harshit/Desktop/Analytics Competitions/ML_HACKEREARTH/train_indessa.csv",
                    col_types = cols(grade = col_factor(levels = c("A","B", "C", "D", "E", "F", 
                    "G")), loan_status = col_factor(levels = c("0", "1")),
                    term = col_factor(levels = c("36 months","60 months"))))
test = read_csv("C:/Users/Harshit/Desktop/Analytics Competitions/ML_HACKEREARTH/test_indessa.csv",
                col_types = cols(grade = col_factor(levels = c("A","B", "C", "D", "E", "F", 
                      "G")), loan_status = col_factor(levels = c("0", "1")),
                      term = col_factor(levels = c("36 months","60 months"))))

#-------------------------------------------------------------------------------------------------------------------------#
# Data Preprocessing

str(train)
predictor_var = train$loan_status
train$loan_status=NULL
c <- list(train, test)
combin = rbindlist(c)
table(combin$term)
ggplot(combin, aes(int_rate, fill = term)) + geom_bar()
summary(combin$batch_enrolled)

## dropping batch_enrolled variable
combin$batch_enrolled=NULL
n_rows = nrow(combin)
k1 = c(1:100)
frq =  function(x){
  b = a[a$Var1==gh$emp_title[x],]
  b = b$Freq
  return(b)
}

o1 = sapply(k1,frq)

## doing undersampling of big data into 3 parts
## by dividing negative class into 3 parts and
## then running 3 models.

df1_positive_dataset = df1[df1$loan_status==1,]
df1_negative_dataset = df1[df1$loan_status==0,]
splitSample <- sample(1:3, size=nrow(df1_negative_dataset), prob=c(0.33,0.33,0.34),replace = TRUE)
df1_negative_dataset_1 <- df1_negative_dataset[splitSample==1,]  
df1_negative_dataset_2 <- df1_negative_dataset[splitSample==2,] 
df1_negative_dataset_3 <- df1_negative_dataset[splitSample==3,] 
## forming 3 train datasets.
train_1 = rbind(df1_negative_dataset_1,df1_positive_dataset)
train_2 = rbind(df1_negative_dataset_2,df1_positive_dataset)
train_3 = rbind(df1_negative_dataset_3,df1_positive_dataset)
## saving these datasets.
write.csv(train_1,'train_1.csv')
write.csv(train_2,'train_2.csv')
write.csv(train_3,'train_3.csv')

# summary(train_1)


train_1$batch_enrolled=NULL
test$batch_enrolled=NULL
train_1$sub_grade = as.factor(train_1$sub_grade)
train_1$emp_length = as.factor(train_1$emp_length)
new_factor_levels = c('0','1','10','2','3','4','5','6','7','8','9','n/a')
levels(train_1$emp_length) = new_factor_levels
train_1[,10][train_1[,10]=='n/a'] = NA
train_1$emp_length <- as.character(train_1$emp_length)
train_1$emp_length <- as.numeric(train_1$emp_length)
train_1$home_ownership = as.factor(train_1$home_ownership)
train_1$pymnt_plan = as.factor(train_1$pymnt_plan)
train_1 = subset(train_1,pymnt_plan!="y")
train_1$pymnt_plan=NULL
train_1$purpose = as.factor(train_1$purpose)
ffr = aggregate(train_1[,c(18)],list(Region = train_1$zip_code),max)
train_1 = merge(train_1,ffr,by.x = 'zip_code',by.y='Region',all.x=T)
train_1$firang = train_1$addr_state.x==train_1$addr_state.y
train_1$zip_code=NULL
train_1$addr_state.x = as.factor(train_1$addr_state.x)
train_1$addr_state.y = as.factor(train_1$addr_state.y)
train_1$initial_list_status = as.factor(train_1$initial_list_status)
train_1$application_type = as.factor(train_1$application_type)
aj = strsplit(train_1$last_week_pay,'th ')
ajk = matrix(unlist(aj),nrow(train_1),2,byrow = TRUE)
ajk = as.data.frame(ajk)
train_1$last_week_pay = ajk$V1
train_1$last_week_pay = as.character(train_1$last_week_pay)
train_1$last_week_pay = as.numeric(train_1$last_week_pay)
test$sub_grade = as.factor(test$sub_grade)
test$emp_length = as.factor(test$emp_length)
new_factor_levels = c('0','1','10','2','3','4','5','6','7','8','9','n/a')
levels(test$emp_length) = new_factor_levels
test[,10][test[,10]=='n/a'] = NA
test$emp_length <- as.character(test$emp_length)
test$emp_length <- as.numeric(test$emp_length)
test$home_ownership = as.factor(test$home_ownership)
test$pymnt_plan = NULL
test$purpose = as.factor(test$purpose)
ffr = aggregate(test[,c(18)],list(Region = test$zip_code),max)
test = merge(test,ffr,by.x = 'zip_code',by.y='Region',all.x=T)
test$firang = test$addr_state.x==test$addr_state.y
test$zip_code=NULL
test$addr_state.x = as.factor(test$addr_state.x)
test$addr_state.y = as.factor(test$addr_state.y)
test$initial_list_status = as.factor(test$initial_list_status)
test$application_type = as.factor(test$application_type)
aj = strsplit(test$last_week_pay,'th ')
ajk = matrix(unlist(aj),nrow(test),2,byrow = TRUE)
ajk = as.data.frame(ajk)
test$last_week_pay = ajk$V1
test$last_week_pay = as.character(test$last_week_pay)
test$last_week_pay = as.numeric(test$last_week_pay)

## Missing values....
train_1[,10][is.na(train_1[,10])==TRUE] = 0
train_1$loanfund = train_1$loan_amnt==train_1$funded_amnt
train_1$loanfund = as.numeric(train_1$loanfund)
train_1$loanfund = as.factor(train_1$loanfund)##...can be done by miss forest..##
train_1$loanfundinv = train_1$loan_amnt==train_1$funded_amnt_inv
train_1$loanfundinv = as.numeric(train_1$loanfundinv)
train_1$loanfundinv = as.factor(train_1$loanfundinv)
train_1$diff_inv_bank = train_1$funded_amnt-train_1$funded_amnt_inv
train_1[,12][is.na(train_1$annual_inc)==T] = mean(train_1$annual_inc,na.rm = T)
train_1[,19][is.na(train_1$delinq_2yrs)==T] = 0
train_1[,20][is.na(train_1$inq_last_6mths)==T] = 0
train_1[,26][is.na(train_1$total_acc)==T] = mean(train_1$total_acc,na.rm = T)
train_1[,21][is.na(train_1$mths_since_last_delinq)==T] = mean(train_1$mths_since_last_delinq,na.rm = T)
train_1[,25][is.na(train_1$revol_util)==T] = mean(train_1$revol_util,na.rm = T)
train_1$mths_since_last_record=NULL
train_1[,22][is.na(train_1$open_acc)==T] = mean(train_1$open_acc,na.rm = T)
train_1[,23][is.na(train_1$pub_rec)==T] = 0
train_1[,24][is.na(train_1$revol_bal)==T] = 0
train_1[,32][is.na(train_1$collections_12_mths_ex_med)==T] = 0
train_1$mths_since_last_major_derog=NULL
train_1[,35][is.na(train_1$last_week_pay)==T] = mean(train_1$last_week_pay,na.rm = T)
train_1[,36][is.na(train_1$acc_now_delinq)==T] = 0
train_1[,37][is.na(train_1$tot_coll_amt)==T] = 0
train_1[,38][is.na(train_1$tot_cur_bal)==T] = mean(train_1$tot_cur_bal,na.rm = T)
train_1[,39][is.na(train_1$total_rev_hi_lim)==T] = mean(train_1$total_rev_hi_lim,na.rm = T)
train_1$verification_status_joint=NULL
train_1$verification_status = as.factor(train_1$verification_status)

test[,10][is.na(test[,10])==TRUE] = 0
test$loanfund = test$loan_amnt==test$funded_amnt
test$loanfund = as.numeric(test$loanfund)
test$loanfund = as.factor(test$loanfund)
test$loanfundinv = test$loan_amnt==test$funded_amnt_inv
test$loanfundinv = as.numeric(test$loanfundinv)
test$loanfundinv = as.factor(test$loanfundinv)
test$diff_inv_bank = test$funded_amnt-test$funded_amnt_inv
test[,12][is.na(test$annual_inc)==T] = mean(test$annual_inc,na.rm = T)
test[,19][is.na(test$delinq_2yrs)==T] = 0
test[,20][is.na(test$inq_last_6mths)==T] = 0
test[,21][is.na(test$mths_since_last_delinq)==T] = mean(test$mths_since_last_delinq,na.rm = T)
test$mths_since_last_record=NULL
test[,22][is.na(test$open_acc)==T] = mean(test$open_acc,na.rm = T)
test[,23][is.na(test$pub_rec)==T] = 0
test[,24][is.na(test$revol_bal)==T] = 0
test[,25][is.na(test$revol_util)==T] = mean(test$revol_util,na.rm = T)
test[,26][is.na(test$total_acc)==T] = mean(test$total_acc,na.rm = T)
test[,32][is.na(test$collections_12_mths_ex_med)==T] = 0
test$mths_since_last_major_derog=NULL
test[,35][is.na(test$last_week_pay)==T] = mean(test$last_week_pay,na.rm = T)
test[,36][is.na(test$acc_now_delinq)==T] = 0
test[,37][is.na(test$tot_coll_amt)==T] = 0
test[,38][is.na(test$tot_cur_bal)==T] = mean(test$tot_cur_bal,na.rm = T)
test[,39][is.na(test$total_rev_hi_lim)==T] = mean(test$total_rev_hi_lim,na.rm = T)
test$verification_status_joint=NULL
test$verification_status = as.factor(test$verification_status)

## Wordcloud
loan_descriptions.corpus <- Corpus(DataframeSource(data.frame(head(train_1[,9], n=nrow(train_1)))))
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, removePunctuation)
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, content_transformer(tolower))

wordcloud(loan_descriptions.corpus,
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Paired"))

## adding new features....
train_1$loan_amntbigg = as.numeric(train_1$loan_amnt>34000)
train_1$loan_amntbigg = as.factor(train_1$loan_amntbigg)
train_1$term = as.numeric(train_1$term)
train_1[,5][train_1[,5]=='1'] = '36'
train_1[,5][train_1[,5]=='2'] = '60'
train_1$term = as.character(train_1$term)
train_1$term = as.numeric(train_1$term)
train_1$int_ratebigg = as.numeric(train_1$int_rate>9)
train_1$int_ratebigg = as.factor(train_1$int_ratebigg)
train_1$int_rateterm = train_1$int_rate*train_1$term
train_1$grade_int = as.numeric(train_1$grade)
train_1$int_grade = train_1$int_rate*train_1$grade_int
train_1$annual_inc_high = as.factor(as.numeric(train_1$annual_inc>2000000))
train_1 = train_1[train_1$dti<=100,]
test[,18][test[,18]>140] = 140
train_1$delinq_big = as.factor(as.numeric(train_1$delinq_2yrs>1))
train_1$pub_rec_bigg = as.factor(as.numeric(train_1$pub_rec>1))
train_1 = train_1[train_1$pub_rec<=20,]


test$loan_amntbigg = as.numeric(test$loan_amnt>34000)
test$loan_amntbigg = as.factor(test$loan_amntbigg)
test$term = as.numeric(test$term)
test[,5][test[,5]=='1'] = '36'
test[,5][test[,5]=='2'] = '60'
test$term = as.character(test$term)
test$term = as.numeric(test$term)
test$int_ratebigg = as.numeric(test$int_rate>9)
test$int_ratebigg = as.factor(test$int_ratebigg)
test$int_rateterm = test$int_rate*test$term
test$grade_int = as.numeric(test$grade)
test$int_grade = test$int_rate*test$grade_int
test$annual_inc_high = as.factor(as.numeric(test$annual_inc>2000000))

test[,18][test[,18]>140] = 140
test$delinq_big = as.factor(as.numeric(test$delinq_2yrs>1))
test$pub_rec_bigg = as.factor(as.numeric(test$pub_rec>1))
test = test[test$pub_rec<=20,]
test[,20][test[,20]>20] = 20



df1 = as.h2o(train_1)
df2 = as.h2o(test)

df1$emp_title = NULL
df1$title = NULL
df1$desc = NULL
df2$emp_title = NULL
df2$title = NULL
df2$desc = NULL

#-------------------------------------------------------------------------------------------------------------------------#
# Data Modelling

# Random Forest
rf1 <- h2o.randomForest(         
  training_frame = train,        
  x=c(2:35,37:49),               
  y=36,                          
  model_id = "rf_v1",            
  ntrees = 100,
  mtries = 7,
  nbins = 100,                  
  stopping_rounds = 3,
  balance_classes = TRUE,        
  score_each_iteration = T)

## predictions
finalRf_predictions<-h2o.predict(
  object = m1
  ,newdata = df2)

df2$predict = finalRf_predictions$p1
y = as.data.frame(finalRf_predictions$p1)
test$predict = df2$predict
write.csv(y,"submit_ta.csv")

##glm
x=c(2:35,37:49)
y = 36
m1 = h2o.glm(training_frame = df1,x = x,y = y,family='binomial',solver='L_BFGS')

##neural_network
x = c("loan_status","member_id")
y = "loan_status"  
d =setdiff(colnames(df1),x)

m1 <- h2o.deeplearning(
  model_id="dl_model_first", 
  training_frame=train, 
  #validation_frame=valid,   ## validation dataset: used for scoring and early stopping
  x=d,
  y=y,
  loss = "CrossEntropy",  ## default
  hidden=c(50,50),       ## default: 2 hidden layers with 200 neurons each
  epochs=50,
  variable_importances=T    ## not enabled by default
)

## predictions
finalRf_predictions<-h2o.predict(object = gbm2,newdata = df2)
y = as.data.frame(finalRf_predictions$p1)
test$predict = y$p1
final = test[,c(1,52)]
write.csv(final,"submit_gb.csv")

## GBM
splits <- h2o.splitFrame(df1,c(0.8,0.0),seed=1234)
train <- h2o.assign(splits[[1]], "train.hex")
valid <- h2o.assign(splits[[2]], "valid.hex")
test2 <- h2o.assign(splits[[3]], "test.hex") 
perf = h2o.performance(gbm2,test2)
h2o.auc(perf)


gbm2 <- h2o.gbm(
  training_frame = df1,     ##
  #validation_frame = valid,  ##
  x=c(2:35,37:49),            ##
  y=36,
  #min_rows = 50,
  #nbins = 100,                       ## 
  ntrees = 250,               ## decrease the trees, mostly to allow for run time
  ##  (from 50)
  learn_rate = 0.05,          ## increase the learning rate (from 0.1)
  max_depth = 40,
  balance_classes = TRUE,            
  stopping_rounds = 5,        ## 
  stopping_tolerance = 1e-4,
  stopping_metric = "AUC",##
  score_each_iteration = T,
  score_tree_interval = 10,
  #nfolds = 4,##
  model_id = "gbm_covType2",  ##
  seed = 2000000) 

#-------------------------------------------------------------------------------------------------------------------------#
