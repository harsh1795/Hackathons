#######################
## Author: BalckViper42
##
#######################
setwd("C:/Users/212630200/Desktop/New folder/Hackathon/ZSHack/")
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
library(stringr)
library(reshape2)
library(dplyr)
library(Matrix)
library(data.table)
#######################
# train = fread("train.csv")
# test = fread("test.csv")
# transProbs = fread("incident.csv", header = TRUE)
# sample_train = train[train$PID == "1021443", ]
# View(sample_train)
#sample_train$Event = as.factor(sample_train$Event)
# sample_train$Date = as.character(sample_train$Date)
# sample_train$Date = str_sub(sample_train$Date, -3, -1)
# sample_train$Event = as.numeric(sample_train$Event)
# sample_train = sample_train[is.na(sample_train$Event)==FALSE, ]
# meandate = as.integer(mean(as.numeric(table(sample_train$Date))))
# listmeandate = test_date(meandate)
# sample_train$Date = str_sub(sample_train$Date, -2, -1)
# counts <- ddply(sample_train, .(sample_train$Date, sample_train$Event), nrow)
# names(counts) = c("Date","Event", "Freq")
# counts$Date=as.numeric(counts$Date)
# counts$Event=as.numeric(counts$Event)
# counts = counts[is.na(counts$Event)==FALSE, ]
# DT =  as.data.table(counts)
# m =  dcast(DT,Date~Event,fill=0)[-1]
# mat = as.matrix(m) 
# emmisionProbs  = mat/rowSums(mat) 
# rownames(transProbs) = transProbs$V1
# transProbs$V1=NULL
# transProbs = as.matrix.data.frame(transProbs)
# Symbols = as.vector(sample_train$Event)
# States = c("1","2","3","4","5","6","7","8","9","10","11","12")
# 
# 
# #datnorm<-as.data.frame(matnorm) 
# ##predevent = ...
# predictdata = dummy.data.frame(no = numeric(), event = character())
# 
# 
# 
# 
# ##############################
# test_date = function(x) {
#   if(x==10){return(c(1, 1, 1 , 1, 1, 1, 1, 1 ,1 ,1))}
#   if(x==9){return(c(1, 1, 1 , 1, 1, 1, 1, 1 ,1 ,2))}
#   if(x==8){return(c(1, 1, 1 , 1, 1, 1, 1, 1 ,2 ,2))}
#   if(x==7){return(c(1, 1, 1 , 1, 1, 1, 1, 2 ,2 ,2))}
#   if(x==6){return(c(1, 1, 1 , 1, 1, 1, 2, 2 ,2 ,2))}
#   if(x==5){return(c(1, 1, 1 , 1, 1, 2, 2, 2 ,2 ,2))}
#   if(x==4){return(c(1, 1, 1 , 1, 2, 2, 2, 2 ,3 ,3))}
#   if(x==3){return(c(1, 1, 1 , 2, 2, 2, 3, 3 ,3 ,4))}
#   if(x==2){return(c(1, 1, 2 , 2, 3, 3, 4, 4 ,5 ,5))}
#   if(x==1){return(c(1, 2, 3 , 4, 5, 6, 7, 8 ,9 ,10))}
# }
# 
# 
# 
# 
# ##################################################
# library(data.table)
# library(markovchain)
# 
# train <- fread("train.csv")
# test <- fread("test.csv")
# 
# head(train)
# head(test)
# 
# train <- train[order(PID)]
# test <- test[order(PID)]
# 
# 
# # Create list of events per PID such that event sequence is mainta --------
# 
# list_train <- train[,.(list(Event)),.(PID,Date)]
# list_one <- list_train[,.(list(V1)),.(PID)]
# list_one[,V1 := lapply(V1, unlist, use.names = F)]
# setnames(list_one,"V1","Events")
# 
# 
# # Building Markov Chain Model on PID Level --------------------------------
# 
# prediction <- list()
# 
# for(x in 1:nrow(list_one))
# {
#   PID <- list_one[x,PID]
#   events_x <- as.character(unlist(list_one[x,Events]))
#   
#   mcX <- markovchainFit(events_x, method = "mle")
#   pred <- predict(object = mcX$estimate, newdata = events_x, n.ahead=10) # predict next 10 events
#   
#   prediction[[PID]] <- pred
#   
# }
# 
# # Creating final submission file
# 
# final_prediction <- data.table(PID = names(prediction), Event = prediction)
# 
# for(i in 1:nrow(final_prediction))
# {
#   for(j in 1:10)
#   {
#     final_prediction[[paste0("Event",j)]] <- lapply(final_prediction$Event,'[',j)
#   }
# }
# 
# final_prediction[,Event := NULL]
# fwrite(final_prediction,"markov_preds.csv")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## visualization
# hist(as.numeric(train$Event))
# ggplot(train[train$PID=="1028890",], aes(day, c(Event))) + geom_point()
# 
# 
# iter = 0
# setwd("C:/Users/212630200/Desktop/New folder/Hackathon/ZSHack/imagese/")
# for(i in tra){
#   traine = train[train$Event==i,]
#   myplot = ggplot(traine, aes(day, PID)) + geom_point() + ylab(paste("Event",i,sep = "  ")) +
#     xlab(paste(i,"Month",sep="  "))
#   ggsave(paste("Eventplot_",i,".jpeg",sep=""),myplot)
# 
#   cat("iter :", iter,"done")
#   iter= iter+1
#  }
# 
# 
# 
# ##########################################################
# train = fread("train.csv")
# train$Dateyear = str_sub(train$Date, 1, 4)
# train$Datemonth = str_sub(train$Date, -2, -1)
# train$Day = paste(train$Datemonth,train$Dateyear,sep="-")
# train$Day = paste("01",train$Day,sep="-")
# train$day = as.Date(train$Day, format = "%d-%m-%Y")
# train$Datemonth[train$Dateyear == "2012"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2012"])+12)
# train$Datemonth[train$Dateyear == "2013"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2013"])+24)
# 
# tra = unique(train$PID)
# predicteddf = data.frame(PID = character(), Event1 = character(), Event2 = character()
#                             , Event3 = character(), Event4 = character(), Event5 = character()
#                             , Event6 = character(), Event7 = character(), Event8 = character()
#                             , Event9 = character(), Event10 = character())
# for (i in tra) {
#   subset = train[train$PID == i, ]
#   event_list = unique(subset$Event)
#   nrows = nrow(subset)
#   a = table(subset$Event)
#   a = as.data.frame(sort(a, decreasing = TRUE))
#   if(nrows<30 & length(event_list)>=10) {
#     a = table(subset$Event)
#     a = as.data.frame(sort(a, decreasing = TRUE))
#     b = a[1:10,1]
#     b = as.character(b)
#     predicteddff = data.frame(PID = i, Event1 = b[1], Event2 = b[2]
#                               , Event3 = b[3], Event4 = b[4], Event5 = b[5]
#                               , Event6 = b[6], Event7 = b[7], Event8 = b[8]
#                               , Event9 = b[9], Event10 = b[10])
#     
#     predicteddf = rbind(predicteddf, predicteddff)
#   }
#   if(nrows>30 & length(event_list)>=10) {
#     a = table(subset$Event)
#     a = as.data.frame(sort(a, decreasing = TRUE))
#     b = a[1:10,1]
#     b = as.character(b)
#     predicteddff = data.frame(PID = i, Event1 = b[1], Event2 = b[2]
#                               , Event3 = b[3], Event4 = b[4], Event5 = b[5]
#                               , Event6 = b[6], Event7 = b[7], Event8 = b[8]
#                               , Event9 = b[9], Event10 = b[10])
#     
#     predicteddf = rbind(predicteddf, predicteddff)
#   }
# }
#   
##################################################################
train = fread("train.csv")
test  = fread("test.csv")
sample_sub <- fread("sample_submission.csv")
# order data
train <- train[order(PID)]
test <- test[order(PID)]
train$Dateyear = str_sub(train$Date, 1, 4)
train$Datemonth = str_sub(train$Date, -2, -1)
train$Datemonth[train$Dateyear == "2012"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2012"])+12)
train$Datemonth[train$Dateyear == "2013"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2013"])+24)
trainb = train[train$Datemonth>=30, ]
c = unique(train$PID)
d = unique(trainb$PID)
e = setdiff(c,d)
trainb = train[!train$PID %in% e, ]
trains = train[train$PID %in% e, ]
trainb = trainb[trainb$Datemonth>=30, ]
# threshold = median(table(trainb$PID))
# 
# PID_count = as.data.frame(table(trainb$PID))
# PID_count_less_list = PID_count$Var1[PID_count$Freq<10]
# PID_count_less_list = as.character(PID_count_less_list)
# trainb_add = train[train$PID %in% PID_count_less_list,]
# trainb_sub = trainb[!trainb$PID %in% PID_count_less_list,]
# trains = rbind(trains, trainb_add)
# trainb = trainb_sub
trainb = trainb[order(trainb$PID), ]
trains = trains[order(trains$PID), ]
trainss = unique(trains$PID)
trainbb = unique(trainb$PID)
# trainsss = trains[trains$Datemonth>=12 & trains$Datemonth<28, ]
# f = unique(trainsss$PID)
# g = setdiff(e,f)
# trainsssb = trains[!trains$PID %in% g, ]
# trainssss = trains[trains$PID %in% g, ]
# trainsssb = trainsssb[trainsssb$Datemonth>=12 & trainsssb$Datemonth<28, ]
# trainsssb = trainsssb[order(trainsssb$PID), ]
# trainssss = trainssss[order(trainssss$PID), ]
# trainsssss = unique(trainssss$PID)
# trainsssbb = unique(trainsssb$PID)
# trainsssb_uniq_event = c()
# trainssss_uniq_event = c()
# for(i in trainsssbb){
#   Subset = trainsssb[trainsssb$PID==i,]
#   a = unique(Subset$Event)
#   #cat("Number is - ", i, " ", length(a), "\n")
#   if(length(a)<10){
#     trainsssb_uniq_event = c(trainsssb_uniq_event,i)
#   }
# }
# for(i in trainsssss){
#   Subset = trainssss[trainssss$PID==i,]
#   a = unique(Subset$Event)
#   #cat("Number is - ", i, " ", length(a), "\n")
#   if(length(a)<10){
#     trainssss_uniq_event = c(trainssss_uniq_event,i)
#   }
# }
# trainsssb_less = trainsssb[trainsssb$PID %in% trainsssb_uniq_event, ] 
# trainssss_less = trainssss[trainssss$PID %in% trainssss_uniq_event, ]
# trainsssb = trainsssb[!trainsssb$PID %in% trainsssb_uniq_event, ]
# trainssss = trainssss[!trainssss$PID %in% trainssss_uniq_event, ]
# trainsssss = unique(trainssss$PID)
# trainsssbb = unique(trainsssb$PID)
# random_mat_lessb = data.frame(Event1 = character(), Event2 = character(), 
#                               Event3 = character(), Event4 = character(), 
#                               Event5 = character(), Event6 = character(), 
#                               Event7 = character(), Event8 = character(), 
#                               Event9 = character(), Event10 = character())
# for(i in trainsssb_uniq_event){
#   subsets = trainsssb_less[trainsssb_less$PID == i, ]
#   v = subsets$Event
#   v = rep(v, times=10)
#   v= v[1:10]
#   v = data.frame(Event1 = v[1], Event2 = v[2], 
#              Event3 = v[3], Event4 = v[4], 
#              Event5 = v[5], Event6 = v[6], 
#              Event7 = v[7], Event8 = v[8], 
#              Event9 = v[9], Event10 = v[10])
#   random_mat_lessb = rbind(random_mat_lessb, v)
# }
# random_mat_lesss = data.frame(Event1 = character(), Event2 = character(), 
#                               Event3 = character(), Event4 = character(), 
#                               Event5 = character(), Event6 = character(), 
#                               Event7 = character(), Event8 = character(), 
#                               Event9 = character(), Event10 = character())
# for(i in trainssss_uniq_event){
#   subsets = trainssss_less[trainssss_less$PID == i, ]
#   v = subsets$Event
#   v = rep(v, times=10)
#   v= v[1:10]
#   v = data.frame(Event1 = v[1], Event2 = v[2], 
#                  Event3 = v[3], Event4 = v[4], 
#                  Event5 = v[5], Event6 = v[6], 
#                  Event7 = v[7], Event8 = v[8], 
#                  Event9 = v[9], Event10 = v[10])
#   random_mat_lesss = rbind(random_mat_lesss, v)
# }



#train$Datemonth=NULL
#train$Dateyear=NULL
# Predicting future events based on popular past events per patient -------
train_dcastb <- dcast(data = trainb, PID ~ Event, length, value.var = "Event")
train_dcasts <- dcast(data = trains, PID ~ Event, length, value.var = "Event")

# train_dcastssss <- dcast(data = trainssss, PID ~ Event, length, value.var = "Event")
# train_dcastsssb <- dcast(data = trainsssb, PID ~ Event, length, value.var = "Event")

# get top 10 events per row
random_submitb <- colnames(train_dcastb)[-1][apply(train_dcastb[,2:ncol(train_dcastb)],1, function(x){order(-x)[1:10]})]
random_submits <- colnames(train_dcasts)[-1][apply(train_dcasts[,2:ncol(train_dcasts)],1, function(x){order(-x)[1:10]})]

# random_submitsssb <- colnames(train_dcastsssb)[-1][apply(train_dcastsssb[,2:ncol(train_dcastsssb)],1, function(x)order(-x)[1:10])]
# random_submitssss <- colnames(train_dcastssss)[-1][apply(train_dcastssss[,2:ncol(train_dcastssss)],1, function(x)order(-x)[1:10])]

# create the submission file
random_matb <- as.data.table((matrix(random_submitb,ncol = 10, byrow = T)))
colnames(random_matb) <- colnames(sample_sub)[-1]
random_mats <- as.data.table((matrix(random_submits,ncol = 10, byrow = T)))
colnames(random_mats) <- colnames(sample_sub)[-1]

# random_matssss <- as.data.table((matrix(random_submitssss,ncol = 10, byrow = T)))
# colnames(random_matssss) <- colnames(sample_sub)[-1]
# random_matsssb <- as.data.table((matrix(random_submitsssb,ncol = 10, byrow = T)))
# colnames(random_matsssb) <- colnames(sample_sub)[-1]
# colnames(random_mat_lessb) <- colnames(sample_sub)[-1]
# colnames(random_mat_lesss) <- colnames(sample_sub)[-1]

random_matb <- cbind(PID = trainbb, random_matb)
random_mats <- cbind(PID = trainss, random_mats)
#random_mats$Event11 = random_mats$Event1
#random_mats$Event10 = random_mats$Event1
#random_mats$Event9 = random_mats$Event3
#random_mats$Event8 = random_mats$Event2
#random_mats$Event7 = random_mats$Event1
# random_matssss <- cbind(PID = trainsssss, random_matssss)
# random_matsssb <- cbind(PID = trainsssbb, random_matsssb)
# random_mat_lessb <-cbind(PID = trainsssb_uniq_event, random_mat_lessb)
# random_mat_lesss <-cbind(PID = trainssss_uniq_event, random_mat_lesss)
random_mat = rbind(random_matb, random_mats)
# random_mat = rbind(random_matb, random_matssss)
# random_mat = rbind(random_mat, random_matsssb)
# random_mat = rbind(random_mat, random_mat_lessb)
# random_mat = rbind(random_mat, random_mat_lesss)
#id = random_mat$PID[1]
# tra = train[train$PID=="1000001", ]
# tra$month = str_sub(tra$Date,-2,-1)
# bb = random_mat[1,2:11]
# bb = unlist(bb)
# bb = as.character(bb)
# tra = tra[tra$Event %in% bb, ]
# freq_tab_1 = as.data.frame(table(tra$Event))
# freq_tab_1 = freq_tab_1[order(-freq_tab_1$Freq), ]
# if(mean(as.numeric(tra2$month)))
# 
# tra2 = tra[tra$Datemonth>28, ]
# freq_tab_2 = as.data.frame(table(tra2$Event))
# freq_tab_2 = freq_tab_2[order(-freq_tab_2$Freq), ]
# 
# train_last_6 = train[train$Datemonth>=28,]
# train_last_6_unique_patient = unique(train_last_6$PID)
# frequenc = c()
# for(i in train_last_6_unique_patient) {
#   subset = train_last_6[train_last_6$PID == i, ]
#   freq = mean(table(subset$Datemonth))
#   frequenc = c(frequenc, freq)
# }
# train_rest_unique = setdiff(unique(train$PID), train_last_6_unique_patient)
# for(i in train_rest_unique) {
#   subset = train[train$PID == i, ]
#   freq = mean(table(subset$Datemonth))
#   frequenc = c(frequenc, freq)
# }
# 
# 
# random_mat$frequency = frequenc
# 
# uniq_ids = unique(train$PID)
# random_mat$repeatseq = 0
# iter = 0
# for(i in c(1:3000)){
#   fre = random_mat$frequency[i]
#   cat(fre,"\n")
#   cat(iter, "----")
#   if(fre<10) {
#     cat("entering", "\n")
#     random_mat$repeatseq[i] = 1
#   }
#   cat("\n")
#   iter=iter+1
# }
# 
# #random_mat_lessb = random_mat[random_mat$repeatseq==1]
# #random_mat_lesss = random_mat[random_mat$repeatseq==0]
# 
# 
# ##
# random_mat$Event1_freq = 0
# random_mat$Event2_freq = 0
# random_mat$Event3_freq = 0
# random_mat$Event4_freq = 0
# random_mat$Event5_freq = 0
# random_mat$Event6_freq = 0
# random_mat$Event7_freq = 0
# random_mat$Event8_freq = 0
# random_mat$Event9_freq = 0
# random_mat$Event10_freq = 0
# 
for(i in c(1:3000)) {
  id = as.character(random_mat[i,1])
  subset= train[train$PID==id, ]
  subset = subset[subset$Datemonth>=30,]
  eventseq = as.character(as.vector(random_mat[i,2:11]))
  for(j in c(1:10)) {
    colnam = paste("Event",j,"_freq",sep = "")
    eventseq_val = eventseq[j]
    frequ = nrow(subset[subset$Event==eventseq_val,])
    cat(frequ, " __ ", j, "  __  ", i, "\n")
    cat(colnam, "\n", "\n")
    if(colnam=="Event1_freq") {
      random_mat$Event1_freq[i] = frequ
    }
    if(colnam=="Event2_freq") {
      random_mat$Event2_freq[i] = frequ
    }
    if(colnam=="Event3_freq") {
      random_mat$Event3_freq[i] = frequ
    }
    if(colnam=="Event4_freq") {
      random_mat$Event4_freq[i] = frequ
    }
    if(colnam=="Event5_freq") {
      random_mat$Event5_freq[i] = frequ
    }
    if(colnam=="Event6_freq") {
      random_mat$Event6_freq[i] = frequ
    }
    if(colnam=="Event7_freq") {
      random_mat$Event7_freq[i] = frequ
    }
    if(colnam=="Event8_freq") {
      random_mat$Event8_freq[i] = frequ
    }
    if(colnam=="Event9_freq") {
      random_mat$Event9_freq[i] = frequ
    }
    if(colnam=="Event10_freq") {
      random_mat$Event10_freq[i] = frequ
    }
  }
}
# random_mat$Event1_freq = as.numeric(as.logical(random_mat$Event1_freq))
# random_mat$Event2_freq = as.numeric(as.logical(random_mat$Event2_freq))
# random_mat$Event3_freq = as.numeric(as.logical(random_mat$Event3_freq))
# random_mat$Event4_freq = as.numeric(as.logical(random_mat$Event4_freq))
# random_mat$Event5_freq = as.numeric(as.logical(random_mat$Event5_freq))
# random_mat$Event6_freq = as.numeric(as.logical(random_mat$Event6_freq))
# random_mat$Event7_freq = as.numeric(as.logical(random_mat$Event7_freq))
# random_mat$Event8_freq = as.numeric(as.logical(random_mat$Event8_freq))
# random_mat$Event9_freq = as.numeric(as.logical(random_mat$Event9_freq))
# random_mat$Event10_freq = as.numeric(as.logical(random_mat$Event10_freq))

random_mat_final = data.frame(Event1 = character(), Event2 = character(),
                              Event3 = character(), Event4 = character(),
                              Event5 = character(), Event6 = character(),                                Event7 = character(), Event8 = character(),
                              Event9 = character(), Event10 = character())

for(i in c(1:3000)) {
  event_seq = as.character(as.vector(random_mat[i,2:11, with=F]))
  # for(j in c(1:10)) {
  #   event_seq_val = event_seq[j]
  #   event_seq_fre = as.numeric(random_mat[i,(j+13), with=F])
  #   if(event_seq_fre == 0) {
  #     random_mat[i,(j+1)] = random_mat[i,2, with = F]
  #   }
  #   cat("changed ", i ," , ",(j+1), "  ", "\n")
  #   #new_seq = c(new_seq, event_seq_val)
  #   #cat("\n",new_seq,"\n")
  # }
  for(j in c(6:10)) {
    event_seq_val = event_seq[j]
    event_seq_fre = as.numeric(random_mat[i,(j+11), with=F])
    if(event_seq_fre < 2) {
      random_mat[i,(j+1)] = random_mat[i,3, with = F]
    }
    cat("changed ", i ," , ",(j+1), "  ", "\n")
    #new_seq = c(new_seq, event_seq_val)
    #cat("\n",new_seq,"\n")
  }
  #v = data.frame(Event1 = new_seq[1], Event2 = new_seq[2],
  #          Event3 = new_seq[3], Event4 = new_seq[4],
  #          Event5 = new_seq[5], Event6 = new_seq[6],
  #          Event7 = new_seq[7], Event8 = new_seq[8],
  #          Event9 = new_seq[9], Event10 = new_seq[10])
  #random_mat_final = rbind(random_mat_final, v)
  cat(i,"___ done ...","\n")
}
#pid = random_mat$PID
#random_mat_final <- cbind(PID = pid, random_mat_final)
#random_mat_final$PID=NULL










random_mat_copy = random_mat[, 1:11]
# random_mat_copy$Event11 = random_mat_copy$Event7
random_mat_copy$Event10 = random_mat_copy$Event4
random_mat_copy$Event9 = random_mat_copy$Event3
random_mat_copy$Event8 = random_mat_copy$Event2
random_mat_copy$Event7 = random_mat_copy$Event1
# random_mat_copy = rbind(random_mat_copy, random_mat_lesss)
# random_mat_copy$repeatseq=NULL
# random_mat_copy$frequency=NULL
#colnames(random_mat_copy) = c("PID", "Event1", "Event2", "Event3", "Event4", "Event5", "Event6", "Event7", "Event8", "Event9", "Event10")
fwrite(random_mat_copy,"final_submission.csv")


##########################################################################
# ##creating training, testing,validation
# new_train = data.frame(PID = character(), Date = numeric(), Event = character(), Dateyear = character(), Datemonth = character())
# new_valid = data.frame(PID = character(), Date = numeric(), Event = character(), Dateyear = character(), Datemonth = character())
# new_test = data.frame(PID = character(), Date = numeric(), Event = character(), Dateyear = character(), Datemonth = character())
# uniq_pids = unique(train$PID)
# iter=0
# for(i in c(1:3000)) {
#   id = uniq_pids[i]
#   train_pid = subset(train[train$PID==id, ])
#   if(nrow(train_pid)>50) {
#     train_pid = train_pid[order(train_pid$Date), ]
#     test_pid = train_pid[(nrow(train_pid)-9):(nrow(train_pid)),]
#     valid_pid = train_pid[(nrow(train_pid)-19):(nrow(train_pid)-10),]
#     train_pid = train_pid[1:(nrow(train_pid)-20),]
#     new_train = rbind(new_train, train_pid)
#     new_valid = rbind(new_valid, valid_pid)
#     new_test = rbind(new_test, test_pid)
#     cat("Total IDs added : ", iter,"\n")
#     iter=iter+1
#   }
#   
# }
###############################################################
# #accuracy calculater
# #train = fread("train.csv")
# #test  = fread("test.csv")
# #sample_sub <- fread("sample_submission.csv")
# train = new_train
# test = new_valid
# n = length(unique(train$PID))
# uniq_idss = unique(train$PID)
# # order data
# train <- train[order(PID)]
# test <- test[order(PID)]
# train$Dateyear = str_sub(train$Date, 1, 4)
# train$Datemonth = str_sub(train$Date, -2, -1)
# train$Datemonth[train$Dateyear == "2012"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2012"])+12)
# train$Datemonth[train$Dateyear == "2013"] = as.character(as.numeric(train$Datemonth[train$Dateyear == "2013"])+24)
# trainb = train[train$Datemonth>=29, ]
# c = unique(train$PID)
# d = unique(trainb$PID)
# e = setdiff(c,d)
# trainb = train[!train$PID %in% e, ]
# trains = train[train$PID %in% e, ]
# trainb = trainb[trainb$Datemonth>=29, ]
# trainb = trainb[order(trainb$PID), ]
# trains = trains[order(trains$PID), ]
# trainss = unique(trains$PID)
# trainbb = unique(trainb$PID)
# 
# # Predicting future events based on popular past events per patient -------
# train_dcastb <- dcast(data = trainb, PID ~ Event, length, value.var = "Event")
# train_dcasts <- dcast(data = trains, PID ~ Event, length, value.var = "Event")
# # get top 10 events per row
# random_submitb <- colnames(train_dcastb)[-1][apply(train_dcastb[,2:ncol(train_dcastb)],1, function(x){order(-x)[1:10]})]
# random_submits <- colnames(train_dcasts)[-1][apply(train_dcasts[,2:ncol(train_dcasts)],1, function(x){order(-x)[1:10]})]
# 
# # create the submission file
# random_matb <- as.data.table((matrix(random_submitb,ncol = 10, byrow = T)))
# colnames(random_matb) <- colnames(sample_sub)[-1]
# random_mats <- as.data.table((matrix(random_submits,ncol = 10, byrow = T)))
# colnames(random_mats) <- colnames(sample_sub)[-1]
# 
# random_matb <- cbind(PID = trainbb, random_matb)
# random_mats <- cbind(PID = trainss, random_mats)
# random_mat = rbind(random_matb, random_mats)
# #train_last_6 = train[train$Datemonth>=30,]
# #train_last_6_unique_patient = unique(train_last_6$PID)
# #frequenc = c()
# #for(i in train_last_6_unique_patient) {
# #  subset = train_last_6[train_last_6$PID == i, ]
# #  freq = mean(table(subset$Datemonth))
# #  frequenc = c(frequenc, freq)
# #}
# #train_rest_unique = setdiff(unique(train$PID), train_last_6_unique_patient)
# #for(i in train_rest_unique) {
# #  subset = train[train$PID == i, ]
# #  freq = mean(table(subset$Datemonth))
# #  frequenc = c(frequenc, freq)
# #}
# 
# 
# #random_mat$frequency = frequenc
# 
# #uniq_ids = unique(train$PID)
# #random_mat$repeatseq = 0
# #iter = 0
# #for(i in c(1:n)){
# #  fre = random_mat$frequency[i]
# #  cat(fre,"\n")
# #  cat(iter, "----")
# #  if(fre<10) {
# #    cat("entering", "\n")
# #    random_mat$repeatseq[i] = 1
# #  }
# #  cat("\n")
# #  iter=iter+1
# #}
# ##
# # random_mat$Event1_freq = 0
# # random_mat$Event2_freq = 0
# # random_mat$Event3_freq = 0
# # random_mat$Event4_freq = 0
# # random_mat$Event5_freq = 0
# # random_mat$Event6_freq = 0
# # random_mat$Event7_freq = 0
# # random_mat$Event8_freq = 0
# # random_mat$Event9_freq = 0
# # random_mat$Event10_freq = 0
# # 
# # for(i in c(1:n)) {
# #   id = as.character(random_mat[i,1])
# #   subset= train[train$PID==id, ]
# #   subset = subset[subset$Datemonth>=28,]
# #   eventseq = as.character(as.vector(random_mat[i,2:11]))
# #   for(j in c(1:10)) {
# #     colnam = paste("Event",j,"_freq",sep = "")
# #     eventseq_val = eventseq[j]
# #     frequ = nrow(subset[subset$Event==eventseq_val,])
# #     cat(frequ, " __ ", j, "  __  ", i, "\n")
# #     cat(colnam, "\n", "\n")
# #     if(colnam=="Event1_freq") {
# #       random_mat$Event1_freq[i] = frequ
# #     }
# #     if(colnam=="Event2_freq") {
# #       random_mat$Event2_freq[i] = frequ
# #     }
# #     if(colnam=="Event3_freq") {
# #       random_mat$Event3_freq[i] = frequ
# #     }
# #     if(colnam=="Event4_freq") {
# #       random_mat$Event4_freq[i] = frequ
# #     }
# #     if(colnam=="Event5_freq") {
# #       random_mat$Event5_freq[i] = frequ
# #     }
# #     if(colnam=="Event6_freq") {
# #       random_mat$Event6_freq[i] = frequ
# #     }
# #     if(colnam=="Event7_freq") {
# #       random_mat$Event7_freq[i] = frequ
# #     }
# #     if(colnam=="Event8_freq") {
# #       random_mat$Event8_freq[i] = frequ
# #     }
# #     if(colnam=="Event9_freq") {
# #       random_mat$Event9_freq[i] = frequ
# #     }
# #     if(colnam=="Event10_freq") {
# #       random_mat$Event10_freq[i] = frequ
# #     }
# #   }
# # }
# # random_mat_final = data.frame(Event1 = character(), Event2 = character(), 
# #                               Event3 = character(), Event4 = character(), 
# #                               Event5 = character(), Event6 = character(),                                Event7 = character(), Event8 = character(), 
# #                               Event9 = character(), Event10 = character())
# # 
# # for(i in c(1:n)) {
# #   event_seq = as.character(as.vector(random_mat[i,2:11, with=F]))
# #   # for(j in c(1:10)) {
# #   #   event_seq_val = event_seq[j]
# #   #   event_seq_fre = as.numeric(random_mat[i,(j+13), with=F])
# #   #   if(event_seq_fre == 0) {
# #   #     random_mat[i,(j+1)] = random_mat[i,2, with = F]
# #   #   }
# #   #   cat("changed ", i ," , ",(j+1), "  ", "\n")
# #   #   #new_seq = c(new_seq, event_seq_val)
# #   #   #cat("\n",new_seq,"\n")
# #   # }
# #   for(j in c(6:10)) {
# #     event_seq_val = event_seq[j]
# #     event_seq_fre = as.numeric(random_mat[i,(j+13), with=F])
# #     if(event_seq_fre < 2) {
# #       random_mat[i,(j+1)] = random_mat[i,2, with = F]
# #     }
# #     cat("changed ", i ," , ",(j+1), "  ", "\n")
# #     #new_seq = c(new_seq, event_seq_val)
# #     #cat("\n",new_seq,"\n")
# #   }
# #   #v = data.frame(Event1 = new_seq[1], Event2 = new_seq[2], 
# #   #          Event3 = new_seq[3], Event4 = new_seq[4], 
# #   #          Event5 = new_seq[5], Event6 = new_seq[6], 
# #   #          Event7 = new_seq[7], Event8 = new_seq[8], 
# #   #          Event9 = new_seq[9], Event10 = new_seq[10])
# #   #random_mat_final = rbind(random_mat_final, v)
# #   cat(i,"___ done ...","\n")
# # }
# #pid = random_mat$PID
# #random_mat_final <- cbind(PID = pid, random_mat_final)
# #random_mat_final$PID=NULL
# 
# uniq_idss = unique(new_valid$PID)
# new_valid_copy = matrix(unlist(new_valid$Event),nrow = length(uniq_idss), ncol = 10,byrow = TRUE)
# new_valid_copy = as.data.frame(new_valid_copy)
# for(i in c(1:10)){
#   new_valid_copy[,i] = as.character(new_valid_copy[,i])
# }
# colnames(new_valid_copy) <- colnames(sample_sub)[-1]
# new_valid_copy <- cbind(PID = uniq_idss, new_valid_copy)
# 
# 
# 
# 
# 
# random_mat_copy = random_mat[, 1:11]
# # random_mat_copy$Event11 = random_mat_copy$Event7
# random_mat_copy$Event10 = random_mat_copy$Event2
# random_mat_copy$Event9 = random_mat_copy$Event3
# random_mat_copy$Event8 = random_mat_copy$Event2
# random_mat_copy$Event7 = random_mat_copy$Event1
# # random_mat_copy = rbind(random_mat_copy, random_mat_lesss)
# # random_mat_copy$repeatseq=NULL
# # random_mat_copy$frequency=NULL
# #colnames(random_mat_copy) = c("PID", "Event1", "Event2", "Event3", "Event4", "Event5", "Event6", "Event7", "Event8", "Event9", "Event10")
# fwrite(random_mat_copy,"subset_final_submission_29__2321.csv")
# fwrite(new_valid_copy, "validation.csv")
# 
# 
# 
# 
