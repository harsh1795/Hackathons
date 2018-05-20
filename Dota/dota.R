#
# Author: Harshit Saxena
#


setwd("/home/harshit/Desktop/Analytics/Dota/")

# Loading Libraries
library(data.table)
library(xgboost)
library(ggplot2)
library(caret)
library(DescTools)
library(lubridate)
library(dummies)
library(cluster)

# Loading data
train9 = fread("train9.csv")
train1 = fread("train1.csv")
test9 = fread("test9.csv")
test1 = fread("test1.csv")
herodata = fread("hero_data.csv")
submit = fread("sample_submission_CKEH6IJ.csv")

# Data Merging
train9$seqno = rep(1:9, times = nrow(train9)/9)
train1$seqno = rep(10, times = nrow(train1))
train = rbind(train9, train1)
train = train[order(user_id, seqno),]

test9$seqno = rep(1:9, times = nrow(test9)/9)
test1$num_wins = NA
test1$kda_ratio = NA
test1$seqno = rep(10, times = nrow(test1))
test = rbind(test9, test1)
test = test[order(user_id, seqno),]

trainf = merge(train, herodata,by = "hero_id", sort = FALSE, all.x = TRUE)
testf = merge(test, herodata, by = "hero_id", sort = FALSE, all.x = TRUE)
trainf = trainf[!trainf$id %in% c("880_41", "304_22", "2611_22", "1734_18", "178_74", "1484_76", "1028_92"), ]
## roles preprocessing
dataf = rbind(trainf, testf)    ## Trainf has 20940 rows
values = unique(unlist(strsplit(as.character(dataf$roles), split = ":")))
final_list = lapply(values, function(x) as.integer(grepl(x, as.character(dataf$roles))))  ## Important
finaldata = as.data.frame(final_list)
colnames(finaldata) = paste0("role_", values)
dataf = cbind(dataf, finaldata)
dataf$roles = NULL

## creating features
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
modeseq = aggregate(seqno~hero_id, data = dataf, Mode)
colnames(modeseq) = c("hero_id", "modeseq")
dataf = merge(x = dataf, y = modeseq, by = "hero_id", all.x = T, sort = FALSE)
meanseq = aggregate(seqno~hero_id, data = dataf, mean)
colnames(meanseq) = c("hero_id", "meanseq")
dataf = merge(x = dataf, y = meanseq, by = "hero_id", all.x = T, sort = FALSE)

#meankda9_hero = aggregate(kda_ratio~hero_id, data = dataf[dataf$seqno==9,], mean)
# add hero_ids = 45,37,112 as 9th because no data available for 10th position
meankda10_hero = aggregate(kda_ratio~hero_id, data = dataf[dataf$seqno==10,], mean)
colnames(meankda10_hero) = c("hero_id", "meankda10_hero")
meankda10_hero[nrow(meankda10_hero)+1,] = c(45, 3729.415)
meankda10_hero[nrow(meankda10_hero)+1,] = c(37, 3634.907)
meankda10_hero[nrow(meankda10_hero)+1,] = c(112, 2588.625)
meankda10_hero[nrow(meankda10_hero)+1,] = c(49, 4024.112)
dataf = merge(dataf, meankda10_hero, by = "hero_id", all.x = T, sort = F)

meankda_hero = aggregate(kda_ratio~hero_id, data = dataf, mean)
colnames(meankda_hero) = c("hero_id", "meankda_hero")
winavg_hero = aggregate(num_wins~hero_id, data = dataf, mean)
colnames(winavg_hero) = c("hero_id", "winavg_hero")
meankda_usr = aggregate(kda_ratio~user_id, data = dataf, mean)
colnames(meankda_usr) = c("user_id", "meankda_usr")
winavg_usr = aggregate(num_wins~user_id, data = dataf, mean)
colnames(winavg_usr) = c("user_id", "winavg_usr")
dataf = merge(x = dataf, y = meankda_hero, by = "hero_id", all.x = T, sort = FALSE)
dataf = merge(x = dataf, y = winavg_hero, by = "hero_id", all.x = T, sort = FALSE)
dataf = merge(x = dataf, y = meankda_usr, by = "user_id", all.x = T, sort = FALSE)
dataf = merge(x = dataf, y = winavg_usr, by = "user_id", all.x = T, sort = FALSE)

## one-hot encoding
ohe_features = c("primary_attr", "attack_type")
dataf <- dummy.data.frame(dataf,names = ohe_features, sep = "_" )   ## Important

## clustering
clusdata = dataf[,c(2,8:40)]
clusdata = clusdata[!duplicated(clusdata[,c("hero_id")]),]
#clus = clusGap(clusdata[,c(2:34)], FUN = kmeans, nstart = 35, K.max = 15, B = 60)
#plot(clus, main = "clusGap(., FUN = kmeans, n.start=20, B= 60)")
hero_cluster = kmeans(x = clusdata[,c(2:34)],centers = 4,nstart = 20 )
clusdata$cluster_hero = hero_cluster$cluster
clusdata = clusdata[,c(1,35)]
dataf = merge(x = dataf, y = clusdata, by = "hero_id", all.x = T, sort = FALSE)
dataf$base_health = NULL
dataf$base_mana = NULL
dataf$base_mana_regen = NULL
datafsmall = dataf[dataf$user_id %in% lessuserid, ]
dataf = rbind(dataf, datafsmall)
dataf = rbind(dataf, datafsmall)

testf = dataf[is.na(dataf$kda_ratio) == TRUE, ]
trainf = dataf[is.na(dataf$kda_ratio) == FALSE, ]

write.csv(trainf, "preprocessedtrain5.csv", row.names = FALSE )
write.csv(testf, "preprocessedtest5.csv", row.names = FALSE )
write.csv(lessuserid, "overfitting.csv", row.names = FALSE)

## Visualizations
library(ggplot2)  
library(ggcorrplot)

ggplot(datauser, aes(user_id, diff)) + geom_point() +                                 # change num_wins, num_games, kda_ratio
  scale_x_continuous("user_id", breaks = seq(1,3000,100))+ 
  scale_y_continuous("diff", breaks = seq(-5000,5000,by = 500))+ theme_bw() 

ggplot(trainf, aes(hero_id, cluster_hero)) + geom_point() + 
  scale_x_continuous("hero_id", breaks = seq(0,125,5))+ 
  scale_y_continuous("cluster_hero", breaks = seq(0,10,by = 1))+ theme_bw() 


# Correlation plot
corr <- round(cor(trainf[,c(4:45)]),1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)





## Part 2 " predict num_wins
library(reshape2)
datauser = dataf[, c("user_id", "kda_ratio", "seqno")]
datauser = dcast(datauser, user_id ~ seqno, value.var = "kda_ratio")
datauser = datauser[is.na(datauser$`10`) == F, ]
colnames(datauser) = c("user_id", "f1", "f2", "f3", "f4", "f5", "f6"
                       , "f7", "f8", "f9", "f10")
datauser$diff = datauser$f10 - datauser$f9
datauserw = datauser[datauser$diff>800, ]
datausere = datauser[datauser$diff< (-300), ]
datauserf = rbind(datauserw, datausere)
lessuserid = datauserf$user_id
#user10= dataf[dataf$seqno=="10", ]
user10 = dataf[, c("user_id", "num_games", "seqno")]
datauser10 = dcast(user10, user_id ~ seqno, value.var = "num_games")
colnames(datauser10) = c("user_id", "g1", "g2", "g3", "g4", "g5", "g6"
                       , "g7", "g8", "g9", "g10")

datauser = merge(datauser, datauser10, by = "user_id", all = T, sort = FALSE)
trainpast = datauser[is.na(datauser$f10) == F,]
testpast = datauser[is.na(datauser$f10) == T,]

bound <- floor((nrow(trainpast)*0.6))
set.seed(123)
trainpast <- trainpast[sample(nrow(trainpast)), ]           #sample rows 
trainpast.train <- trainpast[1:bound, ]              #get training set
trainpast.test <- trainpast[(bound+1):nrow(trainpast), ] 

library(xgboost)
library(caret)
setDT(trainpast.train)
setDT(trainpast.test)
setDT(testpast)
y.train = trainpast.train$f10
y.test = trainpast.test$f10

new_tr = model.matrix(~.+0, data = trainpast.train[,-c("user_id", "f10"), with=F])
new_ts = model.matrix(~.+0, data = trainpast.test[,-c("user_id", "f10"), with=F])
new_ts2 = model.matrix(~.+0, data = testpast[,-c("user_id", "f10"), with=F])

dtrain <- xgb.DMatrix(data = new_tr,label = y.train)
dtest <- xgb.DMatrix(data = new_ts,label=y.test)
dactualtest = xgb.DMatrix(data = new_ts2)
## Xgboost grid search
cv.ctrl <- trainControl(method = "repeatedcv",verboseIter = T,  repeats = 1,number = 2)

xgb.grid <- expand.grid(nrounds = 400,
                        max_depth = seq(1,3),
                        eta = c(0.01,0.1, 0.3),
                        gamma = c(0.0, 0.2, 1),
                        colsample_bytree = c(0.5,0.8,1),
                        min_child_weight=seq(1,10),
                        subsample = 0.8
)

xgb_tune <-train(f10 ~. - user_id,
                 data=trainpast.train,
                 method="xgbTree",
                 metric = "RMSE",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid
)
xgb_tune$bestTune

prediction <- predict(xgb_tune, trainpast.train)
RMSE(trainpast.train$f10,prediction)


params <- list(booster = "gbtree", objective = "reg:linear", 
               eta=0.1, gamma=1, max_depth=2, min_child_weight=5, colsample_bytree=1, subsample = 0.8)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T,
                 stratified = T, print.every.n = 10, maximize = F)
##best iteration = 79

xgb1 <- xgb.train (params = params, data = dtrain, nrounds = 200, watchlist = list(val=dtest,train=dtrain), 
                   print.every.n = 10, maximize = F , eval_metric = "rmse")
xgbpred <- predict (xgb1,dactualtest)
testpast$f10 = xgbpred

datauser = rbind(trainpast, testpast)
test1 = merge(test1, datauser[,c("user_id", "f10")], by = "user_id", sort = FALSE)
test1$num_wins = test1$f10
test1$f10 = NULL
