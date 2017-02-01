##
## set working directory
setwd("C:/Users/Harshit/Desktop/Analytics Competitions/AMEX")
## loading dataset
df = read.csv("trainh.csv")
View(df)
nn = c(1,34,35,43:62,71:75)
df[,nn] = NULL
val = read.csv("valid8.csv")
vv = c(1,2,35,36,43:62,71:75)
val[,nn] = NULL
write.csv(val,"validrefresh.csv")
write.csv(df,"trainrefresh.csv")



############################################################################################
df = read.csv("T.csv")
val = read.csv("V.csv")
df$X = NULL
val$X = NULL
df1 = subset(df,df$Cit_pref1==df$party_voted_past )
View(df1)
df2 = subset(df,df$Cit_pref1!=df$party_voted_past )
View(df2)
val1 = subset(val,val$party_voted_past==val$Cit_pref1)
View(val1)
val2 = subset(val,val$party_voted_past!=val$Cit_pref1)
View(val2)
write.csv(df1,"traincit=last.csv")
write.csv(df2,"trainnotcitlast.csv")
write.csv(val1,"validequalcitlast.csv")
write.csv(val2,"validnotequalcitlast.csv")

td = read.csv("lead56.csv")
td$t=0
unique(td$Centaur==td$Centaur.1)
#breakVector[,1] <- factor(breakVector[,1], levels=levels(FinalTable[,1))
#########################################################################################
train = read.csv("Train2.csv")
final = read.csv("final2.csv")
train$ind = train$lev_pref1 - train$lev_pref2
final$ind = final$lev_pref1 - final$lev_pref2

library(lattice)
with(train, xyplot(lev_pref2 ~ ind, group=(actual_vote==party_voted_past)))

hist(train$lev_pref1, col=rgb(1,0,0,0.5) ,main="Lev_pref1 vs Lev_pref2")
hist(train$lev_pref2, col=rgb(0,0,1,0.5), add=T)
box()


train1 = train[train$ind>3.75,]
train2 = train[train$ind<=3.75,]

final1 = valid[final$ind>3.75,]
final2 = valid[final$ind<=3.75,]
write.csv(train1,"tra1.csv")
write.csv(train2,"tra2.csv")
write.csv(valid1,"vali1.csv")
write.csv(valid2,"vali2.csv")



train1$ind1 = as.logical(train1$lev_pref1>10)

valid1$ind1 = as.logical(valid1$lev_pref1>10)

train2$ind1 = as.logical(train2$lev_pref1>10)

valid2$ind1 = as.logical(valid2$lev_pref1>10)


df3 = subset(train2,train2$ind1==TRUE)
df4 = subset(train2,train2$ind1==FALSE)
val3 = subset(valid2,valid2$ind1==TRUE)
val4 = subset(valid2, valid2$ind1==FALSE)

df1 = subset(train1,train1$ind1==TRUE)
df2 = subset(train1,train1$ind1==FALSE)
val1 = subset(valid1,valid1$ind1==TRUE)
val2 = subset(valid1,valid1$ind1==FALSE)

df1 = rbind(df1,df3)
df2 = rbind(df2,df4)
val1 = rbind(val1,val3)
val2 = rbind(val2,val4)

write.csv(df1,"train1levpref1-10-6.5.csv")
write.csv(df2,"train2levpref1-10-6.5.csv")
write.csv(val1,"valid1levpref1-10-6.5.csv")
write.csv(val2,"valid2levpref1-10-6.5.csv")

##########################################################################################
x = 6
y = seq(4, 8, by = 1)
z = 0.1
lis = c()
ao = 0
bo = 0
for (i in x){
  for (j in y){
    for (k in z){
      train = read.csv("T.csv")
      valid = read.csv("V.csv")
      train1 = train[train$lev_pref1>i,]
      train2 = train[train$lev_pref1<=i,]
      train1$ind = train1$lev_pref1 - train1$lev_pref2
      train1$ind = as.logical(train1$ind>j)
      train2$ind = train2$lev_pref1 - train2$lev_pref2
      train2$ind = as.logical(train2$ind>k)
      df3s = subset(train2,train2$ind==TRUE)
      df4s = subset(train2,train2$ind==FALSE)
      
      df1s = subset(train1,train1$ind==TRUE)
      df2s = subset(train1,train1$ind==FALSE)
      
      df1t = rbind(df1s,df3s)
      df2t = rbind(df2s,df4s)
      df1 = as.h2o(df1t)
      df2 = as.h2o(df2t)
      df1$C1=NULL
      df2$C1=NULL
      df1$X=NULL
      df2$X=NULL
      df1$ind=NULL
      df2$ind=NULL
      splits <- h2o.splitFrame(df1,c(0.8),seed=1234)
      train <- h2o.assign(splits[[1]], "train.hex") 
      test <- h2o.assign(splits[[2]], "test.hex")
      x = c(3:33,37:43,45:52)
      y = 36
      m1 = h2o.glm(training_frame = train,x = x,y = y,family='multinomial',solver='L_BFGS')
      finalRf_predictions<-h2o.predict(object = m1,newdata = test)
      a=mean(finalRf_predictions$predict==test$actual_vote)
      splits <- h2o.splitFrame(df2,c(0.8),seed=1234)
      train <- h2o.assign(splits[[1]], "train.hex") 
      test <- h2o.assign(splits[[2]], "test.hex")
      x = c(3:33,37:43,45:52)
      y = 36
      m1 = h2o.glm(training_frame = train,x = x,y = y,family='multinomial',solver='L_BFGS')
      finalRf_predictions<-h2o.predict(object = m1,newdata = test)
      b=mean(finalRf_predictions$predict==test$actual_vote)
      v = c(i,j,k,a,b)
      lis = c(lis,v)
    }
  }
}

##################################################################################################
train = read.csv("T.csv")
valid = read.csv("V.csv")
train$ind = train$lev_pref1 - train$lev_pref2
valid$ind = valid$lev_pref1 - valid$lev_pref2
boxplot(train$lev_pref1)
boxplot(train$lev_pref2)
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
outlierKD(train,lev_pref1)

z <- train[train$lev_pref1 < (quantile(train$lev_pref1, .25) - 1.5*IQR(train$lev_pref1)) & train$lev_pref1 > (quantile(train$lev_pref1, .75) + 1.5*IQR(train$lev_pref1)),]


quantile(train$lev_pref1, .25)


1.5*IQR(train$lev_pref1)
quantile(train$lev_pref1, .75)



















