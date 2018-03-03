#######################################
#    Author : Harshit Saxena          #
#   (https://github.com/harsh1795)    #
#######################################

#-------------------------------------------------------------------------------------------------------------------------#
# Setting Working Directory
setwd("C:/Users/Harshit/Desktop/GS") 

#-------------------------------------------------------------------------------------------------------------------------#
# Importing Libraries and Data

library(lubridate)
library(missForest)
library(lubridate)
library(data.table)

# Importing data
train = read.csv("file:///C:/Users/Harshit/Desktop/New folder/ML_Bond_metadata_corrected_dates.csv")
df = read.csv("file:///C:/Users/Harshit/Desktop/New folder/dataset.csv")

#-------------------------------------------------------------------------------------------------------------------------#
# Data Preprocessing

View(df)
View(train)
attach(train)
## Missing values
## "DF" contains no NA Values. Now testing "TRAIN" Dataset.
z=(is.na(train$issueDate))
which(z,arr.ind = TRUE)
## Row no. 671 has one na value for issue.date.
train$couponFrequency[train$coupon==0] = 0
## Row no. 8710 has one NA value for couponFrequency.
#3 238 entries are there for NA values in Maturity.
## For all rating given by agency1 as watch0 are NA. Which means they are not rated.There are total of 16873 bonds.
## For agency 2 there are 1266 ratings with watch0 and NA values.
## Converting DATE FORMAT...

train$issue.date = parse_date_time(train$issueDate,orders = c("%d-%m-%Y",library(missForest)"dmy","dBY"))
train$maturity = parse_date_time(train$maturity,orders = c("%d-%m-%Y","dmy","dBY"))
train$issueDate=NULL

train$issue.weekday = lapply(train$issue.date,function(x){wday(as.Date(as.character(x),'%Y-%m-%d'))})
train$maturity.weekday = lapply(train$maturity,function(x){wday(as.Date(as.character(x),'%Y-%m-%d'))})
train$maturity.date = as.numeric(format(train$maturity,format="%d"))
train$issue.dates = as.numeric(format(train$issue.date,format="%d"))
train$maturity.month = as.numeric(format(train$maturity,format="%m"))
train$issue.month = as.numeric(format(train$issue.date,format="%m"))
train$maturity.Year = as.numeric(format(train$maturity,format="%Y"))
train$issue.Year = as.numeric(format(train$issue.date,format="%Y"))

## missing values
data= train
data$ratingAgency1EffectiveDate=NULL
data$ratingAgency2EffectiveDate=NULL
data$maturity=NULL
data$issue.date=NULL
data$isin=NULL
data$issuer=NULL
data$maturity.weekday=NULL
View(data)

## Converting into factors
nn = c(1,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
data[,nn] = lapply(data[,nn],function(x){as.factor(x)})

## Datetime objects
df$date = parse_date_time(df$date,orders = c("dBY"))
df1 = df
df1$time=NULL
a = head(sort(unique(df1$date), decreasing=TRUE), 5)
df11 = df1[df1$date==a[1],]
df12 = df1[df1$date==a[2],]
df13 = df1[df1$date==a[3],]
df14 = df1[df1$date==a[4],]
df15 = df1[df1$date==a[5],]
df1 = rbind(df11,df12,df13,df14,df15)
df1$timeofday=NULL
df1$price=NULL

# exploring data analysis
DT <- data.table(df1)
DT$date=NULL
sell_DT = DT[DT$side=="S",]
buy_DT = DT[DT$side=="B",]
Df_sell <- aggregate(. ~  isin, data = sell_DT, mean)
Df_buy <- aggregate(. ~  isin, data = buy_DT, mean)
Dff = merge(Df_sell,Df_buy,by='isin',all.y = TRUE)
ef = data.frame(train$isin)
names(ef) = "isin"
final = merge(ef,Dff,by='isin',all.x = TRUE)
final$side.x=NULL
final$side.y=NULL
tt = merge(train,final,by='isin',all.x = TRUE, sort=FALSE)
tt = tt[c('isin','volume.x','volume.y')]
names(tt) = c('isin','buyvolume','sellvolume')
tt['buyvolume'] = tt$buyvolume*3.1
tt$sellvolume = tt$sellvolume*2
tt[is.na(tt)] = 0

write.csv(tt,"file:///C:/Users/Harshit/Desktop/New folder/Output.csv")

names(df1)
df_isin = train$isin[1033]
bond = df[df$isin==df_isin,]

#-------------------------------------------------------------------------------------------------------------------------#

