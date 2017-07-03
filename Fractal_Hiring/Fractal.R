#author = Black_Viper
setwd("C:/Users/212630200/Desktop/New folder/Hackathon/AV_Fractal/")
###  Importing Libraries
library(xts)
library(forecast)
library(lubridate)
library(ggplot2)
library(tseries)
library(plyr)
library(data.table)
### Loading Data

losss <- function(error){
  loss =  sqrt(mean(error^2))
  return(loss)
}


dat = read.csv("train.csv", stringsAsFactors = FALSE)

dat$Datetime = as.Date(dat$Datetime)
dat$Category_3 = as.factor(dat$Category_3)
dat$Category_2 = as.factor(dat$Category_2)

plot_1 = ggplot(dat, aes(Datetime, Price)) + geom_line() + scale_x_date('month')  + ylab("Daily Price") +
  xlab("")
qplot(count_ItemID$freq,geom = "histogram",binwidth = 0.5)
plot_2 = ggplot() + geom_line(aes(y = freq, x = Item_ID), data = count_ItemID, stat = "identity")

## got amazing plot for each Item ID
dat_29655 = dat[dat$Item_ID==29655,]
ggplot(dat_29654, aes(Datetime, Number_Of_Sales)) + geom_line() + scale_x_date('month')  + ylab("Daily Number of sales") +
  xlab("")
ggplot(dat_29654, aes(Datetime, Price)) + geom_line() + scale_x_date('month')  + ylab("Daily Price") +
  xlab("")

##making subsets with itemID
train_30375 = dat_30375[1:(0.7*nrow(dat_30375)),]
test_30375 = dat_30375[(nrow(train_30375)+1):nrow(dat_30375),]
price_ts = ts(dat_30375[, c('Price')])
dat_30375$clean_price = tsclean(price_ts)

ggplot(dat_30375, aes(Datetime, clean_price)) + geom_line() + scale_x_date('month')  + ylab("Daily Price") +
  xlab("")

dat_30375$price_ma7 = ma(dat_30375$clean_price, order=7)
dat_30375$price_ma30 = ma(dat_30375$clean_price, order=30)

ggplot() +
  geom_line(data = dat_30375, aes(x = Datetime, y = clean_price, colour = "Price")) +
  geom_line(data = dat_30375, aes(x = Datetime, y = price_ma7,   colour = "Weekly Moving Average"))  +
  geom_line(data = dat_30375, aes(x = Datetime, y = price_ma30, colour = "Monthly Moving Average"))  +
  ylab('Price')


price_ma = ts(na.omit(dat_30375$price_ma7), frequency=30)
decomp = stl(price_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(price_ma, alternative = "stationary")
Acf(price_ma, main='')
Pacf(price_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(3,1,2) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(2,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=240)
plot(fcast)



hold <- window(ts(deseasonal_cnt), start=637)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(638:911)]), order=c(3,1,2))

fcast_no_holdout <- forecast(fit_no_holdout,h=274)
plot(fitted, main=" ")
lines(ts(deseasonal_cnt))

#fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
#fit_w_seasonality

zz = as.data.frame(fcast_no_holdout)
y = as.vector(zz$`Point Forecast`)
jj = as.vector(dat_30375$Price[638:911])
error = y-jj
rmse = losss(error)

library(smooth)
library(Mcomp)
