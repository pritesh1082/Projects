library(data.table)
library(fpp)
library(fpp2)
## Cleansing and transformation ##
currency <- USD_INR_Historical_Data ### 9 years data from 11/01/2010 (2370 total obs)
str(currency)
class(currency)
setDT(currency)
table(is.na(currency$Price))
# FALSE 
# 2370 ## No NA/NULL values
currency[,Date:=as.Date(Date,'%d-%b-%y')] ## converting Date column to Date format
min(currency$Date)
max(currency$Date)
tscurr <- ts(currency$Price, start = c(2010,11), frequency = 260)
plot(tscurr) ## original plot


## 4 basic methods and defining training and test data ##
train <- window(tscurr, start = c(2010,11),end = c(2017,10)) ##training data from 11/1/2010 till 10/20/2017
autoplot(tscurr) + 
  autolayer(meanf(train,h=780), series="MEAN", PI=FALSE) +
  autolayer(naive(train,h=780), series="NAIVE", PI=FALSE) +
  autolayer(snaive(train,h=780), series="SEASONAL NAIVE", PI=FALSE) +
  autolayer(rwf(train,h=780,drift = TRUE), series="DRIFT", PI=FALSE) +
  ggtitle("Forecast INR rate for next 780 days(3 years)") +
  xlab("Time") + ylab("USDINR rate")

test <- window(tscurr, start = c(2017,10))###### test data from 10/23/2017 until 11/29/2019

ggAcf(train)#### on my training data


meantscurr <- meanf(train,h=780)
naivetscurr <- naive(train,h=780)
snaivetscurr <- naive(train,h=780)
drifttscurr <- rwf(train,h=780,drift = TRUE)

meantscurr ## 
naivetscurr ## last obs 65.040
snaivetscurr
drifttscurr ##

summary(meantscurr)
summary(naivetscurr)
summary(snaivetscurr)
summary(drifttscurr)


accuracy(meantscurr,test)###### 
accuracy(naivetscurr,test)##### 
accuracy(snaivetscurr,test)
accuracy(drifttscurr,test)###### comparing mean, naive and drift
## for both training and test data, drift provides the best estimate



##4 decomposition using stl##################################

fitdecomp <- stl(tscurr,s.window = 5) ## Additive decomposition using STL
fitdecomp
plot(fitdecomp)
plot(tscurr) ## original time series
lines(fitdecomp$time.series[,2],col="blue",ylab="Trend") ## Blue line is the trend comp
summary(fitdecomp)

plot(tscurr) ## original time series
plot(tscurr,col='grey',xlab="Time",ylab="USDINR Rate",main='Seasonally adjusted data with original series')
lines(seasadj(fitdecomp),col="red",ylab="Seasonally adjusted")### Trend + error
lines(fitdecomp$time.series[,2],col="blue",ylab="Trend")## only Trend line

# 4 Naive forecasts of seasonally adjusted data
fitsadj <- stl(tscurr, t.window=13, s.window="periodic", robust=TRUE)
sadj <- seasadj(fitsadj)
#fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") + ggtitle("Naive forecasts of seasonally adjusted data")
plot(naive(sadj,h=44), xlab="Time",main="Naive forecasts of seasonally adjusted data")
naive(sadj,h=44)#### 71.78824 
summary(sadj)

## model on training data ##
fitsadj <- stl(train, t.window=13, s.window="periodic", robust=TRUE)
sadj <- seasadj(fitsadj)
#fit %>% seasadj() %>% naive() %>% autoplot() + ylab("New orders index") + ggtitle("Naive forecasts of seasonally adjusted data")
plot(naive(sadj,h=44), xlab="Time",main="Naive forecasts of seasonally adjusted data")
sdecomp <- naive(sadj,h=44)#### 71.78824 
sdecomp
summary(sadj)
accuracy(sdecomp,test)


# ME              RMSE       MAE        MPE         MAPE       MASE       ACF1        Theil's U
# Tr 0.01042683   0.3026986  0.1969408   0.01809994  0.339254   0.04833626 0.02079706        NA
# Test 1.05309673 1.1154868  1.0530967   1.62484661  1.624847   0.25846727 0.86711350  7.162666


# 5 SES ##########

## default alpha##
train <- window(tscurr, start = c(2010,11),end = c(2017,10)) ##training data from 11/1/2010 till 10/20/2017
fitses <-ses(train, h=780) ##Simple Exponential Smoothing, suitable for no trend + linear
fitses
plot(fitses,xlab="Time",ylab="INR per USD") ## ## forecast
fitses$model ### alpha = 0.9999 and l0 = 44.4691 
# AIC     AICc      BIC 
# 9062.529 9062.543 9079.049 

summary(fitses)
accuracy(fitses,test)

# ME          RMSE       MAE        MPE      MAPE       MASE       ACF1     Theil's U
# 0.01130381 0.282172 0.1914352 0.01973781 0.3293246 0.04698499 0.03317511        NA
# 3.85854452 4.784364 4.1104883 5.43775299 5.8315051 1.00885956 0.99298179  17.75148

## with alpha = 0.5 ##
plot(train,col='grey',xlab="Time",ylab="USDINR Rate",main='Simple exponential smoothing with Alpha = 0.5')
fitses1 <-ses(train, alpha=0.5, initial="simple", h=780)
lines(fitted(fitses1), col="blue") ## plot the model
plot(fitses1,xlab="Time",ylab="USDINR Rate") ## ## forecast
fitses1$model ## alpha = 0.5, l= 44.47
summary(fitses1)
accuracy(fitses1,test)

## 6 Holt's linear
plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Holt linear model')
fitholt <-holt(train, h=780)## Holt's linear
lines(fitted(fitholt),col='red') ## plot the model
fitholt$model ## alpha = 0.9999 and beta  = 1e-04 
summary(fitholt)
# AIC     AICc      BIC 
# 9069.610 9069.643 9097.143 


fitr <- fitholt$residuals
fitr
ggAcf(fitr)############# indicates that the correlation between residuals are statistically zero

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Holt linear model')
fcholt=forecast(fitholt,5)
lines(fcholt$mean,col='blue') ## forecast
fcholt$mean
max(fcholt$upper)
min(fcholt$lower)
fcholt$upper
fcholt$lower
plot(fcholt) ## forecast with CI
accuracy(fcholt,test)
# ME            RMSE       MAE          MPE      MAPE       MASE       ACF1       Theil's U
# -0.001503539  0.282411 0.1924859 -0.002151882 0.3313205 0.04724287 0.03254038        NA
# 0.388315278   1.947385 1.4632847  0.483675289 2.1025475 0.35914194 0.99024134   7.250277

autoplot(tscurr) +
  autolayer(fcholt, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("USD/INR Exchange Rate ") +
  guides(colour=guide_legend(title="Forecast"))

##7 Forecasting with ARIMA ####################

# 1 - KPSS Test for Level Stationarity

library(urca)

Test=ur.kpss(tscurr) 
summary(Test)

# Value of test-statistic is: 22.3652 
# 
# Critical value for a significance level of: 
#   10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739

# Since F-Statistic (22.3652) > critical values at all significance,
# hence, we reject H0 (null hypethesis) i.e. non-stationary,
# hence differencing is required



ndiffs(tscurr) ## to determine how many order differencing is required
train1 <- diff(tscurr)
Test1 <- ur.kpss(train1)
summary(Test1)
# Value of test-statistic is: 0.1102 
# 
# Critical value for a significance level of: 
#   10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739

# Now F-statistic (0.1102) is less than all significance level,
# hence we fail to reject H0, i.e. dataset is stationary now

Acf(tscurr)
Pacf(tscurr)
Acf(train1) ####### Acf of the differenced series
Pacf(train1)

arimamodel <- auto.arima(train,seasonal = FALSE)
summary(arimamodel) ## ARIMA(5,1,0) with drift

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Arima model')
model1 <- arima(train,order=c(4,1,1))
plot(model1$residuals)
summary(model1)

# since the p-value > .05, we fail to reject H0. Thus residuals are not correlated.
# which means our model is validated and good to go for prediction.
Box.test(model1$residuals,10,type='Ljung') # check the residuals Box-Ljung test 
Box.test(model1$residuals^2,10,type='Ljung') 
fcasta <- forecast(model1,h=780)
plot(fcasta)
lines(fitted(model1), col="yellow") ## plot the model
model1
fcasta
accuracy(fcasta,test)
# ME          RMSE       MAE        MPE      MAPE       MASE         ACF1       Theil's U
# 0.01028464  0.2784941 0.1919017 0.01800265 0.3306373 0.04709948 -0.001077606        NA
# 3.81874475  4.7521744 4.0851235 5.37990323 5.7960902 1.00263413  0.992998784  17.63158

plot(train,xlab="Time",ylab="USD/INR Exchange Rate ",main='Arima model')
model11 <- arima(train,order=c(5,1,0))
model11
summary(model11)
fcasta11 <- forecast(model11,h=780)
plot(fcasta11)
accuracy(fcasta11,test)


