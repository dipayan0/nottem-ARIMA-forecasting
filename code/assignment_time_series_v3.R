ID=06
#Average Monthly Temperatures at Nottingham, 1920–1939
library(tseries)
library(forecast)
library(itsmr)

N=length(nottem)

#outliers checking
tsoutliers(nottem)

#k=tsclean(nottem)

#k-nottem

decompose(nottem, type="additive")

plot(decompose(nottem, type="additive"))



### Time Plot, ACF plot, Seasonal Sub series plot
ts.plot(nottem, main="nottem dataset plot", xlab="Time (1920-1939)", ylab="Temperature (ºF)")
#plot(nottem)
acf(nottem, lag.max=36, main="ACF plot of nottem dataset",  xlab="Lag", ylab="corr coefficient")
pacf(nottem, lag.max=36,  main="PACF plot of nottem dataset",  xlab="Lag", ylab="partial corr coefficient")

ggsubseriesplot(nottem,main="Sub-Series plot of nottem dataset",  xlab="Months", ylab="Temperature (ºF)")




#ggplot(nottem)+geom_boxplot()
Col<- c('Jan','Feb','Mar','Apl','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
boxplot(as.vector(nottem)~cycle(nottem), data=nottem, names=Col, col="lightgreen", border="black",main = "Boxplot of nottem Dataset", xlab="Month", ylab = "Temperature (ºF)")
#boxplot(as.vector(nottem)~cycle(nottem), nottem, main = "Boxplot of nottem Dataset", ylab = "Temperature (ºF)")


# Convert the dataset to a data frame
#nottem_df <- data.frame(month = cycle(nottem), temperature = as.vector(nottem))

# Create a boxplot of temperature by month
#boxplot(temperature ~ month, data = nottem_df,
#        main = "Month-wise Boxplot of nottem Dataset",
#        xlab = "Month", ylab = "Temperature (ºF)")



## Training and test data split


n_train=ceiling(N*0.7)
n_train
n_test= N - n_train

trainData=ts(nottem[1:n_train], start=c(1920,1), frequency=12)
acf(trainData)
plot(trainData, main="training dataset", ylab="Temperature (ºF)")
length(trainData)
idx=n_train + 1

testData=ts(nottem[idx:N],start=c(1934,1), frequency =12)
ts.plot(trainData,testData, col=c('blue','red'),main="Training (blue) and Test (red) Dataset", ylab="Temperature (ºF)")
#legend(5,5, legend=c("Train Dataset", "Test Dataset"),  
#       fill = c("blue","red") 
#)
length(testData)

####   stationarity check
library(tseries)
adf.test(trainData)     # p-value = 0.01 < 0.05, null hypo is rejected, hence stationary
kpss.test(trainData)    #p-value = 0.1 >0.05, hence null hypo not rejected, stationary


#seasonal differencing and stationarity checking of the difference data


ddnottem=diff(trainData, differences = 1)
plot(ddnottem)
acf(ddnottem)

pacf(ddnottem)
adf.test(ddnottem)
kpss.test(ddnottem)



dnottem=diff(trainData, lag  =12)
ts.plot(dnottem, main="Seasonal difference of nottem")

acf(dnottem, lag.max=36, main="ACF plot of seasonal difference of nottem")
pacf(dnottem, lag.max=36,main="PACF plot of seasonal difference of nottem")
adf.test(dnottem)
kpss.test(dnottem)



#ddnottem=diff(dnottem, lag  =24)
#ts.plot(ddnottem, main="Seasonal difference of nottem")
#acf(ddnottem, lag.max=36, main="ACF plot of Seasonal difference of nottem")
#pacf(ddnottem, lag.max=36, main="PACF plot of Seasonal difference of nottem")
#adf.test(ddnottem)
#kpss.test(ddnottem)


#### Model selection

acf(trainData)
pacf(trainData)


model <- function(data)
{ library(tseries)
  order=c(2,0,1)
  seasonal= list(order=c(2,1,2), period=12)
  M=Arima(data, order=order, seasonal=seasonal)
  return (M)
  
}


#M1=Arima(trainData, =Arima(trainData, order=c(3,0,1), seasonal = list(order=c(0,0,0), period=12))
M1 =Arima(trainData, order=c(2,0,2), seasonal = list(order=c(1,1,1), period=12))  #sigma^2 estimated as 8.445:  log likelihood = -479.38,  aic = 970.77

M1
test(M1$residuals) 
shapiro.test(M1$residuals)

library(forecast)
plot(forecast::forecast(M1, h=n_test))
 #### Transform no 1

plot(trainData, main="Train Data")

Log_trainData=log(trainData)
plot(Log_trainData, main="Log Transformed Train Data")
adf.test(Log_trainData)     #Dickey-Fuller = -11.885,Lag order = 5, p-value = 0.01
kpss.test(Log_trainData)    #KPSS Level = 0.025818, lag parameter = 4, p-value = 0.1

sqrt_trainData=sqrt(trainData)
plot(sqrt_trainData, main="Square root Transformed Train Data")
adf.test(sqrt_trainData)     # Dickey-Fuller = -11.796, Lag order = 5, p-value = 0.01
kpss.test(sqrt_trainData)     #KPSS Level = 0.02829, Truncation lag parameter = 4, p-value = 0.1



cubicrt_trainData=(trainData)^(1/3)
plot(cubicrt_trainData, main="Cubic root Transformed Train Data")
adf.test(cubicrt_trainData)  #Dickey-Fuller = -11.824, Lag order = 5, p-value = 0.01
kpss.test(cubicrt_trainData)  # KPSS Level = 0.027419, Truncation lag parameter = 4, p-value = 0.1

ts.plot(Log_trainData, sqrt_trainData, cubicrt_trainData, col=c("red", "blue","darkgreen"), main="Transformed Train Data", ylab="transformed temp")



##Log transform is selected

acf(Log_trainData, lag.max=36)
pacf(Log_trainData,  lag.max=36)

dLog_trainData=diff(Log_trainData, lag=12)
acf(dLog_trainData, lag.max=36)
pacf(dLog_trainData, lag.max=36)


M2=Arima(Log_trainData, order=c(2,0,1), seasonal = list(order=c(1,1,1), period=12))
#M2=model(Log_trainData)
M2  #sigma^2 estimated as 8.445:  log likelihood = -479.38,  aic = 970.77
  `#install.packages("itsmr")
test(M2$residuals) ##p-value = 2.431e-05
shapiro.test(M2$residuals)

AIC(M2)

#### Forecast
n=length(testData)
plot(forecast::forecast(M2, h=n))



### M3 after BoxCox transformation

lamda <- BoxCox.lambda(trainData)
lamda
Boxcox_trainData=BoxCox(trainData, lambda = lamda)
plot(Boxcox_trainData)
adf.test(Boxcox_trainData)
kpss.test(Boxcox_trainData)


acf(Boxcox_trainData)
pacf(Boxcox_trainData)


M3=Arima(Boxcox_trainData, order=c(2,0,1), seasonal = list(order=c(1,1,1), period=12))
#M3=model(Boxcox_trainData)
M3
test(M3$residuals) 
shapiro.test(M3$residuals)

### M4

M4=auto.arima(trainData)
M4   #  Arima(1,0,2)(1,1,2)[12] with drift sigma^2 = 5.389:  log likelihood = -410.74  AIC=837.49   AICc=838.33   BIC=863.03
test(M4$residuals) 
shapiro.test(M4$residuals)


#### Forecasting using the models
n=length(testData)


## M1
frcst_M1 <- forecast::forecast(M1, h=n)
plot(forecast::forecast(M1, h=n_test), xlab="Time", ylab="Temperature (ºF)")
ts.plot(frcst_M1$mean, testData, col=c("red","darkgreen"),main="Forecasing with M1 (red: forecasted mean value, green:test data)", ylab="Temperature (ºF)")
frcst_M1
#ts.plot(frcst_M1$lower, frcst_M1$upper, testData, col = c("red","darkred","blue","darkblue","darkgreen"),main="Forecasing with M1 (red & blue: CI, green:test data)", ylab="Temperature (ºF)")
ts.plot(frcst_M1$lower, frcst_M1$upper, testData, col = c("red","blue","red","blue","darkgreen"),main="Forecasing with M1 (red & blue: 80% & 95% CI, green:test data)", ylab="Temperature (ºF)")


## M2
scaled_frcst_M2 <- forecast::forecast(M2, h=n)
scaled_frcst_M2
frcst_M2=exp(scaled_frcst_M2$mean)
frcst_M2
plot(frcst_M2)
ts.plot(frcst_M2, testData, col=c("red","darkgreen"),main="Forecasing with M2 (red: forecasted mean value, green:test data)", ylab="Temperature (ºF)")
#ts.plot(exp(scaled_frcst_M2$lower), exp(scaled_frcst_M2$upper), testData, col = c("red","darkred","blue","darkblue","darkgreen"))
ts.plot(exp(scaled_frcst_M2$lower), exp(scaled_frcst_M2$upper), testData, col = c("red","blue","red","blue","darkgreen"),main="Forecasing with M2 (red: 80% CI, blue: 95% CI, green:test data)", ylab="Temperature (ºF)")

frcst_M2Lo80=exp(scaled_frcst_M2$lower[,1])
frcst_M2Lo95=exp(scaled_frcst_M2$lower[,2])
frcst_M2Hi80=exp(scaled_frcst_M2$upper[,1])
frcst_M2Hi95=exp(scaled_frcst_M2$upper[,2])

#plot(frcst_M2)
#frcst_M2

ts.plot(trainData, frcst_M2,frcst_M2Lo80,frcst_M2Lo95, frcst_M2Hi80, frcst_M2Hi95, col=c('blue','orange','green', 'darkgreen','red','darkred'),main="Forecast from M2", ylab="Temperature (ºF)")




## M3
scaled_frcst_M3 <- forecast::forecast(M3, h=n_test)
frcst_M3=InvBoxCox(scaled_frcst_M3$mean, lambda=lamda)
frcst_M3

ts.plot(trainData, frcst_M3, testData, col=c("blue","red","darkgreen"),main="Forecasing with M3 (blue: train data, red: forecasted value, green:test data)", ylab="Temperature (ºF)")
ts.plot(frcst_M3, testData, col=c("red","darkgreen"),main="Forecasing with M3 (red: forecasted mean value, green:test data)", ylab="Temperature (ºF)")

#ts.plot(InvBoxCox(scaled_frcst_M3$lower, lambda=lamda), InvBoxCox(scaled_frcst_M3$upper, lambda=lamda), testData, col = c("red","darkred","blue","darkblue","darkgreen"))
ts.plot(InvBoxCox(scaled_frcst_M3$lower, lambda=lamda), InvBoxCox(scaled_frcst_M3$upper, lambda=lamda), testData, col = c("red","blue","red","blue","darkgreen"),main="Forecasing with M3 (red & blue: 80% & 95% CI, green:test data)", ylab="Temperature (ºF)")




frcst_M3Lo80=InvBoxCox(scaled_frcst_M3$lower[,1],  lambda=lamda)
frcst_M3Lo95=InvBoxCox(scaled_frcst_M3$lower[,2],  lambda=lamda)
frcst_M3Hi80=InvBoxCox(scaled_frcst_M3$upper[,1],  lambda=lamda)
frcst_M3Hi95=InvBoxCox(scaled_frcst_M3$upper[,2],  lambda=lamda)

#plot(frcst_M2)
#frcst_M2

ts.plot(trainData, frcst_M3,frcst_M3Lo80,frcst_M3Lo95, frcst_M3Hi80, frcst_M3Hi95, col=c('blue','orange','green', 'darkgreen','red','darkred'))
#legend(2,10, legend=c('blue','orange','green', 'darkgreen','red','darkred'))


##M4

frcst_M4 <- forecast::forecast(M4, h=n)
plot(frcst_M4)
frcst_M4
plot(forecast::forecast(M4, h=n_test), xlab="Time", ylab="Temperature (ºF)")

#ts.plot(frcst_M4$lower, frcst_M4$upper, testData, col = c("red","darkred","blue","darkblue","darkgreen",)main="Forecasing with M4 (red: forecasted mean value, green:test data)", ylab="Temperature (ºF)")
ts.plot(frcst_M4$mean, testData, col=c("red","darkgreen"),main="Forecasing with M4 (red: forecasted mean value, green:test data)", ylab="Temperature (ºF)")

ts.plot(trainData, frcst_M4$mean, testData, col=c("blue","red","darkgreen"),main="Forecasing with M4 (blue: train data, red: forecasted value, green:test data)", ylab="Temperature (ºF)")

ts.plot(frcst_M4$lower, frcst_M4$upper, testData, col = c("red","blue","red","blue","darkgreen"),main="Forecasing with M4 (red & blue: 80% & 95% CI, green:test data)", ylab="Temperature (ºF)")




### Accuracy Measures

AccuracyMeasures <- function(predicted,testData){
  n=length(testData)
  error = testData - predicted
  ME    = sum(error)/n
  MSE   = sum(error^2)/n
  RMSE  = sqrt(MSE)
  MAE   = sum(abs(error))/n
  PE    = (error/testData)*100
  MPE   = sum (PE)/n
  MAPE  = sum(abs(PE))/n
  
  return (c(MSE, RMSE, MAE, MPE, MAPE))
  }

AM_M1 <- AccuracyMeasures(frcst_M1$mean,testData)
AM_M2 <- AccuracyMeasures(frcst_M2,testData)
AM_M3 <- AccuracyMeasures(frcst_M3,testData)
AM_M4 <- AccuracyMeasures(frcst_M4$mean,testData)

AM_M1
AM_M2
AM_M3
AM_M4

#best model is M1 model

## Forecasting 10 points ahead and plotting

#M4=auto.Arima(nottem)
#M4   #  Arima(1,0,2)(1,1,2)[12] with drift sigma^2 = 5.22:  log likelihood = -516.48  AIC=1048.96   AICc=1049.62   BIC=1076.4

M1 =Arima(nottem, order=c(2,0,2), seasonal = list(order=c(1,1,1), period=12))  #sigma^2 estimated as 8.445:  log likelihood = -479.38,  aic = 970.77

M1

frcst_M1 <- forecast::forecast(M1, h=10)
plot(frcst_M1, xlab="Time", ylab="Temperature (ºF)")
frcst_M1
#last_6_years=ts(nottem[180:240], start=c(1935,1), frequency=12)
#ts.plot(frcst_M1$lower, frcst_M1$upper, frcst_M1$mean, nottem, col = c("red","darkred","blue","darkblue","darkgreen","blue"))
ts.plot(frcst_M1$lower, frcst_M1$upper, frcst_M1$mean, nottem, col = c("red","blue","red","blue","darkgreen","green"),main="Forecasing with M1 (red & blue: 80% & 95% CI, green:test data)", ylab="Temperature (ºF)")
ts.plot(frcst_M1$lower, frcst_M1$upper, frcst_M1$mean, col = c("red","blue","red","blue","darkgreen"),main="Forecasing 10 points with M1 (red & blue: 80% & 95% CI, green: point forecast)", ylab="Temperature (ºF)")



