library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(tseries)
library(forecast)
setwd("D:/")
getwd()
Data<-read.csv("cum.csv",header=TRUE)
attach(Data)
plot(Data)
head(Data)
#Creating a new data frame with Confirmed Cases
df <- Data[2]
df


#Converting it to Time Series Object
#tsdata <- ts(Data[,2],start=c(2020,1), end=c(2020, 4),frequency=12)
tsdata <- ts(log(Data[,2]),start=c(2020,1))
ts.plot(tsdata,xlab='Years', ylab = 'Confirmed Cases')


adf.test(tsdata)




 tsdata1 <- diff(tsdata, differences = 2)
 adf.test(tsdata1)
 plot(tsdata1)
 #Plotting the ACF plot and PACF
 acf(tsdata1)
 pacf(tsdata1,lag.max=34)
 
 
 #Fitting ARIMA (1,2,3) model
 fit1 <- arima(df$`Confirmed`, c(1,2,3))
 fit1
 #Forecasting
 forecast1 <- forecast(fit1, h = 10)
 forecast1
 plot(forecast1)

 library(forecast)
 library(dplyr)
 #Fitting auto.arima ()
 fit2 <- auto.arima(df$`Confirmed`, seasonal = FALSE)
 fit2
 #Forecasting
 forecast2 <- forecast(fit2, h = 10)
 summary(forecast2)
 plot(forecast2)
 #(forecast2,type='l',xlim=c(2020,2020),xlab = 'Year',ylab = 'Confirmed Cases')
# lines(10^(forecast2$pred),col='blue')
 #lines(10^(forecast2$pred+2*forecast2$se),col='orange')
 #lines(10^(forecast2$pred-2*forecast2$se),col='orange')
 
 plot.ts(forecast2$residuals)
 acf(forecast2$residuals, lag.max=20)
 Box.test(forecast2$residuals, type="Ljung-Box") 
 Box.test(forecast2$residuals,lag=5, type="Ljung-Box")  
 Box.test(forecast2$residuals,lag=10, type="Ljung-Box") 
 Box.test(forecast2$residuals,lag=15, type="Ljung-Box") 
 #boxresult <- LjungBoxTest(forecast2$residuals,k=2,StartLag=1)
 #plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
 qqnorm(forecast2$residuals)
 qqline(forecast2$residuals)
 
 
 #library(FitAR)

 predicted_arima <- forecast(fit2, h = 10)$mean %>% as.vector()
 
 
 df_arima_next10 <- data_frame(Date = Data$Date %>% max() + 1:10, 
                               Predicted = predicted_arima %>% round(0))
 
 
 df_arima_next10 %>% knitr::kable(caption = "Table 1: Deaths by Coronavirus Predicted for Next 10 Days")
 
 
 
 