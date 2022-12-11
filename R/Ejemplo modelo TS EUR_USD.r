
#https://www.kaggle.com/kailex/arima-with-fourier-terms



library(alphavantager)
library(readr)
library(magrittr)
library(forecast)
library(TSA)
library(data.table)

library(tsfgrnn)

av_api_key('NHN6N8730K3FXK31')

#### EXTRACCIÓN ####

EUR_USD <- av_get(av_fun= "FX_DAILY" ,from_symbol='EUR',to_symbol='USD',outputsize="full")



#### MODELIZACIÓN ####


EUR_USD_serie <- ts(EUR_USD$close, frequency=365)

plot(EUR_USD$timestamp ,EUR_USD$close, type='l')

EUR_USD_decomp <- decompose(EUR_USD_serie)

plot(EUR_USD_decomp)

EUR_USD_noise <- EUR_USD_decomp$random

EUR_USD_noise <- tsclean(EUR_USD_noise)


plot(EUR_USD_noise)

Acf(EUR_USD_serie)
Pacf(EUR_USD_serie)


ndiffs(EUR_USD_serie)

p <- periodogram(EUR_USD_serie)

plot(p)
plot(1/p$freq)

data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:2]

Acf(EUR_USD_serie)
Pacf(EUR_USD_serie)


#f1 <- fourier(ts(EUR_USD_serie, frequency=365), K=1)
f2 <- fourier(ts(EUR_USD_serie, frequency=365), K=2)

auto_arima <- auto.arima(EUR_USD_serie, xreg=f2,seasonal=T,
                         max.p = 5,
                         max.q = 5,
                         max.P = 5,
                         max.Q = 5,
                         max.order = 5,
                         max.d = 5,
                         max.D = 5,
                         start.p = 5,
                         start.q = 5,
                         start.P = 5,
                         start.Q = 5,
                         approximation = F,
                         num.cores = NULL)

summary(auto_arima)

best_arima <- list(aicc=auto_arima$aicc, i=0, j=0, fit=auto_arima)



Acf(auto_arima$residuals)
Pacf(auto_arima$residuals)


f2_pred<-fourier(ts(EUR_USD_serie, frequency=365), K=2, h=7)

forecast <- forecast(auto_arima, 
               xreg=cbind(f2_pred ),level = c(90, 95, 99))

autoplot(forecast, 14)
#plot(forecast, 60)

accuracy(forecast )



#### Y si mejoramos el ARIMA? ####


#https://en.wikipedia.org/wiki/General_regression_neural_network


GRNN <- grnn_forecasting(EUR_USD_serie, h = 7, lags = 1:12)
GRNN$prediction

autoplot(GRNN)

real <- c(EUR_USD_serie[4994:5000], rep('',length(GRNN$prediction) ) )%>%as.numeric
prediction_grnn <- c(EUR_USD_serie[4994:5000], GRNN$prediction)
prediction_arima <- c(EUR_USD_serie[4994:5000], forecast$mean)
X <- seq(1,length(prediction_arima))


plot(X,prediction_arima, "l", col= "red")
lines(X,prediction_grnn, "l", col= "blue")
lines(real)
legend("topleft", legend=c("ARIMA", "GRNN", "Real"),
       col=c("red", "blue", "black"), lty=1, cex=0.8)




