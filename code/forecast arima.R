install.packages('fpp2')
library(fpp2)
install.packages('forecast')
library(forecast)
install.packages('tseries') 
require(tseries)
library(astsa)
require(xts)
require(forecast)
tdata <- rbind(tdata1,tdata2,tdata3,tdata4)
#transforming to time series object
td<- ts(tdata$v0_num_traj)
#plotting time series plot
plot(td)
#stationarity check
adf.test(td)
#plottting acf
acf(td)
#plotting pacf
pacf(td)
#arima model traing the data
fittt = Arima(tdata$v0_num_traj[1:400],order = c(3,0,5))
plot(fittt)
#testing data and accuracy prediction using observations
test_data <- ts(tdata$v0_num_traj[401:500]

#refitting the model 
refit <- Arima(c(11,53,141,173),model = fittt)
#fore casting
ts_forecast = forecast(refit, h=4)
#fore casting values
ts_forecast$mean
#plot
autoplot(ts_forecast)
Box.test(refit$residuals)
#accuracy of the model
accuracy(ts_forecast)
