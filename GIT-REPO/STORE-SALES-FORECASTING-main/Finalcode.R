# PREDICTIVE ANALYTICS FINAL PROJECT CODE
# TEAM 4

# required packages
library(fpp3) 
library('ggplot2')
library('readxl')
install.packages('forecast')
library(forecast)
library(fabletools)

setwd('/Users/harita_addanki/Desktop/Classes/Predictive Anlaytics/Project')

# train.csv file contains the sales data from 2013 to 2017 of 53 stores across
# Ecuador.
data = read.csv('train.csv')
# Taking a subset of the sales data from the CLEANING family products.
data = data[data['family']=='CLEANING',]
# Further filtering the data by considering the sales of only the store with
# number 23 located in Ambato.
data = data[data['store_nbr']==23,]

# creating a copy of the filtered data which is used for creation of time series
newdata = data.frame(data)

# considering only the date and sales variables from the data
newdata = newdata[,c(2,5)]
# resetting the index
rownames(newdata) = c(1:1684)

# To avoid noise, considering 3 years of data from 2013 to 2015.
newdata = newdata[1:1092,]

head(newdata)
# converting the date variable in the data to data type Date.
newdata = mutate(newdata,date=as.Date(date, format = '%Y-%m-%d'))

# replacing the zero values in the response variable by mean sales of that week
newdata[newdata['sales']==0,]
newdata[1,2] = round(mean(newdata[c(1:7),2]),2)
newdata[365,2] = round(mean(newdata[c(365:373),2]),2)
newdata[729,2] = round(mean(newdata[c(729:735),2]),2)
# checking the rows with zero values and the result is none
newdata[newdata['sales']==0,]

# creating a timeseries object with a frequency of 7, since the periodicity of
# the data is 7 (weekly data recorded)
sales_data = ts(newdata[,2], start = 2013, frequency = 7)

# creating a tsibble object with an index as date variable
sales_data = as_tsibble(sales_data,index=date)

# naming the columns in the tsibble object
names(sales_data) = c("Date", "Sales")

# plotting the time series
autoplot(sales_data,Sales)

# standard deviation of the timeseries - 102.21
sd(sales_data$Sales)

# DECOMPOSING THE DATA TO ANALYZE THE COMPONENTS

# STL DECOMPOSITION
mod_STL <- model(sales_data, STL(Sales ~ trend(window = 30) +
                                   season(window = "periodic"), 
                                 robust = TRUE))
comp_STL <- components(mod_STL)
autoplot(comp_STL)

# the plot shows that the time series model is additive with no increasing
# trend, although there is some seasonality in year that is observed. 
# Followed by that, we can observe a strong weekly seasonality. 

# Now calculating the error from the remainder component
err_STL <- comp_STL$remainder
MSE_STL <- mean(err_STL^2)
RMSE_STL <- MSE_STL^0.5
RMSE_STL 
# The RMSE value observed from the decomposition is 110.76

# Plotting the auto correlation and partial auto correlation function with
# a lag of 20

ACF(sales_data, lag_max = 20)
autoplot(ACF(sales_data, lag_max = 20))
# From the plot we can observe that there is a strong correlation at lag 7
# meaning, the data at day 1 is stronly correlated to data at day 7. This
# confirms that there is a weekly seasonality in the data and the sales are 
# mostly correlated for cleaning products on same days of different weeks.


PACF(sales_data, lag_max = 20)
autoplot(PACF(sales_data, lag_max = 20))
# Similar results could be observed, even if partial auto correlation is 
# considered. High correlation is observed at lag 7.


# Since our data has strong seasonality, and the magnitude of the seasonal 
# component is stable over time, we are considering Holt Winters Additive
# model.
fit_WCsalts_HWa <- model(sales_data, 
                         ETS(Sales ~ error("A") 
                             + trend("A") 
                             + season("A")))

accuracy(fit_WCsalts_HWa)
# The RMSE of the model is given by 86.3
report(fit_WCsalts_HWa) 
# AIC of the model is given by 17399.20


# Deriving the residuals for fitted data using HWA model
aug_HWa <- augment(fit_WCsalts_HWa)
aug_HWa


#  Plotting the model components
autoplot(aug_HWa, Sales) +
  autolayer(aug_HWa,.fitted, colour = "Red") +
  autolayer(aug_HWa,.resid, colour = "Green") +
  labs(y = "Sales", title = "Favorita Store Sales",
       x = "Date") 


# forecast for 30 days
forc_WCsalts_HWa <- forecast(fit_WCsalts_HWa, h = 30)
print(forc_WCsalts_HWa,n=30)

# Plotting the forecast
autoplot(forc_WCsalts_HWa, sales_data, 
         level = NULL, colour = "Blue")  


# Moving onto ARIMA models, in order to use them, we are first 
# identifying if the series is stationary - if not computing difference series
# performing Ljung-Box test which examines whether there is significant evidence
# for non-zero correlations at lags 1-20. 
#  Ho:  Data are uncorrelated in time
#  Ha:  Data are correlated in time
# Small p-values(<0.05) suggest that the series is stationary.
# giving lag = 7 because of the weekly seasonality
features(sales_data, Sales, ljung_box, lag =7, dof = 0)
# p-value is 0<0.05) and hence we can reject the null hypothesis and
# conclude that the data is stationary.

# Since the data is already stationary, we are using the auto-ARIMA model
# without transforming the data.
fit_4 <- model(sales_data,
               auto = ARIMA(Sales, stepwise = FALSE, approx = FALSE))
report(fit_4) 
# AIC of the model is given by 12879.44
accuracy(fit_4) 
# RMSE of the model is given by 87.5

# deriving the residuals for fitted data using Auto Arima model
aug_4 <- augment(fit_4)
aug_4

# plotting the model
autoplot(aug_4, Sales) +
  autolayer(aug_4,.fitted, colour = "Red") +
  autolayer(aug_4,.resid, colour = "Green") +
  labs(y = "Sales", title = "Favorita Store Sales",
       x = "Date") 

# Plotting the residual distribution, residual acf plot and innovation residuals
gg_tsresiduals(fit_4)

# Now forecasting for 30 days using the fitted model
forc_4 <- forecast(fit_4, h = 30)
print(forc_4,n=30)

# Plotting the forecast
autoplot(forc_4, sales_data)

# checking if the residuals are distinguishable from white noise through
# Ljung-Box test,
features(aug_4, .resid, ljung_box, lag = 7, dof = 0)
# p-value(0.6>0.05), means we cannot reject the null hypothesis and the 
# residuals are not distinguishable from white noise.

# Prophet Model
install.packages('prophet')
library(prophet)

prophet_data = data.frame(sales_data)
# naming the variables
names(prophet_data) <- c('ds', 'y')

# fitting the model to the data
pmodel = prophet(prophet_data,daily.seasonality = TRUE)

# forecasting for 30 days. 
future <- make_future_dataframe(pmodel, periods=30)
forecasts = predict(pmodel,future)
preds = forecasts[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]

# RMSE for the model is 1.56
se = (preds$yhat-prophet_data$y)^2
mse = mean(se)
rmse = mse^0.05

# MAPE for the model is 0.12
MAPE = mean(abs((preds$yhat[1:1092]-prophet_data$y)/prophet_data$y))

# plotting the forecast
plot(pmodel,forecasts)

# plotting the model components
prophet_plot_components(pmodel,forecasts)






