# required packages
# for mutate function used for data frame modifications
library(fpp3) 
library('ggplot2')
# for reading an excel file
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

# creating a timeseries object with a frequency of 7, since the periodicity of
# the data is 7 (weekly data recorded)
sales_data = ts(newdata[,2], start = 2013, frequency = 7)

# creating a tsibble object with an index as date variable
sales_data = as_tsibble(sales_data,index=date)

# naming the columns in the tsibble object
names(sales_data) = c("Date", "Sales")

# plotting the time series
autoplot(sales_data,Sales)

# standard deviation of the timeseries - 106.45
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
# The RMSE value observed from the decomposition is 116.47

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
# The RMSE of the model is given by 94.5.
report(fit_WCsalts_HWa) 
# AIC of the model is given by 17597.41


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



# Since our data has strong seasonality, and the magnitude of the seasonal 
# component is stable over time, we are considering Holt Winters Additive
# model.
fit_WCsalts_HWa <- model(s, 
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


# Moving onto ARIMA models and using auto-ARIMA initially without transforming
# the data.
fit_4 <- model(sales_data,
               auto = ARIMA(Sales, stepwise = FALSE, approx = FALSE))
report(fit_4) 
# AIC of the model is given by 13064.04
accuracy(fit_4) 
# RMSE of the model is given by 95.3

# deriving the residuals for fitted data using Auto Arima model
aug_4 <- augment(fit_4)

# Plotting the residual distribution, residual acf plot and innovation residuals
gg_tsresiduals(fit_4)

# Now forecasting for 30 days using the fitted model
forc_4 <- forecast(fit_4, h = 30)
print(forc_4,n=30)

# Plotting the forecast
autoplot(forc_4, sales_data)



fit_4 <- model(s,
               auto = ARIMA(Sales, stepwise = FALSE, approx = FALSE))
report(fit_4) 
# AIC of the model is given by 12879.44
accuracy(fit_4) 
# RMSE of the model is given by 87.5

# deriving the residuals for fitted data using Auto Arima model
aug_4 <- augment(fit_4)

# Plotting the residual distribution, residual acf plot and innovation residuals
gg_tsresiduals(fit_4)

# Now forecasting for 30 days using the fitted model
forc_4 <- forecast(fit_4, h = 30)
print(forc_4,n=30)

# Plotting the forecast
autoplot(forc_4, sales_data)

# Identifying if the series is stationary - if not computing difference series
# performing Ljung-Box test which examines whether there is significant evidence
# for non-zero correlations at lags 1-20. 
#  Ho:  Data are uncorrelated in time
#  Ha:  Data are correlated in time
# Small p-values(<0.05) suggest that the series is stationary.

features(aug_4, .resid, ljung_box, lag = 1, dof = 0) 

# p-value is 0.9(>0.05) and hence we cannot reject the null hypothesis and
# conclude that the data is not stationary.

# so performing the box-cox transformation to stabilize the variance of the 
# series.

# computing the value of the lambda for box-cox transformation. 
# If lambda is less than 0, then log transformation is applied to the data.
lambda <- features(s, Sales, features = guerrero)
lambda
# lambda is -0.90.  -0.104

s = data.frame(sales_data)

# creating a timeseries object with a frequency of 7, since the periodicity of
# the data is 7 (weekly data recorded)
s = ts(s[,2], start = 2013, frequency = 7)

# creating a tsibble object with an index as date variable
s = as_tsibble(s,index=Date)

# naming the columns in the tsibble object
names(s) = c("Date", "Sales")

s[s['Sales']==0,]
s[1,2] = round(mean(s[c(1:7),2]),2)
s[365,2] = round(mean(s[c(365:373),2]),2)
s[729,2] = round(mean(s[c(729:735),2]),2)
s[s['Sales']==0,]
fit_4l <- model(s,auto = ARIMA(box_cox(Sales,lambda), stepwise = FALSE, 
                             approx = FALSE))
report(fit_4l) 
# The AIC of the model is given by -
accuracy(fit_4l) 
# The RMSE of the model is given by 

# deriving the residuals for fitted data using Box Cox transformed Arima model
aug_4l <- augment(fit_4l)
aug_4l

# Plotting the residual distribution, residual acf plot and innovation residuals
gg_tsresiduals(fit_4l)

# performing the Ljung-Box test again to check the stationarity of the data
features(aug_4l, .resid, ljung_box, lag = 1, dof = 0)
# The p-value is 0.02(<0.05) and hence null hypothesis can be rejected
# and we can say that the data being used for the model is stationary

# Forecasting data for 30 days 
forc_4l <- forecast(fit_4l, h = 30)
print(forc_4l,n=30)
autoplot(forc_4l, sales_data)


# so performing the box-cox transformation to stabilize the variance of the 
# series.

# computing the value of the lambda for box-cox transformation. 
# If lambda is less than 0, then log transformation is applied to the data.
lambda <- features(sales_data, Sales, features = guerrero)
lambda
# lambda is -0.104

# fitting an auto ARIMA model to the box transformed data
fit_4l <- model(sales_data,auto = ARIMA(box_cox(Sales,lambda), stepwise = FALSE, 
                                        approx = FALSE))
report(fit_4l) 
# The AIC of the model is given by -2432.1
accuracy(fit_4l) 
# The RMSE of the model is given by 88.5

# deriving the residuals for fitted data using Box Cox transformed Arima model
aug_4l <- augment(fit_4l)
aug_4l

# Plotting the residual distribution, residual acf plot and innovation residuals
gg_tsresiduals(fit_4l)

# performing the Ljung-Box test again to check the stationarity of the data
features(aug_4l, .resid, ljung_box, lag = 1, dof = 0)


# The p-value is 0.02(<0.05) and hence null hypothesis can be rejected
# and we can say that the data being used for the model is stationary

# Forecasting data for 30 days 
forc_4l <- forecast(fit_4l, h = 30)
print(forc_4l,n=30)
autoplot(forc_4l, sales_data)



