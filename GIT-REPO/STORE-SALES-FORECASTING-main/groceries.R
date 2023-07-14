library(fpp3) 
library('ggplot2')
library('readxl')
install.packages('forecast')
library(forecast)
library(fabletools)

setwd('/Users/harita_addanki/Desktop/Classes/Predictive Anlaytics/Project')
newdata = read_excel('newdata.xlsx')

newdata = newdata[1:1092,2:3]

newdata = mutate(newdata,date=as.Date(date, format = '%Y-%m-%d'))

#You are given 5 years of store-item sales data, and asked to predict 3 months 
#of sales for 50 different items at 10 different stores.
tail(newdata)

sales_data <- ts(newdata[,2], start = 2013, frequency = 7)


sales_data <- as_tsibble(sales_data,index=date)
names(sales_data) <- c("Date", "Sales")

autoplot(sales_data,Sales)
sd(sales_data$Sales)


# 2015 to 2016 sales of beverages store 34

# STL DECOMPOSITION
mod_STL <- model(sales_data, STL(Sales ~ trend(window = 30) +
                                   season(window = "periodic"), 
                                 robust = TRUE))
comp_STL <- components(mod_STL)
autoplot(comp_STL)

err_STL <- comp_STL$remainder
MSE_STL <- mean(err_STL^2)
RMSE_STL <- MSE_STL^0.5
RMSE_STL #116.46
# Is this normal?
# looks like an additive model?


ACF(sales_data, lag_max = 30)
autoplot(ACF(sales_data, lag_max = 30))

PACF(sales_data, lag_max = 30)
autoplot(PACF(sales_data, lag_max = 30))

# what should be the lag_max value?

#HWA


fit_WCsalts_HWa <- model(sales_data, 
                         ETS(Sales ~ error("A") 
                             + trend("A") 
                             + season("A")))

accuracy(fit_WCsalts_HWa) # rmse = 94.5
# RMSE IS ALARMING, CAN WE USE MAPE? FOR COMPARISON?
report(fit_WCsalts_HWa) # aic = 17597
# AIC 


aug_HWa <- augment(fit_WCsalts_HWa)
aug_HWa
# residuals for fitted data


#  Plotting the model components
autoplot(aug_HWa, Sales) +
  autolayer(aug_HWa,.fitted, colour = "Red") +
  autolayer(aug_HWa,.resid, colour = "Green") +
  labs(y = "USD ($1000s)", title = "Water Craft Sales",
       x = "Year/Quarter") 


# forecast for 30 days
forc_WCsalts_HWa <- forecast(fit_WCsalts_HWa, h = 30)
forc_WCsalts_HWa
str(forc_WCsalts_HWa)

# some confusion with dist values in the forecast return values


autoplot(forc_WCsalts_HWa, sales_data, 
         level = NULL, colour = "Blue")  



auto_ets <- model(sales_data, 
                  ETS(Sales))
accuracy(auto_ets) # rmse = 94.3.
report(auto_ets)  # AIC = 17589.40

# arima

fit_4 <- model(sales_data,
               auto = ARIMA(Sales, stepwise = FALSE, approx = FALSE))
glance(fit_4)
report(fit_4) # aic = 13064
accuracy(fit_4) # rmse = 95.3
gg_tsresiduals(fit_4)
aug_4 <- augment(fit_4)
features(aug_4, .resid, ljung_box, lag = 1, dof = 0) #data is not stationary

forc_4 <- forecast(fit_4, h = 30)
forc_4_a <- filter(forc_4, .model == "auto")
autoplot(forc_4_a, sales_data)





lambda <- features(sales_data, Sales, features = guerrero)



# box cox
fit_4l <- model(sales_data,
                auto = ARIMA(box_cox(Sales,lambda), stepwise = FALSE, approx = FALSE))
glance(fit_4l)
report(fit_4l) #aic = -NEGATIVE
accuracy(fit_4l) # rmse = 90.1
gg_tsresiduals(fit_4l)
aug_4l <- augment(fit_4l)
features(aug_4l, .resid, ljung_box, lag = 1, dof = 0)

forc_4l <- forecast(fit_4l, h = 30)
autoplot(forc_4l, sales_datal)




