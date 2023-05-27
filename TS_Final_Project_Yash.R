# Important Libraries 

library(fpp)
library(ggplot2)
library(tseries)
library(forecast)
library(MASS)
library(TSA)
library(dplyr)
library(gdata)
library(readxl)
library(lubridate)
library(xts)
library(stringr)
library(vars)


setwd("C:\\Users\\verma\\OneDrive - The University of Chicago\\Desktop\\MSCA\\QUARTER 3 SPRING\\TIME SERIES\\FINAL PROJECT")

house_price<-read_excel("US House Price for Final Project.xlsx")
house_price$Date <- as.Date(house_price$Date, format = "%m/%d/%y")
gdp<-read_excel("GDP Data Point.xlsx")
interest<-read_excel("Interest Rate.xlsx")

house_price
summary(house_price)

# plotting 

plot(house_price$Date, house_price$`US Country wide House Price`)

ggplot(data = house_price, aes(x = Date, y = `US Country wide House Price`)) +
  geom_line() +
  labs(x = "Date", y = "US Country wide House Price", title = "US Country wide House Price over Time")

ggplot(data = house_price, aes(x = Date, y = `US Country wide House Price`, fill = `US Country wide House Price`)) +
  geom_col() +
  labs(x = "Date", y = "US Country wide House Price", title = "US Country wide House Price over Time")

ts_price<-ts(house_price$`US Country wide House Price`,start=c(2000,1),frequency=12)
ts_gdp<-ts(gdp$GDP,start=c(2000,1),frequency=12)
ts_interest<-ts(interest$Total,start=c(2000,1),frequency=12)

#Split Training and Testing

price_training=window(ts_price,start=c(2000,1),end=c(2022,3))
price_testing=window(ts_price,start=c(2022,4),end=c(2023,3))

interest_training=window(ts_interest,start=c(2000,1),end=c(2022,3))
interest_testing=window(ts_interest,start=c(2022,4),end=c(2023,3))

gdp_training=window(ts_gdp,start=c(2000,1),end=c(2022,3))
gdp_testing=window(ts_gdp,start=c(2022,4),end=c(2023,3))

#Determince the order of differencing

diff_order <- ndiffs(price_training)
diff_order

# Plot ACF and PACF of the differenced data
eacf(price_training)

## ARIMA 

#6,2,2
#6,2,3
#4,2,4

# Fit the ARMA model

model_3 <- Arima(price_training, order = c(6, 2, 2))

model_4 <- Arima(price_training, order = c(6, 2, 3))

model_5 <- Arima(price_training, order = c(4, 2, 4))

model_6 <- auto.arima(price_training, seasonal = 'FALSE')

# Print the model orders and coefficients

summary(model_3)
checkresiduals(model_3)

#Model 4 
summary(model_4)
checkresiduals(model_4)

#Model 5

summary(model_5)
checkresiduals(model_5)

#Model 6

summary(model_6)
checkresiduals(model_6)

## ARIMA with regression

BoxCox.lambda(ts_gdp)

fit <- auto.arima(price_training, xreg = BoxCox(gdp_training, lambda = -0.9999242), lambda = "auto", seasonal = FALSE)
summary(fit)

checkresiduals(fit)

fcst <- forecast(fit, xreg = BoxCox(gdp_testing, lambda = -0.9999242), h = 12)

plot(fcst)

plot(ts_interest, ylim = c(0, 400000))
lines(ts_price)

fit2 <- auto.arima(price_training, xreg = interest_training, lambda = "auto", seasonal = FALSE)
summary(fit2)

checkresiduals(fit2)

print(forecast(fit2, xreg = interest_testing))

plot(forecast(fit2, xreg = interest_testing))
lines(ts_price)

plot(fcst)
lines(ts_price)

print(fcst)

# Fit ARIMA model with GDP as predictor
arima_model <- arima(price_training, order = c(4,2,4), xreg = gdp_training)

# Make predictions on the testing period
forecast_result <- forecast(arima_model, xreg = gdp_testing)

# Print the forecast result
print(forecast_result)


## EXPONENTIAL SMOOTHING 

# Fit ETS model on the training set
ets_model <- ets(price_training)

summary(ets_model)

checkresiduals(ets_model)

# Make predictions on the test set
forecast_result <- forecast(ets_model, h = length(price_testing))

# Compare the forecast result with the actual test data
comparison <- data.frame(Actual = price_testing, Forecast = forecast_result$mean)
print(comparison)

# Plot the forecast result and the actual test data
autoplot(cbind(Actual = price_testing, Forecast = forecast_result$mean)) +
  labs(title = "Comparison of ETS Model Forecast vs Actual Test Data",
       x = "Year",
       y = "Home Price") +
  theme_minimal()

# Compute forecast accuracy measures
accuracy(forecast_result, price_testing)

# Check the result
print(forecast_result)

#Holt-Winters Method for this data set(multiplicative/additive)
m_add <- hw(ts_price, seasonal="additive", h=20)
plot(m_add)

m_mult <- hw(ts_price, seasonal="multiplicative", h=20)
plot(m_mult)

fit_add <- hw(ts_price, seasonal="additive", h=16)
fit_mult <- hw(ts_price, seasonal="multiplicative", h=16)
fit_add_damp <- hw(ts_price, seasonal="additive", h=16, damped = TRUE)
plot(fit_add_damp)
fit_mult_damp <- hw(ts_price, seasonal="multiplicative", h=16, damped = TRUE)
fit_snaive <- snaive(ts_price, h=16)

accuracy_fit_add <- accuracy(fit_add)
accuracy_fit_mult <- accuracy(fit_mult)
accuracy_fit_add_damp <- accuracy(fit_add_damp)
accuracy_fit_mult_damp <- accuracy(fit_mult_damp)
accuracy_fit_snaive <- accuracy(fit_snaive)

summary(fit_add)
summary(fit_mult)
summary(fit_add_damp)
summary(fit_mult_damp)
summary(fit_snaive)

checkresiduals_model <- checkresiduals(fit_add_damp)
