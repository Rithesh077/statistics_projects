library(forecast)
library(ggplot2)

dataSet$Date <- as.Date(dataSet$Date, format = "%d-%m-%Y")

sales_ts <- ts(
  dataSet$Weekly_Sales,
  start = c(2010, 5),  
  frequency = 52
)

product_ts <- dataSet %>%
  filter(dataSet$Product == "Blueberry scented candle") %>%
  ts(., frequency = 52)

ndiffs(sales_ts)

best_model <- auto.arima(
  sales_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(best_model)

train <- window(sales_ts, end = c(2012, 26))
test <- window(sales_ts, start = c(2012, 27))

fit <- auto.arima(train)
fcst <- forecast(fit, h = length(test))

accuracy(fcst, test)[, c("RMSE", "MAE")]

autoplot(fcst) +
  autolayer(test, series = "Actual") +
  ggtitle("12-Week Sales Forecast vs Actuals")