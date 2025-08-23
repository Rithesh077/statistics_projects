# Retail Demand Forecasting and Inventory Optimization
library(forecast)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)

# data import (corrected path)
sales_data <- read.csv(here::here("data/online_sales.csv"))

# convert InvoiceDate to Date
sales_data <- sales_data %>%
  mutate(InvoiceDate = as.Date(InvoiceDate))

# weekly sales aggregation
weekly_sales <- sales_data %>%
  mutate(week = floor_date(InvoiceDate, "week")) %>%
  group_by(StockCode, week) %>%
  summarise(unitsSold = sum(Quantity), .groups = "drop")

# forecasting selected product
chosen_product <- "SKU_1964"
product_sales <- weekly_sales %>%
  filter(StockCode == chosen_product)

# create time series object
time_series_object <- ts(product_sales$unitsSold, frequency = 52)

# ARIMA model
arima_model <- auto.arima(time_series_object)
print(arima_model)

# forecast for 4 weeks
forecast_result <- forecast(arima_model, h = 4)
print(forecast_result)

forecast_data_frame <- data.frame(
  week = seq(max(product_sales$week) + 7, by = "1 week", length.out = 4),
  forecasted_demand = round(forecast_result$mean)
)

# inventory optimization
current_stock <- 100
full_forecast_df <- as.data.frame(forecast_result)
print(full_forecast_df)

target_stock <- round(sum(full_forecast_df$`Hi 95`))
reorder_quantity <- max(0, target_stock - current_stock)
cat(paste("Reorder Quantity for", chosen_product, ":", reorder_quantity, "\n"))

# forecast plot
autoplot(forecast_result) +
  ggtitle(paste("4 Week Demand Forecast for", chosen_product)) +
  xlab("Weeks") + ylab("Units Sold")
ggsave(here::here("outputs/figures/retail_forecast.png"), width = 8, height = 5)
