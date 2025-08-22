# ---- Retail Demand Forecasting & Inventory Optimization ----
library(forecast)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(readr)

# paths
data_path <- "data/online_sales.csv"
out_fig <- "outputs/figures/retail_forecast.png"
out_tbl <- "outputs/tables/retail_forecast.csv"

# data
sales_data <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(InvoiceDate = as.Date(InvoiceDate))

weekly_sales <- sales_data %>%
  mutate(week = floor_date(InvoiceDate, "week")) %>%
  group_by(StockCode, week) %>%
  summarise(unitsSold = sum(Quantity), .groups = "drop")

# forecast selected product
chosen_product <- "SKU_1964"
product_sales <- weekly_sales %>%
  filter(StockCode == chosen_product) %>%
  arrange(week) # nolint

ts_obj <- ts(product_sales$unitsSold, frequency = 52)

fit <- auto.arima(ts_obj)
fc <- forecast(fit, h = 4)

# inventory optimization
current_stock <- 100
full_fc <- as.data.frame(fc)
target_stock <- round(sum(full_fc$`Hi 95`))
reorder_qty <- max(0, target_stock - current_stock)
cat(paste("Reorder Quantity for", chosen_product, ":", reorder_qty, "\n"))

# save forecast table
forecast_df <- data.frame(
  week = seq(max(product_sales$week) + 7, by = "1 week", length.out = 4),
  forecasted_demand = round(fc$mean)
)
write_csv(forecast_df, out_tbl)

# save plot
p <- autoplot(fc) +
  ggtitle(paste("4 Week Demand Forecast for", chosen_product)) +
  xlab("Weeks") + ylab("Units Sold")
ggsave(out_fig, p, width = 8, height = 5)
cat(paste("Forecast plot saved to", out_fig, "\n"))
