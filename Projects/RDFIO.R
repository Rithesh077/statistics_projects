#Retail Demand Forecasting and Inventory Optimization
library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
#data export and conversion of invoicedate to date
salesData <- online_sales_dataset
salesData <-  salesData %>% 
  mutate(InvoiceDate = as.Date(InvoiceDate))
#weekly sales
weeklySales <- salesData %>% 
  mutate(week = floor_date(InvoiceDate,"week")) %>% 
  group_by(StockCode,week) %>% 
  summarise(unitsSold=sum(Quantity), .groups = "drop")

#forcasting selected product

chosenProduct <- "SKU_1964"

productSales <- weeklySales %>% 
  filter(StockCode == chosenProduct)

#time series of chosen product
timeSeriesObject <- ts(productSales$unitsSold,frequency = 52)#52 weeks since weekly sales is analyzed 

#arima model
arimaModel <- auto.arima(timeSeriesObject)

#forecast data
forecastResult <- forecast(arimaModel,h = 4)
 forecastDataFrame <- data.frame(
   week = seq(max(productSales$week) + 7,by = "1 week",length.out = 4),
   forecastedDemand = round(forecastResult$mean)
   
 )
#inventory Optimization
 currentStock <- 100
 safetyStock <- 1.25
 reorderQuantity <- max((sum(forecastDataFrame$forecastedDemand)*safetyStock)-currentStock,0) 

 #forecast plot
 autoplot(forecastResult) +
   ggtitle(paste("4 Week Demand Forecast for", chosenProduct)) +
   xlab("Weeks") + ylab("Units Sold")
 


