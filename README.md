# R Projects – Sales & Sentiment Analysis

## Project Overview
This repository contains two R-based data science projects:
- **Sales Data Analysis**: Analyzes customer behavior using e-commerce data.
- **Sentiment Analysis of Product Reviews**: Classifies Amazon reviews into positive, negative, and neutral sentiments using AFINN lexicon.

---

## Tools & Libraries Used
- R
- dplyr
- tidyr
- tidytext
- ggplot2
- stringr

---

## Project Details

### 1. Sales Data Analysis
- Cleaned and transformed sales data using `dplyr` and `tidyr`.
- Performed descriptive and correlation analysis (Pearson, Spearman).
- Found relationships between discounts and customer ratings.

### 2. Sentiment Analysis
- Preprocessed and tokenized text data using `tidytext`.
- Assigned sentiment scores using AFINN lexicon.
- Labeled reviews as Positive, Negative, or Neutral.
- Derived insights on customer satisfaction.


# Retail Demand Forecasting and Inventory Optimization

This project aims to streamline inventory planning for retail businesses by forecasting product demand using time series analysis and computing optimal reorder quantities based on forecasted sales.

## Project Objective

To build a data-driven solution that:
- Forecasts weekly product demand using ARIMA models.
- Calculates reorder quantities to prevent stockouts or overstocking.
- Visualizes demand trends for better inventory decision-making.

## Dataset Used
The dataset includes over 49,000 sales transactions from a simulated online retail platform. Key fields:
- `InvoiceDate`: Timestamp of each order.
- `StockCode`: Unique product identifier.
- `Quantity`: Units sold (demand variable).
- `ReturnStatus`: Indicates if an item was returned.
- `UnitPrice`, `Category`, `ShippingCost`, and more.

## Libraries Used
- **R**: Core programming environment
- **tidyverse** (`dplyr`, `lubridate`, `ggplot2`)
- **forecast**: For ARIMA modeling and time series forecasting

## Use cases
- Inventory control for e-commerce platforms.
- Demand-driven purchasing decisions.
- Stock planning for warehouse management.

## Author
Rithesh K R
B.Sc. in Computer Science and Statistics  
First-year undergraduate interested in real-world applications of data science.
