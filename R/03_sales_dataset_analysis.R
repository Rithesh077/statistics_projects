# Sales Data Analysis
library(tidyr)
library(dplyr)

# load dataset (corrected path)
sales_dataset <- read.csv("../data/online_sales.csv")
str(sales_dataset)
head(sales_dataset)

# select numeric variables
numeric_variables <- select(sales_dataset, 4, 5, 6, 7, 8)
summary(numeric_variables)

# cleaning function
to_numeric <- function(numeric_variable) {
  numeric_variable <- gsub("₹", "", numeric_variable)
  numeric_variable <- gsub(",", "", numeric_variable)
  numeric_variable <- gsub("%", "", numeric_variable)
  as.numeric(numeric_variable)
}

# clean dataset
cleaned_data <- numeric_variables %>%
  mutate(
    discounted_price = to_numeric(discounted_price),
    actual_price = to_numeric(actual_price),
    discount_percentage = to_numeric(discount_percentage),
    rating_count = to_numeric(rating_count)
  )

# compute final price
cleaned_data <- cleaned_data %>%
  mutate(
    final_price = actual_price - discounted_price,
    rating = as.numeric(as.character(rating))
  ) %>%
  filter(!is.na(rating))

# summary
summary(cleaned_data)

# correlations
cor(cleaned_data$discounted_price, cleaned_data$rating, use = "complete.obs", method = "pearson") # nolint
cor(cleaned_data$discount_percentage, cleaned_data$rating, use = "complete.obs", method = "pearson") # nolint
cor(cleaned_data$final_price, cleaned_data$rating, use = "complete.obs", method = "pearson") # nolint
cor(cleaned_data$final_price, cleaned_data$rating, use = "complete.obs", method = "spearman") # nolint
cor(cleaned_data$rating_count, cleaned_data$rating, use = "complete.obs", method = "pearson") # nolint
