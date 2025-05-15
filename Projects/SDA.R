#Sales Data Analysis


#data import
salesDataset<-amazon
str(salesDataset)
View(salesDataset)
headSales<-head(salesDataset)
View(headSales)

library(tidyr)
library(dplyr)

#data exploration
head(salesDataset)
numeric_variables<-select(salesDataset,4,5,6,7,8)
View(numeric_variables)
headNumeric<-head(numeric_variables)
str(numeric_variables)
summary(numeric_variables)

#data cleaning and transformation

to_numeric<-function(numericVariable)
{
  numericVariable<-gsub("₹","",numericVariable)
  numericVariable<-gsub(",","",numericVariable)
  numericVariable<-gsub("%","",numericVariable)
  
  as.numeric(numericVariable)
}

cleanedData<-numeric_variables %>% 
  mutate(
    discounted_price = to_numeric(discounted_price),
    actual_price = to_numeric(actual_price),
    discount_percentage = to_numeric(discount_percentage),
    rating_count = to_numeric(rating_count)
  )
View(cleanedData)

final_price <- function(finalPrice){
  finalPrice = cleanedData$actual_price - cleanedData$discounted_price
}

dataForAnalysis <- mutate(cleanedData,final_price = final_price(finalPrice))

dataForAnalysis <- dataForAnalysis %>% 
  mutate(dataForAnalysis,rating = as.character(rating))

levels(dataForAnalysis$rating)
dataForAnalysis <- dataForAnalysis %>% 
  filter(!(rating %in% c("|")))
dataForAnalysis <- dataForAnalysis %>% 
  mutate(dataForAnalysis,rating= as.numeric(as.character(rating)))
View(dataForAnalysis)

#statistical analysis
summary(dataForAnalysis)

#correlation between discount price/percentage and ratings
cor(dataForAnalysis$discounted_price,dataForAnalysis$rating,use = "everything",method = "pearson")
cor(dataForAnalysis$discount_percentage,dataForAnalysis$rating,use = "everything",method = "pearson")
#correlation between final price and rating
cor(dataForAnalysis$final_price,dataForAnalysis$rating,use = "everything",method = "pearson")
cor(dataForAnalysis$final_price,dataForAnalysis$rating,use = "everything",method = "spearman")