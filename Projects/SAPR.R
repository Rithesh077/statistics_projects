#Sentiment Analysis of Product Reviews

reviews <- Reviews
View(Reviews)
library(dplyr)
library(stringr)
install.packages("tidytext")
library(tidytext)

texts <- Reviews %>% 
  mutate(Text = str_replace(Text,"[[:punct:]]", ""),
         Text = str_to_lower(Text))
View(texts)

data(stop_words)
tokenReviews <- texts %>% 
  unnest_tokens(word,Text) %>% 
  anti_join(stop_words)
View(tokenReviews)

library(textdata)
sentiments <- get_sentiments("afinn")
sentimentScores <- tokenReviews %>% 
  inner_join(sentiments, by = "word") %>% 
  group_by(UserId) %>% 
  summarise(sentimentScores = sum(value))

finalReviews <- Reviews %>% 
  left_join(sentimentScores,by = "UserId") %>% 
  mutate(sentimentLabel = case_when(
    sentimentScores > 0 ~ "positive",
    sentimentScores < 0 ~ "negative",
    is.na(sentimentScores) | sentimentScores == 0 ~ "neutral"))
View(finalReviews)
