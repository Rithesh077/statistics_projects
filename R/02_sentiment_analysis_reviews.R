# Sentiment Analysis of Product Reviews
library(dplyr)
library(stringr)
library(tidytext)
library(textdata)

# load reviews dataset (assuming in data folder)
reviews <- read.csv("../data/online_sales.csv")

# clean text
texts <- reviews %>%
  mutate(
    Text = str_replace_all(Text, "[[:punct:]]", ""),
    Text = str_to_lower(Text)
  )

# tokenize
data(stop_words)
token_reviews <- texts %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words)

# sentiment scoring
sentiments <- get_sentiments("afinn")
sentiment_scores <- token_reviews %>%
  inner_join(sentiments, by = "word") %>%
  group_by(UserId) %>%
  summarise(sentiment_scores = sum(value))

# label sentiments
final_reviews <- reviews %>%
  left_join(sentiment_scores, by = "UserId") %>%
  mutate(sentiment_label = case_when(
    sentiment_scores > 0 ~ "positive",
    sentiment_scores < 0 ~ "negative",
    is.na(sentiment_scores) | sentiment_scores == 0 ~ "neutral"
  ))

View(final_reviews)
