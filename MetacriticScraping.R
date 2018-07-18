library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(wordcloud)
library(ggplot2)

#------------------------------------------------------------DATA COLLECTION AND CLEANING------------------------------------------------------------------------

url <- 'http://www.metacritic.com/game/playstation-4/star-wars-battlefront-ii/user-reviews'

pages <- 22
url_list = c()

#URL Creation
for (page_num in 0:pages) {
  x <- str_c(url, '?page=', page_num)
  url_list <- c(url_list, x)
}

revList = c()

#Pulling Data From Each Page of Reviews
for (page in url_list) {
  url <- page
  webpage <- read_html(url)
  rev <- html_nodes(webpage, css = ".review_body")
  revList <- c(revList, html_text(rev))
}

#Puts Text Into Tidy Format
textDF <- data_frame(line = 1:2387, text = revList)
textDF <- textDF %>%
  unnest_tokens(word, text)


#Removes Stop Words
data("stop_words")
textDF <- textDF %>%
  anti_join(stop_words)

extraWords <- bind_rows(data_frame(word = c("game", 'play', 'star', 'wars', 'bf1', 'bf2', 'players', "battlefront", "playing", "ea", 'kill', 'death','assault', 'damage', 'like', 'gift', 'fun', 'nice', 'amazing', 'awesome', '4', 'xbox', 'loves', 'battle', 'son', '3', '10', 'reviews', 'pretty', 'grandson', '1'), 
                                   lexicon = c("custom")), 
                        stop_words)
textDF <- textDF %>%
  anti_join(extraWords)

#----------------------------------------------------------------WORD FREQUENCY------------------------------------------------------------------------

textDF %>%
  count(word, sort = TRUE)

#Creates Word Cloud
textDF %>%
  count(word) %>%
  with(wordcloud(word,n, max.words = 50))

#----------------------------------------------------------------SENTIMENT ANALYSIS------------------------------------------------------------------------
#Positive vs. Negative
textBing <- textDF %>%
  ungroup() %>%
  inner_join(get_sentiments('bing'))

textBing %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

#10 Sentiment Categories
textSentiment <- textDF %>% 
  ungroup() %>%
  inner_join(get_sentiments('nrc'))

textSentiment %>%
  count(word, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()

#----------------------------------------------------------------TOPIC MODELING------------------------------------------------------------------------
library(topicmodels)
library(tidyverse)
#Groups Reviews Individually
revDF <- as_data_frame(revList)
revDF$Source <- 'Metacritic'
revDF$RevNum <- 1:2388

byWord <- revDF %>%
  unnest_tokens(word, value)

wordCount <- byWord %>%
  anti_join(stop_words) %>%
  anti_join(extraWords) %>%
  count(RevNum, word, sort = TRUE) %>%
  ungroup()

revDTM <- wordCount %>%
  cast_dtm(RevNum, word, n)

revLDA <- LDA(revDTM, k = 10, control = list(seed = 1234))

revTopics <- tidy(revLDA, matrix = 'beta')

topTerms <- revTopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topTerms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
