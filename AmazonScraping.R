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

url <- 'https://www.amazon.com/Star-Wars-Battlefront-II-Xbox-One/product-reviews/B071Y1RXHG/ref=cm_cr_arp_d_paging_btm_next_2?ie=UTF8&reviewerType=all_reviews'

pages <- 21
url_list = c()

#URL Creation
for (page_num in 1:pages) {
  x <- str_c(url, '&pageNumber=', page_num)
  url_list <- c(url_list, x)
}

revList = c()

#Pulling Data From Each Page of Reviews
for (page in url_list) {
  url <- page
  webpage <- read_html(url)
  rev <- html_nodes(webpage, css = ".review-text")
  revList <- c(revList, html_text(rev))
}

#Puts Text Into Tidy Format
textDF <- data_frame(line = 1:206, text = revList)
textDF <- textDF %>%
  unnest_tokens(word, text)

#Removes Stop Words
data("stop_words")
textDF <- textDF %>%
  anti_join(stop_words)

extraWords <- bind_rows(data_frame(word = c("game", 'play', 'star', 'wars', 'bf1', 'bf2', 'players', "battlefront", "playing", "ea", 'kill', 'death','assault', 'damage', 'like'), 
                                          lexicon = c("custom")), 
                               stop_words)
textDF <- textDF %>%
  anti_join(extraWords)

#----------------------------------------------------------------WORD FREQUENCY------------------------------------------------------------------------

textDF %>%
  count(words, sort = TRUE)

#Creates Word Cloud
textDF %>%
  anti_join(stop_words) %>%
  count(words) %>%
  with(wordcloud(words,n, max.words = 50))

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
#Groups Reviews Individually
revDF <- as_data_frame(revList)
revDF$Source <- 'Amazon'
revDF$RevNum <- 1:206

#Breaks Each Review Into Individual Words
byWord <- revDF %>%
  unnest_tokens(word, value)

wordCount <- byWord %>%
  anti_join(stop_words) %>%
  count(Source, word, sort = TRUE) %>%
  ungroup()

revDTM <- wordCount %>%
  cast_dtm(Source, word, n)

revLDA <- LDA(revDTM, k = 4, control = list(seed = 1234))

revTopics <- tidy(revLDA, matrix = "beta")






