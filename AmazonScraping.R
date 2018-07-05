library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(magrittr)
library(tidyverse)

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
  unnest_tokens(words, text)

#Removes Stop Words
data("stop_words")
textDF <- textDF %>%
  anti_join(stop_words, by=c("words"="word"))

textDF %>%
  count(words, sort = TRUE)

nrc_neg <- get_sentiments("nrc") %>% 
  filter(sentiment == "disgust")

textDF %>%
  filter() %>%
  inner_join(nrc_neg, by=c('words'='word')) %>%
  count(words, sort = TRUE)