library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(magrittr)

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

clean <- function(string) {
  #temp <- tolower(string)
  temp <- stringr::str_replace_all(string,'[.,,,(,),-,:]','')
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- stringr::str_split(temp, " ")[[1]]
  return(temp)
} 

cleanList = c()

#Cleaning up each review and turns results into individual characters
for (rev in revList) {
  cleanList = c(cleanList, clean(rev))
}
