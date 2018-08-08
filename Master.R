#-----Library Loading-----
library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(magrittr)
library(tidyr)
library(wordcloud)
library(ggplot2)

#-----Data Collection & Storage-----
AmzSWBII <- 'B071Y1RXHG'
McSWBII <- 'http://www.metacritic.com/game/playstation-4/star-wars-battlefront-ii/user-reviews'

AmzSWB <- 'B00W8FYFBU'
McSWB <- 'http://www.metacritic.com/game/playstation-4/star-wars-battlefront/user-reviews'

AmzCOD <- 'B071QY1WLY'
McCOD <- 'http://www.metacritic.com/game/playstation-4/call-of-duty-wwii/user-reviews'

AmzBF <- 'B01F9HMO9S'
McBF <- 'http://www.metacritic.com/game/playstation-4/battlefield-1/user-reviews'

McURL = c(McSWB, McCOD, McBF)   #Took out SWBII due to bug
AmzID = c(AmzSWBII, AmzSWB, AmzCOD, AmzBF)

revDF <- data.frame()
MCRevList = c()

#Amazon
prodList = c()

#Remove all white space
AmzRevDF <- NULL

for (id in AmzID) {
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  prod_code = id
  url <- paste0("https://www.amazon.com/dp/", prod_code)
  doc <- read_html(url)
  
  #obtain the text in the node, remove "\n" from the text, and remove white space
  prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
  prodList = c(prodList, prod)
  
  pages <- 20
  reviews_all <- NULL
  for(page_num in 0:pages){
    url <- paste0("http://www.amazon.com/product-reviews/",id,"/?pageNumber=", page_num)
    doc <-read_html(url)
    
    body <- html_text(html_nodes(doc, css = ".review-text"))
    stars <- doc %>%
      html_nodes("#cm_cr-review_list  .review-rating") %>%
      html_text() %>%
      str_extract("\\d") %>%
      as.numeric()
    RevDF <- data.frame(prod, body, stars)
    AmzRevDF <- rbind(AmzRevDF, RevDF)
  }
}

games = c('star-wars-battlefront-ii', 'star-wars-battlefront', 'call-of-duty-wwii', 'battlefield-1')
consoles = c('xbox-one', 'pc', 'playstation-4')
MCRevDF <- NULL

for (game in games) {
  baseList = c()
  urlList = c()
  MCurlList = c()
  
  for (console in consoles) {
    base <- paste0('http://www.metacritic.com/game/',console)
    baseList <- c(baseList, base)
  }
  for (b in baseList) {
    url <- paste0(b, '/', game, '/user-reviews')
    urlList <- c(urlList, url)
  }
  for (u in urlList) {
    webpage <- read_html(u)
    last <- html_nodes(webpage, css = ".last_page")
    last <- as.numeric(html_text(last))
    if(identical(last, numeric(0)) == TRUE) {
      body <- html_text(html_nodes(doc, css = ".review_body"))
      temp <- data.frame(game, body)
    } 
    else {
      for (page_num in 1:last) {
        url <- paste0(u, '?page=', page_num)
        doc <- read_html(url)
        body <- html_text(html_nodes(doc, css = ".review_body"))
        temp <- data.frame(game, body)
      }
      MCRevDF <- rbind(MCRevDF, temp)
    }
  }
}

