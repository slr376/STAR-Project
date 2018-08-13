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

AmzID = c(AmzSWBII, AmzSWB, AmzCOD, AmzBF)

revDF <- data.frame()

#Amazon

#Remove all white space
AmzRevDF <- NULL

for (id in AmzID) {
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  prod_code = id
  url <- paste0("https://www.amazon.com/dp/", prod_code)
  doc <- read_html(url)
  
  #obtain the text in the node, remove "\n" from the text, and remove white space
  prod <- html_nodes(doc, "#productTitle") %>% html_text() %>% gsub("\n", "", .) %>% trim()
  pages <- 20
  Amzurl = c()
  for(page_num in 0:pages){
    url <- paste0("http://www.amazon.com/product-reviews/",id,"/?pageNumber=", page_num)
    Amzurl = c(Amzurl, url)
  }
  for(u in Amzurl) {
    doc <- read_html(u)
    body <- html_text(html_nodes(doc, css = ".review-text"))
    stars <- doc %>%
      html_nodes("#cm_cr-review_list  .review-rating") %>%
      html_text() %>%
      str_extract("\\d") %>%
      as.numeric()
    Amztemp <- data.frame(prod, body, stars)
    AmzRevDF <- rbind(AmzRevDF, Amztemp)
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
    if(identical(last, numeric(0)) == TRUE || is.na(last)) {
      url <- u
      MCurlList <- c(MCurlList, url)
    } 
    else {
      for (page_num in 1:last) {
        url <- paste0(u, '?page=', page_num)
        MCurlList <- c(MCurlList, url)
      }
    }
  }
  for (x in MCurlList) {
    webpage <- read_html(x)
    body <- html_text(html_nodes(webpage, css = '.review_body'))
    body <- body[2:length(body)]
    stars <- html_text(html_nodes(webpage, css = '.indiv'))
    prod <- rep(game, length(body))
    MCtemp <- data.frame(prod, body, stars)
    MCRevDF <- rbind(MCRevDF, MCtemp)
  }
}

MCRevDF$stars <- as.numeric(MCRevDF$stars)

masterDF <- rbind(MCRevDF, AmzRevDF)
masterDF[masterDF$prod == 'Star Wars Battlefront II - Xbox One',] <- 'star-wars-battlefront-ii'
masterDF[masterDF$prod == 'Star Wars: Battlefront - Standard Edition - Xbox One',] <- 'star-wars-battlefront'
masterDF[masterDF$prod == 'Call of Duty: WWII - PlayStation 4 Standard Edition',] <- 'call-of-duty-wwii'
masterDF[masterDF$prod == 'Electronic Arts Battlefield 1 - Xbox One',] <- 'battlefield-1'

bfDF <-masterDF[masterDF$prod == 'battlefield-1', ]
swbfiiDF <-masterDF[masterDF$prod == 'star-wars-battlefront-ii', ]
swbfDF <-masterDF[masterDF$prod == 'star-wars-battlefront', ]
codDF <-masterDF[masterDF$prod == 'call-of-duty-wwii', ]

masterDF$body <-as.character(masterDF$body)

#-----Sentiment Analysis
tidyGame <- masterDF %>%
  group_by(prod) %>%
  mutate(index = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, body)

data("stop_words")
tidyGame <- tidyGame %>%
  anti_join(stop_words)

extraWords <- bind_rows(data_frame(word = c("game", 'play', 'star', 'wars', 'bf1', 'bf2', 'players', "battlefront", "playing", "ea", 'kill', 'death','assault', 'damage', 'like', 'gift', 'fun', 'nice', 'amazing', 'awesome', '4', 'xbox', 'loves', 'battle', 'son', '3', '10', 'reviews', 'pretty', 'grandson', '1'), 
                                   lexicon = c("custom")), 
                        stop_words)
tidyGame <- tidyGame %>%
  anti_join(extraWords)

gameSent <- tidyGame %>%
  inner_join(get_sentiments("bing")) %>%
  count(prod, index = index %/% 4, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(gameSent, aes(index, sentiment, fill = prod)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~prod, ncol = 2, scales = "free_x") +
  labs(title='Title')

wordCount <- tidyGame %>%
  group_by(prod) %>%
  summarize(word = n())

bingChart <- tidyGame %>%
  right_join(get_sentiments('bing')) %>%
  group_by(prod) %>%
  count(sentiment)

bingChart <- bingChart[-c(9, 10), ]

ggplot(bingChart, aes(fill=sentiment, ymax=cumsum(bingChart$n), ymin=c(0, head(cumsum(bingChart$n), n=-1)), xmax=4, xmin=3)) +
  geom_rect(colour="grey30") +
  facet_wrap(~prod, ncol = 2) +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  labs(title="Positive vs. Negative")
#-----Topic Modeling
