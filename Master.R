#-----Library Loading-----
library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(magrittr)
library(tidyr)
library(wordcloud)
library(ggplot2)
library(stopwords)

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

extraWords <- bind_rows(data_frame(word = c('call','mapas', 'historia', 'mundial', 'armas', 'é', 'und', 'multijugador', '????????', 'NA', '2', '60', 'campaña', 'duty','battlefield', 'cod', 'wwii', 'juego','modo', 'guerra', 'bien', 'bien', 'si', '6', '66', 'lul', "game", 'play', 'star', 'wars', 'bf1', 'bf2', 'players', "battlefront", "playing", "ea", 'kill', 'death','assault', 'damage', 'like', 'gift', 'fun', 'nice', 'amazing', 'awesome', '4', 'xbox', 'loves', 'battle', 'son', '3', '10', 'reviews', 'pretty', 'grandson', '1'), 
                                   lexicon = c("custom")), 
                        stop_words)
tidyGame <- tidyGame %>%
  anti_join(extraWords)
#Plot 1
gameSent <- tidyGame %>%
  inner_join(get_sentiments("bing")) %>%
  count(prod, index = index %/% 4, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(gameSent, aes(index, sentiment, fill = prod)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~prod, ncol = 2, scales = "free_x") +
  labs(title='Sentiment By Review') +
  theme(plot.title = element_text(hjust = 0.5))

wordCount <- tidyGame %>%
  group_by(prod) %>%
  summarize(word = n())

#Plot 2
bingChart <- tidyGame %>%
  right_join(get_sentiments('bing')) %>%
  group_by(prod) %>%
  count(sentiment)

bingChart <- bingChart[-c(9, 10), ]

ggplot(bingChart, aes(x = prod, y = n,fill=sentiment)) +
  geom_bar(stat='identity', width = 0.45) +
  facet_wrap(~prod, scales = "free") +
  labs(title='Positive vs. Negative') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))

#Plot 3
nrcChart <- tidyGame %>%
  right_join(get_sentiments('nrc')) %>%
  group_by(prod) %>%
  count(sentiment)

nrcChart <- nrcChart[-c(41:50), ]

ggplot(nrcChart, aes(x = prod, y = n,fill=sentiment)) +
  geom_bar(stat='identity', width = 0.45) +
  facet_wrap(~prod, scales = "free") +
  labs(title='NRC Sentiments') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))
#-----Topic Modeling
library(topicmodels)

words <- masterDF %>%
  mutate(index = row_number()) %>%
  unnest_tokens(word, body)

count <- words %>%
  anti_join(stop_words) %>%
  anti_join(extraWords) %>%
  anti_join(get_stopwords('es')) %>%
  anti_join(get_stopwords('pt')) %>%
  anti_join(get_stopwords('ru')) %>%
  count(word, index, sort = TRUE) %>%
  ungroup()

count <- na.omit(count)

gameDTM <- count %>%
  cast_dtm(index, word, n)

gameLDA <- LDA(gameDTM, k = 5, control = list(seed = 1234))

topics <- tidy(gameLDA, matrix = 'beta')

topTerms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#-----Plot 4
topTerms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#-----Plot 5
gameGamma <- tidy(gameLDA, matrix = 'gamma')
gameGamma <- gameGamma %>%
  separate(document, 'prod', sep = '_', convert = TRUE)
