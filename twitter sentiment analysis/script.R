install.packages('twitteR', dependencies=T)
install.packages("tm")
install.packages("wordcloud")

library(twitteR)
library(plyr)

ckey <- 'iRUD7RcGsMCimTFb3kvelREBM' 
csecret <- 'yxawQWNLouKMU8PfonUo0ZNTPtWPZYWb9HrkZY4wQbIViF9ANh'
atoken <- '4902591811-sdannBfZIZ7FShr35kEb99EJgdIXW3ffRESaZxX'
asecret <- 'FOlpSxW4TpsaQzyvM1cb32CrJRaRS2sHkIpKh5vrEmSVL'
setup_twitter_oauth(ckey, csecret, atoken, asecret)

LFC_tweets = searchTwitter("Liverpool", n=1500, lang="en")
require(tm)
require(wordcloud)

#himearth <- searchTwitter('earthquake+himalaya', lang="en", n=50, resultType = "recent")

#LFC_corpus 

LFC_text <- sapply(LFC_tweets, function(x) x$getText())
LFC_text <- lapply(LFC_text, function(x) gsub("[^[:alnum:]///' ]", "", x))
LFC_text <- lapply(LFC_text, function(x) gsub("[/]+", "", x))
LFC_text <- lapply(LFC_text, function(x) gsub("http(s?)[//]*[a-z]*", "", x))


LFC_corpus <- Corpus(VectorSource(LFC_text))
LFC_clean <- tm_map(LFC_corpus, content_transformer(tolower))
LFC_clean <- tm_map(LFC_clean, removeWords, stopwords("english"))
LFC_clean <- tm_map(LFC_clean, removeNumbers)
LFC_clean <- tm_map(LFC_clean, stripWhitespace)
inspect(LFC_clean[1])

wordcloud(LFC_clean, random.order=F, max.words=100, colors=rainbow(200), scale=c(5,0.1), rot.per = 0.35)

library(syuzhet)
#tdm <- TermDocumentMatrix(LFC_clean)
#mySentiment <- get_nrc_sentiment(LFC_text[[1]])

dataframe<-data.frame(text= sapply(LFC_clean, as.character), stringsAsFactors = FALSE)
mySentiment <- get_nrc_sentiment(dataframe$text)
sentimentTotals <- data.frame(colSums(mySentiment))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
#rownames(sentimentTotals) <- NULL
library(ggplot2)

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")
