
###### Analysis of text content of tweets ######

data <- read.csv('./data/Tweets.csv')

library(dplyr)
data <- select(data, airline_sentiment, negativereason, airline, text)
head(data)

# Remove the @airline bit of the text of the tweet
data$text <- gsub("^@\\w+ *", "", data$text)
head(data)


# divide tweets in 2 dataframes according to positive or negative sentiment
positive <- subset(data, airline_sentiment == 'positive')
negative <- subset(data, airline_sentiment == 'negative')

dim(positive); dim(negative)







###### Determine word frequency and build cloud of words for each sentiment ###### 

library(tm); library(SnowballC)
library(wordcloud)

# these words appear quite frequently in tweets and in my opinion are not informative,
# so I will remove them"
wordsToRemove <- c('get', 'cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')

# generate a function to analyse corpus text
analyseText <- function(text_to_analyse){
  # analyse text and generate matrix of words
  # Returns a dataframe containing 1 tweet per row, one word per column
  # and the number of times the word appears per tweet
  CorpusTranscript <- Corpus(VectorSource(text_to_analyse))
  CorpusTranscript <- tm_map(CorpusTranscript, content_transformer(tolower), lazy = T)
  CorpusTranscript <- tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
  CorpusTranscript <- tm_map(CorpusTranscript, removePunctuation)
  CorpusTranscript <- tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript <- tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript <- DocumentTermMatrix(CorpusTranscript)
  CorpusTranscript <- removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  CorpusTranscript <- as.data.frame(as.matrix(CorpusTranscript))
  colnames(CorpusTranscript) <- make.names(colnames(CorpusTranscript))
  
  return(CorpusTranscript)
}

# analysis of negative tweets
words <- analyseText(negative$text)
dim(words)

# sum the number of times each word appears in total accross all negative tweets.
freqWords_neg <- colSums(words)
freqWords_neg <- freqWords_neg[order(freqWords_neg, decreasing = T)]
head(freqWords_neg)


# analysis of positive tweets
words <- analyseText(positive$text)
dim(words)

freqWords_pos <- colSums(words)
freqWords_pos <- freqWords_pos[order(freqWords_pos, decreasing = T)]
head(freqWords_pos)

# combine thanks and remove extra column
freqWords_pos[1] <- freqWords_pos[1] + freqWords_pos[2]
freqWords_pos <- freqWords_pos[-2]
head(freqWords_pos)


# word clouds
par(mfrow = c(1,2))

wordcloud(freq = as.vector(freqWords_neg), 
          words = names(freqWords_neg),
          random.order = FALSE,
          random.color = FALSE, 
          colors = brewer.pal(9, 'Reds')[4:9])

wordcloud(freq = as.vector(freqWords_pos), 
          words = names(freqWords_pos),
          random.order = FALSE,
          random.color = FALSE, 
          colors = brewer.pal(9, 'BuPu')[4:9])



# generate a function to analyse corpus text and return a document term matrix instead of dataframe
# we can perform further analysis on document term matrices
analyseText2 <- function(text_to_analyse){
  # analyse text and generate matrix of words
  # Returns a dtm containing 1 tweet per row, one word per column
  # and the number of times the word appears per tweet
  CorpusTranscript <- Corpus(VectorSource(text_to_analyse))
  CorpusTranscript <- tm_map(CorpusTranscript, content_transformer(tolower), lazy = T)
  CorpusTranscript <- tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
  CorpusTranscript <- tm_map(CorpusTranscript, removePunctuation)
  CorpusTranscript <- tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript <- tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript <- DocumentTermMatrix(CorpusTranscript)
  CorpusTranscript <- removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  
  return(CorpusTranscript)
}


words_neg <- analyseText2(negative$text)
# find words correlated with the ones mentioned below (correlation at 70%)
findAssocs(words_neg, c("flight", 'customer', 'gate', 'phone'), .07)


words_pos <- analyseText2(positive$text)
findAssocs(words_pos, c("flight", 'awesome', 'amazing', 'service'), .07)




#### To further understand the associations between words, we can make clustering analysis of words. ####

# hierarchical clustering
d <- dist(t(as.matrix(words_neg)), method = 'euclidean')
fit <- hclust(d = d, method = 'ward.D')


#fancy plot
op <- par(mfrow = c(1,1), bg = "#DDE3CA")
plot(fit, 
     col = "#487AA1", 
     col.main = "#45ADA8", 
     col.lab = "#7C8071", 
     main = 'Negative Sentiment', 
     xlab = '',
     col.axis = "#F38630", 
     lwd = 3, 
     lty = 3, 
     sub = "", 
     hang = -1, 
     axes = FALSE)
# add axis
axis(side = 2, 
     at = seq(0, 400, 100), 
     col = "#F38630", 
     labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), 
      side = 2, 
      at = seq(0, 100, 10), 
      line = 1, 
      col = "#A38630", 
      las = 2)

plot.new()
plot(fit, hang=-1, main = 'Negative Sentiment', xlab = '')
rect.hclust(fit, k=4, border="red")




# positive sentiment tweets
d <- dist(t(as.matrix(words_pos)), method = 'euclidean')
fit <- hclust(d = d, method = 'ward.D')

#fancy plot
op <- par(bg = "#DDE3CA")
plot(fit, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", main = 'Positive Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)


# k-means clustering

library(cluster)
library(fpc)  


d <- dist(t(as.matrix(words_neg)), method="euclidean")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex = 0.4, main = 'Negative Sentiment')


d <- dist(t(as.matrix(words_pos)), method="euclidean")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0, cex = 0.4, main = 'Positive Sentiment')
