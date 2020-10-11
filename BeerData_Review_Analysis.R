#Loading Important Packages

library(dplyr)
library(NLP)
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")

library("RColorBrewer")
library("syuzhet")
library("ggplot2")
library(readr)
library(lubridate)
library(tidytext)
library(tidyr)
library(dplyr)
library(ggplot2)
install.packages("wordcloud2")
library(wordcloud2)
library(RColorBrewer)
library(wordcloud)
install.packages("textdata")
library(textdata)

#Loading the Data
data<-read.csv(file.choose(),header = TRUE)
#will have to perform text mining and Sentimental Analysis 
# Due to the extremely large size of data, Will be splitting data based on beer styles and comparing the reviews for the different styles and the count of positive and negative reviews
unique(data$beer_style) #Gives the different styles of beer

typeof(data$beer_style) #This variable is a character 

data$beer_style<-as.factor(data$beer_style) #We convert to factor

summary(data$beer_style)  
#Gives the frequency of each style of beer, which also indicates the number of reviews per style
#Clearly, the one with maximum reviews will have more number of positive reviews too(negative too)
#Due to the very large size of data, will only see the top 5 styles of beer with highest intake and compare the result
#There will be a selection Bias, but due to a large variety and large volume of data, this seems to be the optimum solution

#Top 5 beer styles are
# American IPA      American Double / Imperial IPA    American Double / Imperial Stout  American Pale Ale (APA)            American Amber / Red Ale  
# creating 5 subsets based on beer style

data1<-filter(data,data$beer_style=="American IPA")
data2<-filter(data,data$beer_style=="American Double / Imperial IPA")
data3<-filter(data,data$beer_style=="American Double / Imperial Stout")
data4<-filter(data,data$beer_style=="American Pale Ale (APA)")
data5<-filter(data,data$beer_style=="American Amber / Red Ale")

#data1: Word Cloud and Sentimental Analysis

TextDoc <- Corpus(VectorSource(data1))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

#Sentimental Analysis

data11<-filter(data,data$beer_style=="American IPA")
# break the headlines_into tokens of words using the nrc lexicon
d <- data11 %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("nrc"))
head(d, n=20)

d %>%
  group_by(sentiment) %>%
  summarise(sumSent = n()) %>%
  arrange(desc(sumSent))
print(d)


d %>%
  group_by(review_overall,sentiment) %>%
  summarise(review = n()) %>%
  ggplot(aes(x=reorder(sentiment,review), y=review, fill=factor(sentiment))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 25)) +
  facet_wrap(~review_overall)
###########################################################################################################################################################

#data2: Word Cloud and sentimental Analysis

TextDoc <- Corpus(VectorSource(data2))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
#Sentimental Analysis
data22<-filter(data,data$beer_style=="American Double / Imperial IPA")
# break the headlines_into tokens of words using the nrc lexicon
d1 <- data22 %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("nrc"))
head(d1, n=20)

d1 %>%
  group_by(sentiment) %>%
  summarise(sumSent = n()) %>%
  arrange(desc(sumSent))
print(d1)

d1 %>%
  group_by(review_overall,sentiment) %>%
  summarise(review = n()) %>%
  ggplot(aes(x=reorder(sentiment,review), y=review, fill=factor(sentiment))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 25))  +
  facet_wrap(~review_overall)

#########################################################################################################################################

#data3: Word cloud and sentimental analysis

TextDoc <- Corpus(VectorSource(data3))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
#Sentimental Analysis
data33<-filter(data,data$beer_style=="American Double / Imperial Stout")
# break the headlines_into tokens of words using the nrc lexicon
d2 <- data33 %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("nrc"))
head(d2, n=20)

d2 %>%
  group_by(sentiment) %>%
  summarise(sumSent = n()) %>%
  arrange(desc(sumSent))
print(d2)

d2 %>%
  group_by(review_overall,sentiment) %>%
  summarise(review = n()) %>%
  ggplot(aes(x=reorder(sentiment,review), y=review, fill=factor(sentiment))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 25))  +
  facet_wrap(~review_overall)
#####################################################################################################################################################################################

#data4: Word Cloud and sentimental analysis

TextDoc <- Corpus(VectorSource(data4))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
#Sentimental Analysis
data44<-filter(data,data$beer_style=="American Pale Ale (APA)")
# break the headlines_into tokens of words using the nrc lexicon
d3 <- data44 %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("nrc"))
head(d3, n=20)

d3 %>%
  group_by(sentiment) %>%
  summarise(sumSent = n()) %>%
  arrange(desc(sumSent))
print(d3)

d3 %>%
  group_by(review_overall,sentiment) %>%
  summarise(review = n()) %>%
  ggplot(aes(x=reorder(sentiment,review), y=review, fill=factor(sentiment))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 25))  +
  facet_wrap(~review_overall)

###################################################################################################################################################################

#data5: Word Cloud and sentimental analysis

TextDoc <- Corpus(VectorSource(data5))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
#TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team")) 
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 10)
# Plot the most frequent words
barplot(dtm_d[1:5,]$freq, las = 2, names.arg = dtm_d[1:5,]$word,
        col ="lightgreen", main ="Top 5 most frequent words",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
#Sentimental Analysis
data55<-filter(data,data$beer_style=="American Amber / Red Ale")
# break the headlines_into tokens of words using the nrc lexicon
d4 <- data55 %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("nrc"))
head(d4, n=20)

d4 %>%
  group_by(sentiment) %>%
  summarise(sumSent = n()) %>%
  arrange(desc(sumSent))
print(d4)

d4 %>%
  group_by(review_overall,sentiment) %>%
  summarise(review = n()) %>%
  ggplot(aes(x=reorder(sentiment,review), y=review, fill=factor(sentiment))) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 25))  +
  facet_wrap(~review_overall)
