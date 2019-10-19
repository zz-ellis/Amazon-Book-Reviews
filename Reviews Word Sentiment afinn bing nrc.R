#630 Group Project
#AFINN Sentiment Analysis
#Reference: #http://rpubs.com/mkivenson/sentiment-reviews

#LOAD PACKAGES
library(DT)
library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(tm)
library(wordcloud)
library(reticulate)
library(crfsuite)
library(tidyr)

#SET WORKING DIRECTORY TO FILE WHERE COMBINED.CSV IS

#IMPORT DATA
df <- read.csv(file="combined.csv", header=FALSE, sep="\t")
Reviews <- df

#SPLIT ASIN and ID
library(stringi)
library(stringr)
Reviews$doc_id <- stri_sub(Reviews$V2, 22, -17)
Reviews$asin <- stri_sub(Reviews$V2,-10)
Reviews$rating <- as.numeric(Reviews$V1)
Reviews$review.title <- Reviews$V3
Reviews$text <- stri_sub(Reviews$V4, 39)
Reviews <- subset(Reviews, select=-c(1:4))
head(Reviews)

#REMOVE BREAKS AND SPAN
Reviews$text <- str_replace_all(Reviews$text, "[\\r\\n\\t]+", " ")
Reviews$text <- str_replace_all(Reviews$text, "</span>", " ")
Reviews$text <- str_replace_all(Reviews$text, "<br/><br/>", " ")
Reviews$text <- str_replace_all(Reviews$text, "â???T", "'")

#SUMMARY
summary(Reviews)

#WORD SUMMARY
words <- Reviews %>%
  select(c("doc_id", "asin", "rating", "text")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))

datatable(head(words))

#AFINN SENTIMENT
afinn <- get_sentiments("afinn") %>% mutate(word = wordStem(word))
Reviews.afinn <- words %>%
  inner_join(afinn, by = "word")
head(Reviews.afinn)

summary(afinn$value)
library(ggplot2)
qplot(afinn$value,   geom="histogram",binwidth=1,main="AFINN Review Sentiment Histogram")

#MOST COMMON WORDS
word_summary <- Reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(value), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#VIEW COMMON WORDS
ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue") + coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

#WORDCLOUD
library(RColorBrewer)
wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#POSITIVE WORDS
good <- Reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(value), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#NEGATIVE WORDS
bad <- Reviews.afinn %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(value), count_word = n()) %>%
  filter(count_word>1000) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#BY BOOK
review_summary <- Reviews.afinn %>%
  group_by(asin) %>%
  summarise(mean_rating = mean(rating), sentiment = mean(value))
datatable(head(review_summary))

#PLOT
y_mid = 0
x_mid = 3.5

review_summary %>% 
  mutate(quadrant = case_when(mean_rating > x_mid & sentiment > y_mid   ~ "Positive Review/Postive Sentiment",
                              mean_rating <= x_mid & sentiment > y_mid  ~ "Negative Review/Positive Sentiment",
                              mean_rating <= x_mid & sentiment <= y_mid ~ "Negative Review/Negative Sentiment",
                              TRUE                                      ~ "Positive Review/Negative Sentiment")) %>% 
  ggplot(aes(x = mean_rating, y = sentiment, color = quadrant)) + 
  geom_hline(yintercept=y_mid, color = "black", size=.5) + 
  geom_vline(xintercept=x_mid, color = "black", size=.5) +
  guides(color=FALSE) +
  scale_color_manual(values=c("lightgreen", "pink", "pink","lightgreen")) +
  ggtitle("Amazon Product Rating vs Sentiment Rating of Review") +
  ggplot2::annotate("text", x = 4.33, y=3.5,label="Positive Review/Postive Sentiment") +
  ggplot2::annotate("text", x = 2, y=3.5,label="Negative Review/Positive Sentiment") +
  ggplot2::annotate("text", x = 4.33, y=-2.5,label="Positive Review/Negative Sentiment") +
  ggplot2::annotate("text", x = 2, y=-2.5,label="Negative Review/Negative Sentiment") +
  geom_point()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#BING SENTIMENT
bing <- get_sentiments("bing") %>% mutate(word = wordStem(word))
Reviews.bing <- words %>%
  inner_join(bing, by = "word")
head(Reviews.bing)

summary(bing$sentiment)
library(ggplot2)
qplot(bing$sentiment,main="BING Review Sentiment Histogram")

#MOST COMMON WORDS
word_summary <- Reviews.bing %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#VIEW COMMON WORDS
ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue") + coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

#WORDCLOUD
library(RColorBrewer)
wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#POSITIVE WORDS
good <- Reviews.bing %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#NEGATIVE WORDS
bad <- Reviews.bing %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  filter(count_word>1000) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#NRC SENTIMENT
nrc <- get_sentiments("nrc") %>% mutate(word = wordStem(word))
Reviews.nrc <- words %>%
  inner_join(nrc, by = "word")
head(Reviews.nrc)

summary(nrc$sentiment)
library(ggplot2)
qplot(nrc$sentiment, main="NRC Review Sentiment Histogram")

#MOST COMMON WORDS
word_summary <- Reviews.nrc %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  arrange(desc(count_word))
datatable(head(word_summary))

#VIEW COMMON WORDS
ggplot(filter(word_summary, count_word < 50000), aes(mean_rating, score)) + geom_text(aes(label = word, color = count_word, size=count_word), position= position_jitter()) + scale_color_gradient(low = "lightblue", high = "darkblue") + coord_cartesian(xlim=c(3.5,4.5)) + guides(size = FALSE, color=FALSE)

#WORDCLOUD
library(RColorBrewer)
wordcloud(words = word_summary$word, freq = word_summary$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#POSITIVE WORDS
good <- Reviews.nrc %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  filter(mean_rating>mean(mean_rating)) %>%
  arrange(desc(mean_rating))
wordcloud(words = good$word, freq = good$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))

#NEGATIVE WORDS
bad <- Reviews.nrc %>%
  group_by(word) %>%
  summarise(mean_rating = mean(rating), score = max(sentiment), count_word = n()) %>%
  filter(count_word>1000) %>%
  filter(mean_rating<mean(mean_rating)) %>%
  arrange(mean_rating)
wordcloud(words = bad$word, freq = bad$count_word, scale=c(5,.5), max.words=100, colors=brewer.pal(8, "Set2"))
