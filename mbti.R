library(dplyr)
library(tidyr)
library(tidyverse)
library(tidytext)
library(tm)
library(SnowballC)
library(stringr)
library(wordcloud)

mbti<-read_csv("mbti_1.csv")
#remove special characters
mbti$posts <- str_replace_all(mbti$posts,"<.*?>","")
removehttp <- function(x) gsub("http.*", " ", x)
# function to remove additional special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
mbti$posts  <- sapply(mbti$posts , removehttp)
mbti$posts  <- sapply(mbti$posts , removeSpecialChars)

library(stringr)
mbti<-mbti%>%
  group_by(type)%>%
  ungroup()
mbtiword<-
  mbti%>%
  unnest_tokens(word,posts)
#remove stopwords
data("stop_words")
mbtiword <- mbtiword %>%
  anti_join(stop_words)
#sort high frequency words
mbtiword %>%group_by(type)%>%
  count(word, sort = TRUE)
#upper 5% count and word cloud
wordCountUpper = data.frame(table(unlist(strsplit(mbtiword$word), " ")))
wordCountUpper = wordCountUpper[order(-wordCountUpper$Freq),]
wordCountUpper=wordCountUpper[-(0:10), ]
wordcloud(words = wordCountUpper[, 1],
          freq = wordCountUpper[, 2],
          min.freq = 100, max.words = 100)
popular_words <- mbtiword %>% 
  group_by(type) %>%
  count(word, type, sort = TRUE) %>%
  slice(seq_len(20)) %>%
  ungroup() %>%
  arrange(type,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n)) +
  geom_col(show.legend = NULL, fill = "#edc948", alpha=.75) +
  labs(x = NULL, y = "Count") +
  ggtitle("Popular Words by Type") +   
  facet_wrap(~type, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()  +
  theme_minimal()



#plot most commonwords
library(ggplot2)
mbtiword %>%
  count(word, type,sort=TRUE) %>%
  filter(n>1000) %>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(word,n)) + 
  geom_col()+
  xlab(NULL)+
  coord_flip()


ggplot(mbtiword, aes(x=word))+
  geom_bar(stat="count", bins= 9, fill="violetred4") + 
  geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white") +
  ggtitle("Star Counts") +
  xlab("Stars") + ylab("Count") +
  theme_minimal()
