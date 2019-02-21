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

# function to remove additional special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
mbti$posts  <- sapply(mbti$posts , removeSpecialChars)

library(stringr)
mbti<-mbti%>%
  group_by(type)%>%
  ungroup()
mbti<-mbti%>%unnest_tokens(word,posts)

#remove stopwords
data("stop_words")
mbti <- mbti %>%
  anti_join(stop_words)
#sort high frequency words
mbti %>%
  count(word, sort = TRUE)


popular_words <- mbti %>% 
  group_by(type) %>%
  count(word, type, sort = TRUE) %>%
  slice(seq_len(20)) %>%
  ungroup() %>%
  arrange(type,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n)) +
  geom_col(show.legend = NULL, fill = "#edc948", alpha=.75) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Year") +   
  facet_wrap(~type, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()  +
  theme_minimal()



#plot most commonwords
library(ggplot2)
mbti %>%
  count(word, sort=TRUE) %>%
  filter(n>10000) %>%
  mutate(word = reorder(word, n))%>%
  ggplot(aes(word,n)) + 
  geom_col()+
  xlab(NULL)+
  coord_flip()

#get rid of http and 1
