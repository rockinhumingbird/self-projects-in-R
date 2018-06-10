library(twitteR)
library(ROAuth)

my.key<-""
my.secret<- ""
setup_twitter_oauth(my.key,my.secret)

blocktweets = searchTwitter(searchString = 'blockchainpr',n=10000,lang="en")
write.csv(bigdata.df, "C:/./bigdata.csv")
 
dtm = DocumentTermMatrix(bcorpus)
 
tweetsdf = as.data.frame(as.matrix(dtm))
inspect(dtm[1:5,1:7])
sparse = removeSparseTerms(dtm,0.99)
inspect(sparse[1:10,1:10])
library(tidytext)
library(dplyr)
tweets_words = 
  thebachelorette%>%
  group_by(X)%>%
  unnest_tokens(output = word,input = text)%>%
  ungroup()%>%
  mutate(row=1:n())
as.data.frame(tweets_words)[1:50,c('X','screenName','word')]

 tweetsSparse = as.data.frame(as.matrix(sparse))
 colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
 wordcloud(colnames(tweetsSparse),colSums(tweetsSparse),scale=c(3.5,0.25),min.freq=2)
 wordcloud(colnames(tweetsSparse),colSums(tweetsSparse),scale=c(5,0.5),min.freq=5,colors=brewer.pal(9),"Spectral")[-c(1,3)]
 
 ## score each tweet. Sentiment = no. of pos. words - no. of neg. words
 library(stringr); library(plyr)
 score = laply(tweetsText,function(x){
   x = gsub('[[:punct:]]', '', x)
   x = gsub('[[:cntrl:]]', '', x)
   x = gsub('\\d+', '', x)
   xList = str_split(x,"\\s+")
   y = unlist(xList)
   y = iconv(enc2utf8(y), sub = "byte") #convert non UTF-8 char
   y =tolower(y)
   sum(!is.na(match(y,positiveWords))) - sum(!is.na(match(y,negativeWords)))
 })

 
 tweetsText = iconv(enc2utf8(tweetsText), sub = "byte") #convert to UTF-8 char
 tweetsdf = data.frame(score=score,tweets = tweetsText)
 tweetsdf
 library(tidytext)
 library(dplyr)
 bcorpus = tm_map(bcorpus,removeWords,c("the",'bachelor', 'thebachelor','thebachelorette''',stopwords("english")))

 tweets_words = 
   tweetsText%>%
   group_by(X)%>%
   unnest_tokens(output = word,input = text)%>%
   ungroup()%>%
   mutate(row=1:n())
 as.data.frame(tweets_words)[1:50,c('X','screenName','word')]
  library(ggplot2)
bcorpus%>%
   inner_join(get_sentiments('bing'),by = 'word')%>%
   select('sentiment')%>%
   group_by(sentiment)%>%
   summarize(freq=n())%>%
   ungroup()%>%
   ggplot(aes(x=sentiment,y=freq))+geom_bar(position='dodge',stat='identity',fill=c('red','green'))

library(ggplot2); library(ggthemes)
ggplot(tweetsdf,aes(score))+
  geom_histogram(fill='tomato')+
  theme_economist()+
  ggtitle(label = paste('Average Sentiment =',mean(tweetsdf$score)))
