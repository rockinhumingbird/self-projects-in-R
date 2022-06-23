library(dplyr);library(ggplot2);library(ggthemes)
library(tidyr);library(tidytext);library(tidyverse)
ramen=read_csv("ramen-ratings.csv")
ramen$Stars<-as.numeric(ramen$Stars)
#subsetting topten ramen
topten<-subset(ramen,(!is.na(ramen$`Top Ten`)))
#the topten and the 5-star ramen brand
topten%>%filter(Stars== '5')%>%
  ggplot(aes(Brand))+geom_bar()+coord_flip()

##among the topten list which brand has the highest freq
topten%>%
  group_by(Brand)%>%
  ggplot(aes(x=Brand)) + 
  geom_bar() + labs(x= 'Brand')+labs(y = 'Which Brand in Topten list') +
  scale_fill_discrete() +
  theme_few()+coord_flip()

#look at which country has the highest review# among the topten list
topten%>%
  ggplot(aes(reorder(Country,`Review #`)))+
  geom_bar()+labs(x="Country", y= 'Review#')+coord_flip()

#look at which country has highest stars# among the topten list
topten%>%
  ggplot(aes(reorder(Country,Stars)))+
  geom_bar()+labs(x="Country", y= 'Stars#')+coord_flip()

##see singapore-only list
ramensig <-ramen %>% filter(Country == "Singapore" )
##boxplot see the distribution of brand stars rating
ramensig%>%
       ggplot(aes(
         x= Brand,
         y= Stars, fill = Brand)) + geom_boxplot() +
  scale_y_continuous(name = "Stars") +
  scale_x_discrete(name = "SingaporeBrand") 

##see the Japan-only List
ramenJapan <-
  ramen %>% filter(Country == "Japan" )
###these are Japanese ramen rating =5 and number of reviews boxplot
ramenJapan%>%filter(ramenJapan$Stars=='5')%>%
  ggplot(aes(
    x= Brand,
    y= `Review #`, fill = Brand)) + geom_boxplot() +
    scale_x_discrete(name = "JBrand") +
    scale_y_continuous(name = "Reviews") +
    theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 10))+coord_flip()


####text analysis for variety
library(tidytext)
variety<- ramen$Variety
vdf<-data_frame(line = 1:2580, text= variety)
vdf<- vdf %>% 
  unnest_tokens(word, text)


##see the most frequent words in variety
vdf%>%count(word)%>%
  filter(n>50)%>%
  mutate(freq= n/sum(n))%>%
  ggplot(aes(word,n)) + 
  geom_col()+
  xlab(NULL)+
  coord_flip()


