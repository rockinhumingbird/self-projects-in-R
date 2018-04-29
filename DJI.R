#####################Loading of packages and general overview of the dataset#####################

# The necessary packages are installed and loaded into R
install.packages("quantmod")
install.packages("ggplot2")
install.packages("forecast")
install.packages("xts")
install.packages("DataCombine")
install.packages("tidyquant")
install.packages("car")
install.packages("twitteR")
install.packages("ROAuth")
install.packages("tidytext")
install.packages("dplyr")
install.packages("ISLR")
install.packages("psych")
install.packages("broom")
install.packages("plotly")
library("quantmod")
library("ggplot2")
library("forecast")
library("xts")
library("DataCombine")
library("tidyquant")
library("car")
library("twitteR")
library("ROAuth")
library("tidytext")
library("dplyr")
library("ISLR")
library("psych")
library("broom")
library("plotly")

###############################PART1: General analyses and ARIMA prediction############################## 

# For the following analyses, an exact timeframe is defined for the concerned data
start <- as.Date("2007-09-01")
end <- as.Date("2017-12-31")

# US GDP data is loaded into the file from the FRED data base using the quantmod package
getSymbols(Symbols = 'GDP', src = "FRED")

# DJI data for the defined timeframe is scraped from Yahoo Finance (the standard source of the quantmod package)
getSymbols(Symbols = 'DJI', src = "yahoo", from = start, to = end)

# Before focusing on GDP further, the volatility of the DJI is assessed using Bollinger Bands
# Visualisation of BBand in ggplot2 
data("FANG")
DJI<-tq_get("DJI",get = "stock.prices", from = "2007-09-01", to = "2017-12-31")
DJI<as.data.frame(DJI)
ggplot(data=DJI,aes(x = date, y = close, open = open,
           high = high, low = low, close = close)) +
  geom_candlestick() +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
  labs(title = "AAPL Candlestick Chart", 
       subtitle = "BBands with SMA Applied", 
       y = "Closing Price", x = "") + 
  coord_x_date(xlim = c(end - weeks(240), end),
               ylim = c(10000, 25000)) + 
  theme_tq()


ggplot(data=DJI,aes(x = DJI$date, y = close, open = open,
                             high = high, low = low, close = close)) +
          geom_candlestick(aes(open = open, high = high, low = low, close = close))+
          geom_bbands(ma_fun = SMA, sd = 2, n = 20, 
          linetype = 4, size = 1, alpha = 0.2, 
          fill = palette_light()[[1]], 
          color_bands = palette_light()[[1]], 
          color_ma= palette_light()[[2]]) + 
          labs(title = "DJI Candlestick Chart", 
               subtitle = "BBands with SMA Applied, Experimenting with Formatting", 
               y = "Closing Price", x = "Date") + coord_x_date(xlim = c(end - weeks(240), end),ylim = c(10000, 25000)) +
          theme_tq()  

# Since FRED does not allow date ranges in the data for its "getSymbols" function,the dataset needs to be subset
GDP.subset <- GDP[paste(start,end,sep="/")]

# Both datasets are brought in the same format, using quarterly data for further analyses; 
# then, they are plotted; as we can see, the DJI is more volatile than the GDP data
GDP.quarterly <- to.quarterly(GDP.subset)
GDP.adjusted <- GDP.quarterly[, -(1:3)]
colnames(GDP.adjusted)[colnames(GDP.adjusted)=="GDP.subset.Close"] <- "GDP"
DJI.quarterly <- to.quarterly(DJI)
DJI.adjusted <- DJI.quarterly[, -(1:5)]
colnames(DJI.adjusted)[colnames(DJI.adjusted)=="DJI.Adjusted"] <- "DJI"
total.df1 <- merge(DJI.adjusted, GDP.adjusted)
head(total.df1)
plot(total.df1$DJI, main = "DJI vs. US GDP from 2008 to 2018")
lines(total.df1$GDP, col = "red")
axis(side = 3, at = pretty(total.df1$GDP))
legend(x = "bottomright", 
       legend = c("DJI", "GDP", "Financial crisis"), 
       col = c("black", "red", "blue"),
       lty = c(1, 1))
vert_line <- which(index(total.df1$DJI) == "2009 Q1")
abline(v = .index(total.df1$DJI)[vert_line], col = "blue")

# Quarterly changes for the DJI and GDP data is calculated
DJI_quarterly_grate <- total.df1$DJI/lag(total.df1$DJI,1) -1
DJI_quarterly_grate <- DJI_quarterly_grate[-(1:2), ]
head(DJI_quarterly_grate)
GDP_quarterly_grate <- total.df1$GDP/lag(total.df1$GDP,1) -1
GDP_quarterly_grate <- GDP_quarterly_grate[-(1:2), ]
total.df2 <- merge(DJI_quarterly_grate, GDP_quarterly_grate)
head(total.df2)
head(total.df1)
total.df1 <- total.df1[-(1:2), ]

# NA values are inspected and removed from the dataframe
sum(is.na(total.df2))
total.df <- na.omit(total.df2)

# First, the relationship between the two variables is assessed using a linear regression model 
lm_model <- lm(total.df2$DJI ~ total.df2$GDP, data = total.df2)
summary(lm_model)

# Next, a comprehensive time series diagnostic is performed 
par(mfrow = c(2, 2))
hist(total.df2$DJI)
lines(density(total.df2$DJI), col = "red")
acf(total.df2$DJI)
qqnorm(total.df2$DJI)
qqline(total.df2$DJI, col = "red")

# Using past data, a first seasonal forecast for both GDP and the DJI is completed;
# even if it is not a good forecasting method per se, it provides a useful benchmark for the following analysis
forecast.DJI <- naive(total.df2$DJI, h = 20)
forecast.GDP <- naive(total.df2$GDP, h = 20)
autoplot(forecast.DJI)
autoplot(forecast.GDP)

# Exponential smoothing including trend is used to predict the future development of the Dow Jones Index; 
# with alpha being very low, the predictions are highly different than the most recent observation
ho <- holt(total.df1$DJI, h = 10)
autoplot(ho)

# The accuracy of forecasts can only be determined by considering how well a model performs 
# on new data that were not used when fitting the model; the data is now split into a training set and a test set
training <- window(total.df1$DJI, end = 2016)
test <- window(total.df1$DJI, end = 2018)
# Accurary for forecasting of the training data on the test data is assessed
# the best model had the lowest error (particularly the MAPE, Mean absolute percentage error)
# Firstly, the mean method is assessed
accuracy(meanf(training, h = 100), test)
# Secondly, the naive method
accuracy(rwf(training, h = 100), test)
# Thirdly, the drift method is being looked at (which according to the MAPE is the most suitable of the used methods)
accuracy(rwf(training, drift = TRUE, h = 100), test)
# Lastly, the seasonal naive method is checked
accuracy(snaive(training, h = 100), test)

# With a p-value < 0.05 and a positive sign in front of the coefficient, 
# we can see that there is a statistically significant, postive relationship between the two variables;
# The relationship is plotted 
ggplot(total.df2, aes(x = GDP, y = DJI)) + 
  geom_point(color = 'red') + geom_smooth(method = "lm", se = FALSE) + ggtitle("Regression of % changes for US GDP and the DJI")

# A Fit model using the ARIMA function is created
fit <- auto.arima(total.df2[, "DJI"], xreg = total.df2[, "GDP"])
fit

# As with all residual models, we have to make sure that the residuals look like white noise (p-value > 0.05), 
# which means that the residuals are uncorrelated; also, the histogram looks like a normal curve
checkresiduals(fit)
                       
# A forecast from the dynamic regression model is created
fcast <- forecast(fit, xreg = rep(mean(total.df2[,2]), 10))
autoplot(fcast) + xlab("Years") + ylab("Change in %") + ggtitle("Forecasts from regression with ARIMA(0,0,0) errors")


###############################PART2: sentiment score for DJI, using twitter text mining############################## 

# Get API Credentials from Twitter
apiKey<-"WJy6wpIOsbYAmuV4bCxDWKrhl"
apiSecret<-"mYSNjipmsJq3jjbfFKGyPahOS9MkeD426r5IXfCpnhotrujnmR"
apiToken<-"982312249627852802-nxzPM6hP8b2SgeIKkNInWfbKOzPpaHL"
apiTokenSecret<-"8PB34oiS36pLw8sli5DE1N10aMLnCZkymo6NWYUn7e0sh"
setup_twitter_oauth(apiKey,apiSecret,apiToken,apiTokenSecret)

# gather tweets of Dow Jones 
set.seed(100)
tweets = searchTwitter(searchString = "dowjones",n=1000,lang="en")

# Convert tweets list to a data frame, save to local drive
# Examine structure of file. Review first ten tweets.
tweets = twListToDF(tweets)
tweets$X = as.character(1:nrow(tweets))
str(tweets)
write.csv(tweets,'tweets.csv')
tweets[1:10,c('screenName','text')]

# Extract words from all tweets. Examine first 30 rows of data.
tweets_words = 
  tweets%>%
  group_by(X)%>%
  unnest_tokens(output = word,input = text)%>%
  ungroup()%>%
  mutate(row=1:n())
as.data.frame(tweets_words)[1:30,c('X','screenName','word')]

# Bar Plot of positive and negative words, using "bing" lexicon
# We can see people have positve attitude towards dow jones industrail average today 
tweets_words %>%
  inner_join(get_sentiments('bing'),by = 'word')%>%
  select('sentiment')%>%
  group_by(sentiment)%>%
  summarize(freq=n())%>%
  ungroup()%>%
  ggplot(aes(x=sentiment,y=freq))+geom_bar(position='dodge',stat='identity',fill=c('red','green'))

# get the sentiment for each tweet, using "afinn" lexicon 
tweets_words %>%
  inner_join(get_sentiments('afinn'),by = 'word')%>%
  group_by(X)%>%
  summarize(tweet_sentiment = mean(score,na.rm=T))%>%
  ungroup()

# Calculate sentiment score, which is the average of the sentiment of all tweets
sentimentscore=as.numeric(tweets_words %>%
                            inner_join(get_sentiments('afinn'),by = 'word')%>%
                            group_by(X)%>%
                            summarize(tweet_sentiment = mean(score,na.rm=T))%>%
                            ungroup()%>%
                            summarize(sentiment=mean(tweet_sentiment,na.rm=T)) )

# Sentiment Score for Dowjones is 1.15 today! postive.
# But the Dow Jones Industrial Average is down 0.32% today 
print(sentimentscore)

###############################PART3: Logistics regression for the DJI future development############################## 

# Get DJI data from Yahoo
# For the following analyses, a new timeframe is defined 
start <- as.Date("2014-12-31")
end <- as.Date("2017-12-31")

# Using the new timeframe, the DJI data for the defined timeframe is scraped from Yahoo Finance
getSymbols(Symbols = 'DJI', src = "yahoo", from = start, to = end)

# Again, using the new timeframe, US GDP data is loaded into the file from the FRED data base using the quantmod package
getSymbols(Symbols = 'GDP', src = "FRED")
# Since FRED does not allow date ranges in the data for its "getSymbols" function,the dataset needs to be subset
GDP.subset <- GDP[paste(start,end,sep="/")]
head(GDP.subset)
GDP.quarterly <- to.quarterly(GDP.subset)
GDP.adjusted <- GDP.quarterly[, -(1:3)]
colnames(GDP.adjusted)[colnames(GDP.adjusted)=="GDP.subset.Close"] <- "GDP"
head(GDP.adjusted)

# The dataset is explored
names(DJI)
dim(DJI)
head(DJI,50)
summary(DJI)

# Convert data to quarterly data
DJI.quarterly <- to.quarterly(DJI)
names(DJI.quarterly) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
DJI_quarterly_grate <- DJI.quarterly$Close/lag(DJI.quarterly$Close,1) -1
DJI_quarterly_grate <- DJI_quarterly_grate[-(1:1), ]
colnames(DJI_quarterly_grate)[colnames(DJI_quarterly_grate)=="Close"] <- "Returns"

# combine sentimentscore with other independent varibales to the dataframe 
Quarterly_data <- cbind(DJI.quarterly, DJI_quarterly_grate$Returns, GDP.adjusted)
colnames(Quarterly_data)[colnames(Quarterly_data)=="DJI.Adjusted"] <- "Adjusted"
Quarterly_data <- Quarterly_data[-1,]
head(Quarterly_data)
Quarterly_data$Returns[Quarterly_data$Returns<0] <- 0
Quarterly_data$Returns[Quarterly_data$Returns>0] <- 1


# correlation matrix
cor(Quarterly_data[,-9])

# fit a logistic regression model to predict Returns
# using the different prices and the Volume
# glm(): generalized linear model function
# family=binomial => logistic regression

glm.fit <- glm(Returns~Open+High+Low+Close+Volume+GDP,
               data = Quarterly_data, family = binomial)
summary(glm.fit)


# probability table
augment(glm.fit)


# explore fitted model coefficients
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[ ,4]


# Prediction of DJI return direction from 2015Q1 - 2017Q4
glm.pred <- predict(glm.fit, type = "response")
glm.pred


# Histogram of all predictions, "1" DJI goes up, "0"DJI goes down 
ggplot(data=data.frame(glm.pred),aes(x=glm.pred))+
  geom_histogram(fill='steelblue3')+
  geom_vline(xintercept=0.5,size=1.2)

# Computing Accuracy
table(as.numeric(glm.pred>0.5))
ct = table(Quarterly_data$Returns,glm.pred>0.5); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(Quarterly_data); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity


