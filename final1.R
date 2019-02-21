data <- read.csv('diamonds.csv')


library(dplyr)
library(plyr)
library(ggplot2)
library(ISLR)
library(caTools)
library(MLmetrics)
library(ROCR)
library(caret)
library(tidyr)
library(corrplot)
library(car)
library(leaps)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(mi)
library(missForest)
library(mice)
library(e1071)

#Q1 - diamonds_exam_Sec03_version_B
#Q2

set.seed(200)
split = sample(1:nrow(data), nrow(data)*0.75)
train = data[split,]
test = data[-split,]
nrow(train)

#Q3
class(train$depth)

#Q4
mean(train[train$color=='J',]$price)
aggregate(train$price, by=list(train$color), FUN=mean)

#Q5
max(train[train$cut=='Fair',]$price)
aggregate(train$price, by=list(train$cut), FUN=max)


#Q6 - double check
mean(train[train$carat>1 & train$cut=='Ideal',]$price)


#Q7
plot(train$carat, train$price, main="Scatterplot Diamonds", xlab="Carat", ylab="Price ", pch=20)

#Q8
cor(train$table, train$x)

#Q9
model1 <- lm(price~depth, data=train)
summary(model1)

#Q10
#YES

#Q11
#For a 1 unit increase in depth, price goes down by $33.06

#Q12 - double check how R references linear regression
model2 <- lm(price~cut, data=train)
summary(model2)

#Q13
mean(train[train$cut=='Ideal',]$price)
mean(train[train$cut=='Fair',]$price)

#Q14
str(train)
model3 <- lm(price~carat+cut+color+clarity+depth+table, data=train)
summary(model3)

#Q15
#ALL

#Q16
predict(model3)[1]

#Q17
pred <- predict(model3,newdata=test)
RMSE(pred, test$price)
rmse_test = sqrt(mean((pred - test$price)^2))
rmse_test


#Q18
model4 <- lm(price~carat+depth+table+x+y+z, data=train)
vif(model4)
which.max(vif(model4))





#Q19
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~carat+depth+table+x+y+z,data=train)
forwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='forward')
summary(forwardStepwise)

#Q20
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~carat+depth+table+x+y+z,data=train)
hybridStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
summary(hybridStepwise)

#Q21
model5 = glm(price_hilo~carat+cut+color+clarity+depth+table, data = train, family ='binomial')
summary(model5)

#Q22 - Counter check
(exp(model5$coef['cutGood'])-1)*100

#Q23 
pred = predict(model5, newdata = test)
table(pred>0.4, test$price_hilo)
mean(as.numeric(pred>0.4)==test$price_hilo)


#Q24
model6 <- rpart(price_hilo~carat+cut+color+clarity+depth+table, train, method = 'class')
rpart.plot(model6)


#Q25
#1%

#Q26
#94%

#27
model7= randomForest(factor(price_hilo)~carat+cut+color+clarity+depth+table,train, ntree = 100)
varImpPlot(model7)
importance(model7)

#28
pred=predict(model7,newdata=test,type='prob')[,2]
ROCRpred = prediction(predictions = pred, labels = test$price_hilo)
as.numeric(performance(prediction.obj = ROCRpred, measure = 'auc')@y.values)
plot(performance(prediction.obj = ROCRpred,'tpr','fpr'))



ROCRpred = prediction(pred,test$price_hilo)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

#29

model8= svm(factor(price_hilo)~carat+cut+color+clarity+depth+table, train,
             type = 'C-classification',kernel = 'radial')
pred = predict(model8, newdata = test)
table(actual=test$price_hilo, predicted = pred)



#30
ct = table(actual=test$price_hilo, predicted = pred)
ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy
