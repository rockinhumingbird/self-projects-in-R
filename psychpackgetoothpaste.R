toothpaste = read.csv('toothpaste.csv')
survey=toothpaste[,2:7]
head(survey)

round(cor(survey), 3)

library(corrplot)
corrplot(cor(toothpaste[,2:7]),type = 'lower',,col = c('red','white','green'),
         method = 'square',diag = F)

library(psych)
cortest.bartlett(cor(survey),n = 30)
# Bartlett's test is used to test if k samples are from populations with equal variances. Equal variances across populations is called homoscedasticity or homogeneity of variances. Some statistical tests, for example the analysis of variance, assume that variances are equal 
#across groups or samples. 

#KMO MEASURE OF SAMPLING ADEQUANCY
KMO(r = cor(survey))

##determine the numbers of factors
scree(cor(survey),factors = T,pc = F)

#eigen value
data.frame(factor = 1:ncol(survey), eigen = scree(cor(survey),factors = T,pc = F)$fv)
