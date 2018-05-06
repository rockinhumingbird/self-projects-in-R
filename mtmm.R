library(lavaan)
library(psy)
library(multicon)
library(gesca)
#create a correlation matrix and associated data
sig <- matrix(c(1.00,.4,.6,.05,.1,-.05,.4,1.00,.5,.08,
                -.02,.03,.6,.5,1.00,.09,.1,-.07,.05,.08,.09,1.00,.6,.7,.1,-.02,.1,.6,
                1.00,.5,-.05,.03,-.07,.7,.5,1.00), ncol=6, byrow=TRUE)
rownames(sig)<-colnames(sig)<-c('t1m1','t1m2','t1m3','t2m1','t2m2','t2m3')
round(sig,2)

library(mvtnorm)
library(qgraph)
#generate correlation graph
qgraph(sigma)

library(mvtnorm)
# Now create random data based on this correlation matrix
d <- rmvnorm(100, sigma=sig)
d<-as.data.frame(d)
names(d)<-colnames(sig)
round(cor(d),2)

#Now use MTMM on this data.frame indicating that there are 2 traits and 3 methods. 
library(multicon)
MTMM(d, 2, 3)
library(psych)
iclust(d)
#do a principal components exploratory analysis
fa.sort(principal(d,2))
rownames(sig)<-colnames(sig)<-c('t1m1','t1m2','t1m3','t2m1','t2m2','t2m3')
iclust(sig)
iclust(sig,2)
#Do a cfa solving for two trait factors
mtmod<- 't1 =~ t1m1 + t1m2 + t1m3
          t2 =~ t2m1 + t2m2 + t2m3'
mmtrait<-cfa(mtmod,d,std.ov=TRUE)
summary(mmtrait)
standardizedsolution(mmtrait)
fitmeasures(mmtrait)
# Do a cfa solving for three trait factors
methmod<-' m1 =~ t1m1 + t2m1
          m2 =~  t1m2 + t2m2 
           m3 =~ t1m3 + t2m3'
methfac<-cfa(methmod,d)
summary(methfac)   
fitmeasures(methfac)  

# Do these in GESCA (Generalized Structural Components Analysis)
mmtraitg<-gesca.run(mtmod,d)
summary(mmtraitg)

qualmeasures(mmtraitg)
effectmeasures(mmtraitg)
latentmeasures(mmtraitg)


#
methg<-gesca.run(methmod,d)
summary(methg)

#
# Try a correlated uniqueness CFA  model

cumod<- 't1 =~ t1m1 + t1m2 + t1m3
          t2 =~ t2m1 + t2m2 + t2m3
           t1m1 ~~ t2m1
            t1m2 ~~ t2m2
            t1m3 ~~ t2m3'
corun<-cfa(cumod,d,std.ov=TRUE)
summary(corun)
