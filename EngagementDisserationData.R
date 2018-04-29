##CFA on three constructs engagement,burnout and workaholism#
library(sem)
library(semPlot)
library(lavaan)
##three factors:workengagement burnout and workaholism using UWES-9 MBI-S WART##
mod2latent<-'fac1=~Vigor+Dedication+Absorption
fac2=~Exhaustion+Cynicism+Inability
fac3=~Compulsive+Control+Impaired+SelfWorth+NoDelegate'
fitcfa<-lavaan(mod2latent,data=shortWE)
summary(fitcfa)
parameterEstimates(fitcfa)
semPlot(fitcfa)

###Mokken on burnout work engagement##
library(mokken)

shor=as.matrix(weburnout[,8:25])
#return H coefficient#
coefH(shor)
#Returns a list (of class monotonicity.class) with results from the investigation of monotonicity
check.monotonicity(shor)

monotonicity.list <- check.monotonicity(shor)
plot(monotonicity.list)
summary(monotonicity.list)
#Automated Item Selection Procedure
aisp(shor)
#reliability
check.reliability(shor)

##DIF in item

res<-DIF[,(1:9)]
gender<-DIF$gender
library(lordif)
genderDIF<-lordif(res,gender,criterion="Chisqr",alpha=0.01,mincell=5)
res=as.matrix(res)
genderDIF<-lordif(res,gender,criterion="Chisqr",alpha=0.01)
plot(genderDIF,labels=c("Male","Female"))

###linear model 
library(lattice)
library(ggplot2)
linear.model<-lm(shortWE$we~shortWE$Exhaustion)
summary(linear.model)
plot(shortWE$we,shortWE$Exhaustion,pch=16,cex.lab=1.3,col="red")
abline(lm(shortWE$we~shortWE$Exhaustion),col="blue")
###
ggplot(shortWE, aes(x = shortWE$we, y = shortWE$Exhaustion, color = "red")) + 
  geom_point() + labs(x="workengagement",y="Exhaustion") + geom_smooth(method = loess)
ggplot(shortWE, aes(x = shortWE$we, y = shortWE$Exhaustion, color = "red")) + 
  geom_point() + labs(x="workengagement",y="Exhaustion") + geom_smooth(method = lm)

#quadratic model
We2<-shortWE$we^2
quadratic.model<-lm(shortWE$Exhaustion~shortWE$we+We2)
summary(quadratic.model)
ggplot(shortWE, aes(x = We2, y = shortWE$Exhaustion, color = "red")) + 
  geom_point() + labs(x="workengagement",y="Exhaustion") + geom_smooth(method = loess)




#clusterMclust
library(mclust)
#ward distance 
distances = round(dist(shortWE,method = "euclidean"),2)
distances
clust=hclust(distances,method="ward.D2")
plot(clust)
###completedendrogram
hh=hclust(dist(shortWE),"complete")
plot(hh)
id=identify(hh)
#bootstrapping hierarchical clustering with p-values
library("pvclust")
myclustdata<-shortWE[,c(121:126)]
fitclust<- pvclust(myclustdata, method.hclust="ward",method.dist="euclidean")
plot(fitclust,method="ward.D2")
#multiscaleboostrapping
result.par <- pvclust(myclustdata, nboot=1000, parallel=TRUE)
plot(result.par)
##mclust
library(mclust)
mclust2=na.omit(myclustdata)
fitclust2<-Mclust(mclust2)
plot(fitclust2)

####lazycluster for every subject
library(cluster)
clusplot(mycdata,fit$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
########clusteringFPC
library(fpc)
cluster.stats(myclustdata,fitclust$cluster)
cluster.stats(mydata,fit$cluster,fite$cluster)
wss <- (nrow(myclustdata))*sum(apply(myclustdata,2,var))

for (i in 1:15) wss[i] <- sum(kmeans(mclust2,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
