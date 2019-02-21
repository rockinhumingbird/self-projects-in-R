# How many variable in the dataset
library(ggplot2);library(data.table);library(mice)
fastfood = fread('ff.csv')
ncol(fastfood)
# subset first 11 col and see the variables in datacluster
data_cluster<-fastfood[,1:11]
ncol(data_cluster)
# check missing values
table(is.na(data_cluster$cleanliness))
#How many rows left 
nrow(na.omit(data_cluster))

#impute missing vales
library(mice)
md.pattern(data_cluster)
set.seed(1706)
data_cluster = complete(mice(data_cluster))
data_cluster$cleanliness[10]

##scaled data and check clenaningess 10th
scaled<-scale(data_cluster)
scaled[10,4]
#######section 2: clustering technique
#computer Euclidean distance on all obs
distances = dist(scaled,method = 'euclidean')
length(distances)
#q2: HCLUST cophenetic correlation coef
clusters <- hclust(distances,method = 'ward.D2')
# cophen distance
d2 <- cophenetic(clusters)
cor(d2,distances)
#dendrogram
plot(hclust(d2))
plot(hclust(distances))
library(dendextend)
plot(color_branches(as.dendrogram(clusters),groupLabels = F))
#two cluster solution, howmany obs on te smaler
clusterGroups = cutree(clusters,k=2)
table(clusterGroups)#output smaller cluster membership
#three cluster solution how many obs on the smaller side
clusterGroups = cutree(clusters,k=3)
# k-means 2 cluster solution
set.seed(1706)
km = kmeans(x = scaled,centers = 2,iter.max=100,nstart=100)
km$centers
km$size

mean(km$cluster==clusterGroups) 
# %match between results of hclust and kmeans
km = kmeans(x = scaled,centers = 3,iter.max=100,nstart=100)
#8 ttwss
set.seed(1706)
ss=sapply(X= 2:10,FUN = function(i)
{kmeans(x= scaled,centers = 3,iter.max = 100)})
ss
#ratio between ss / tss
ans= 3098.038/6831

within_ss = sapply(2:10,FUN = function(x) 
  kmeans(x = scaled,centers = x,iter.max = 100,nstart = 100)
  $tot.withinss)
ggplot(data=data.frame(cluster = 2:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#For the three-cluster solution, what is the ratio of between sum of squares and total sum of squares?
ratio_ss = sapply(1:10,FUN = function(x) 
  {km = kmeans(x = scaled,centers = x,
               iter.max = 1000,nstart = 25)
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))


#####q11 Silhouette Plot
library(cluster)
#2cluster solution avg width
pam(x = scaled,k = 2)$silinfo$avg.width #0.5936
#3cluster solution avg width
pam(x = scaled,k = 3)$silinfo$avg.width#0.17

#model based clustering
library(mclust)
Mclust(scaled)
m2<-Mclust(scaled,2)
table(m2$classification)

#km$size-clusterGroups
#km$size-m2$classifciation 579-451

###section 3 
# k-means 3 cluster solution
set.seed(1706)
km3 = kmeans(x = scaled,centers = 3,iter.max=100,nstart=100)
new<-cbind(km3$cluster,fastfood)
km3$centers
kseg<-km3$cluster
table(kseg)
#visualize
library(psych)
temp = data.frame(cluster = factor(kseg),
                  factor1 = fa(new[,2:12],nfactors = 3,rotate = 'varimax')$scores[,1],
                  factor2 = fa(new[,2:12],nfactors = 3,rotate = 'varimax')$scores[,2],
                  factor3 = fa(new[,2:12],nfactors = 3,rotate = 'varimax')$scores[,3]
)
temp = data.frame(cluster = factor(kseg),
                  factor1 = fa(new[,13:22],nfactors = 3,rotate = 'varimax')$scores[,1],
                  factor2 = fa(new[,13:22],nfactors = 3,rotate = 'varimax')$scores[,2],
                  factor3 = fa(new[,13:22],nfactors = 3,rotate = 'varimax')$scores[,3]
)
library(ggplot2)                  
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()
library(dplyr)
new[,1:12] %>%
  group_by(kseg)%>%
  summarize_all(function(x) round(mean(x,na.rm=T,sort=TRUE),2))%>%
  data.frame()

table(new$V1,new[,14:22])

library(flexclust)
km_kcca = as.kcca(km,scaled) # flexclust uses objects of the classes kcca
clusterTrain = predict(km,scaled)
clusterTest = predict(km_kcca,newdata=testNorm)


