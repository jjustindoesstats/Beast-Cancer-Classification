breastc<-read.csv("/Users/user/Desktop/breastcancer.csv")
breastc
head(breastc)

library(class)

#scale the data
breastscale<-scale(breastc[,-c(1,2)])
breastscale

head(breastscale)

#Data visaulization


#K-Nearest Neighbors
#create a training set of 805 of the observations
set.seed(1)
trainfunction <- sample(c(1:nrow(x)),floor(0.8*nrow(x)))
trainset <- breastscale[trainfunction,]

#test set which will consist of all the other observations
test<-breastscale[-trainfunction,]

#try KNN with multiple options of k
# KNN by region
neighbours<-1

breastcancerKNN <- knn(trainset, test, breastc[trainfunction,2], neighbours)

table(breastc[-trainfunction,2],breastcancerKNN)

#Clustering
#hierarchical clustering
#try multiple linkage types
hier<-hclust(dist(breastscale),"single")
plot(hier)

hier<-hclust(dist(breastscale),"single")
plot(hier)
plot(hier,hang=-1)

hier2<-hclust(dist(breastscale),"complete")
plot(hier2)
plot(hier2,hang=-1)

hier3<-hclust(dist(breastscale),"ward.D2")
plot(hier3)
plot(hier3,hang=-1)


hier4<-hclust(dist(breastscale),"ward.D")
plot(hier4)
plot(hier4,hang=-1)
#side by side comparison of complete and ward.D2

opar <- par(mfrow = c(1, 2))
plot(hier3,hang=-1)
plot(hier2,hang=-1)
par(opar)


opar <- par(mfrow = c(1, 2))
plot(hier3,hang=-1)
plot(hier2,hang=-1)
par(opar)

opar2<-par(mfrow=c(1,2))
plot(hier3,hang=-1)
plot(hier4,hang=-1)
par(opar2)

# cut tree into two since thats the number of classes

D2<-cutree(hier3,2)
D2
table(breastc[,2],D2)

#k-means clustering
library(cluster)
breastscale_k2means<-kmeans(breastscale,2)
breastscale_k2means$cluster


sil2 <- silhouette(breastscale_k2means$cluster, dist(breastscale))
summary(sil2) 

plot(sil2)

#test for other values of k
breastscale_k3means<-kmeans(breastscale,3)
breastscale_k3means$cluster
sil3 <- silhouette(breastscale_k3means$cluster, dist(breastscale))
summary(sil3) 

plot(sil3)

breastscale_k4means<-kmeans(breastscale,4)
breastscale_k4means$cluster
sil4 <- silhouette(breastscale_k4means$cluster, dist(breastscale))
summary(sil4) 
plot(sil4)
#optimal value is 2

plot(breastscale, col = breastscale_k2means$cluster)

table(breastc[,2],breastscale_k2means$cluster)

plot(sil4)
#k-medoids clustering
breastk2medoids<-pam(breastscale,2)
breastk2medoids

breastk2medoids$clustering

sill2<-silhouette(breastk2medoids$clustering,dist(breastscale))
plot(sill2)
summary(sill2)


breastk3medoids<-pam(breastscale,3)
breastk3medoids

breastk3medoids$clustering

sill3<-silhouette(breastk3medoids$clustering,dist(breastscale))
plot(sill3)
summary(sill3)

breastk4medoids<-pam(breastscale,4)
breastk4medoids

breastk4medoids$clustering

sill4<-silhouette(breastk4medoids$clustering,dist(breastscale))
plot(sill4)
summary(sill4)

table(breastc[,2],breastk2medoids$clustering)

