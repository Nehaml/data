#K-Means Clustering Project
rm(list = ls())
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)


#Building an analytical model to create clusters of airline travellers
Path<-"desktop/neha/data"
getwd()
airlines<-read.csv("AirlinesCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))

str(airlines)
summary(airlines)

as.data.frame(colSums(is.na(airlines)))

#Normalizing the Data for clustering 
library(caret)
preproc<-preProcess(airlines)
airlinesNorm<-predict(preproc,airlines)
summary(airlinesNorm)


#Hiearchical Clustering
distan<-dist(airlinesNorm, method = "euclidean")
ClusterAirline<-hclust(distan, method = "ward.D")
plot(ClusterAirline)

#Assigning points to the clusters
AirlineCluster<-cutree(ClusterAirline, k = 5)
table(AirlineCluster)


AirlineCluster1<-cutree(ClusterAirline, k = 4)
table(AirlineCluster1)

MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)#
  print(z)
}

Bal_mean<-MeanComp(airlines$Balance, AirlineCluster, mean)
Bal_mean<-MeanComp(airlines$Balance, AirlineCluster1, mean)
Bal_DaysSinceEnroll<-MeanComp(airlines$DaysSinceEnroll, AirlineCluster, mean)

#Appending the Clusters Assignment
Airlines_H<-data.frame(airlines,AirlineCluster)
write.csv(Airlines_H,"Airlines_Hierarchical.csv", row.names = FALSE)

set.seed(88)

#Finding and Visualizing out the various clusters
AirlineCluster_K1<-kmeans(airlinesNorm, centers = 4,iter.max = 1000)
AirlineCluster_K2<-kmeans(airlinesNorm, centers = 5,iter.max = 1000)
AirlineCluster_K3<-kmeans(airlinesNorm, centers = 6,iter.max = 1000)
AirlineCluster_K4<-kmeans(airlinesNorm, centers = 7,iter.max = 1000)

p1 <- fviz_cluster(AirlineCluster_K1, geom = "point", data = airlinesNorm) + ggtitle("k = 4")
p2 <- fviz_cluster(AirlineCluster_K2, geom = "point",  data = airlinesNorm) + ggtitle("k = 5")
p3 <- fviz_cluster(AirlineCluster_K3, geom = "point",  data = airlinesNorm) + ggtitle("k = 6")
p4 <- fviz_cluster(AirlineCluster_K4, geom = "point",  data = airlinesNorm) + ggtitle("k = 7")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Determing optimal numbers of clusters using the Elbow Method
set.seed(123)
fviz_nbclust(airlinesNorm, kmeans, method = "wss")

k<-6
AirlineCluster_K<-kmeans(airlinesNorm, centers = k,iter.max = 1000)
AirlineCluster_K

table(AirlineCluster_K$cluster)
AirlineCluster_K$centers
fviz_cluster(AirlineCluster_K, data = airlinesNorm)


#Finding out the Mean Values of the Variables in the Clusters
Bal_mean_k<-aggregate(airlines, by=list(cluster=AirlineCluster_K$cluster), mean)
Bal_mean<-MeanComp(airlines$Balance, AirlineCluster_K$cluster, mean)


#Appending the Clusters Assignment
airlines_new_k <- data.frame(airlines, AirlineCluster_K$cluster)
write.csv(airlines_new_k,"Airlines_k-Means.csv", row.names = FALSE)
write.csv(Bal_mean_k,"Airlines_k-Means_Summary.csv", row.names = FALSE)

