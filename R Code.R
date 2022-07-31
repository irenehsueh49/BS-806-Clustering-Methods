---
title: "Irene Hsueh's BS 806 Homework 10"
author: "Irene Hsueh"
date: "11/12/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(Heatplus)
set.seed(1234)
```

### Simulating Sample
```{r}
#Sample of 300 observations with 4 variables from 3 clusters
cluster1_x1 <- rnorm(n=100, mean=0, sd=1)
cluster1_x2 <- rnorm(n=100, mean=2.5, sd=1)
cluster1_x3 <- rnorm(n=100, mean=-2.5, sd=1)
cluster1_x4 <- rnorm(n=100, mean=1, sd=1)

cluster2_x1 <- rnorm(n=100, mean=-2.5, sd=1)
cluster2_x2 <- rnorm(n=100, mean=0, sd=1)
cluster2_x3 <- rnorm(n=100, mean=1, sd=1)
cluster2_x4 <- rnorm(n=100, mean=2.5, sd=1)

cluster3_x1 <- rnorm(n=100, mean=2.5, sd=1)
cluster3_x2 <- rnorm(n=100, mean=-2.5, sd=1)
cluster3_x3 <- rnorm(n=100, mean=1, sd=1)
cluster3_x4 <- rnorm(n=100, mean=2.5, sd=1)

cluster1 <- cbind(cluster1_x1, cluster1_x2, cluster1_x3, cluster1_x4)
cluster2 <- cbind(cluster2_x1, cluster2_x2, cluster2_x3, cluster2_x4)
cluster3 <- cbind(cluster3_x1, cluster3_x2, cluster3_x3, cluster3_x4)

simulation <- rbind(cluster1, cluster2, cluster3)
head(simulation)

#Summary Statistics
summary(simulation)
sigma <- sqrt(apply(simulation, 2, var))
```



### Principal Component Analysis
```{r}
#Scaled PCA
pca <- prcomp(simulation, scale=TRUE)

#Standard Deviations and Loading Factors
pca

#Scree Plot
summary(pca)
plot(pca, col="hotpink")

#First 10 Principal Components
pca$x[1:10,]

#Plots of first Two Principal Components
plot(pca$x[,1:2], col="hotpink")
biplot(pca, col=c("hotpink", "black"))
```



### K-Means Clustering
```{r}
#2 Clusters
kmeans_2 <- kmeans(simulation, 2)

#3 Clusters
kmeans_3 <- kmeans(simulation, 3)

#4 Clusters
kmeans_4 <- kmeans(simulation, 4)

#5 Clusters
kmeans_5 <- kmeans(simulation, 5)

#Plot of WSS for Clusters
wss <- c()
for(i in 1:10){
  km <- kmeans(simulation, i)
  wss <- c(wss, km$tot.withinss)}
plot(c(1:10), wss, type="l", col="hotpink")
axis(side=1, at=1:10)
```



### Visualizing Clusters
```{r}
#Visualizing 2 Clusters
plot(pca$x[,1:2])
for(i in 1:2){
  points(pca$x[which(kmeans_2$cluster == i),1],
         pca$x[which(kmeans_2$cluster == i),2],
         col=c("cyan2", "hotpink")[i])
  }

#Visualizing 3 Clusters
plot(pca$x[,1:2])
for(i in 1:3){
  points(pca$x[which(kmeans_3$cluster == i),1],
         pca$x[which(kmeans_3$cluster == i),2],
         col=c("hotpink", "cyan2", "springgreen")[i])
  }

#Visualizing 4 Clusters
plot(pca$x[,1:2])
for(i in 1:4){
  points(pca$x[which(kmeans_4$cluster == i),1],
         pca$x[which(kmeans_4$cluster == i),2],
         col=c("springgreen", "hotpink", "cyan2", "mediumpurple")[i])
  }

#Visualizing 5 Clusters
plot(pca$x[,1:2])
for(i in 1:5){
  points(pca$x[which(kmeans_5$cluster == i),1],
         pca$x[which(kmeans_5$cluster == i),2],
         col=c("sienna1", "cyan2", "springgreen", "mediumpurple", "hotpink")[i])
  }
```



### Hierarchical Clustering
```{r}
#Visualizing 2 Clusters
hierarchical_cluster <- hclust(dist(simulation))

plot(hierarchical_cluster, col="hotpink")
rect.hclust(hierarchical_cluster, k=2)
```



### Resampling to Detect Significant Clusters
```{r}
permutations <- c()
random_hierarchical_tree <- c()

for(i in 1:10){
  test <- sample(simulation[,1]) 
  for(index in 2:ncol(simulation))
    {test <- cbind(test, sample((simulation[,index])))}
  random_tree <- hclust(dist(test))
  random_hierarchical_tree <- cbind(random_hierarchical_tree, random_tree[[2]])
  permutations <- rbind(permutations, quantile(random_tree[[2]], probs=0.99))
  }
head(permutations)

random_tree_reference <- apply(random_hierarchical_tree, 1, mean)
threshold <- apply(permutations, 2, mean)

#Comparing QQ-Plot of Real Data Clustering vs Reference Data Clustering 
real_tree <- hclust(dist((simulation)))
plot(random_tree_reference,
     real_tree[[2]], 
     xlab="Expected", 
     ylab="Observed", 
     col="hotpink"
     ) 
abline(0,1)

#Cutting Dendrogram at Thresholds to Detect Number of Clusters
hierarchical_cluster <- hclust(dist(simulation))
plot(hierarchical_cluster, col="hotpink")
rect.hclust(hierarchical_cluster, h=threshold)
title(sub="1% Significance", line=2)
```



### Plotting Heatmap
```{r}
heatmap <- annHeatmap2(t(simulation), 
                       annotation=NULL, 
                       cluster=list(cuth=threshold),
                       legend=2)
plot(heatmap, cex=0.75, cex.lab=0.75, cex.axis=0.75) 
```



### Comparing K-Means and Hierarchical Clustering
```{r}
#Comparing 2 Clusters
clustering_labels_2 <- cutree(hierarchical_cluster, k=2)
table(kmeans_2$cluster, clustering_labels_2)

#Comparing 3 Clusters
clustering_labels_3 <- cutree(hierarchical_cluster, k=3)
table(kmeans_3$cluster, clustering_labels_3)
```

