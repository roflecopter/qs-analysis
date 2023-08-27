#set dir with this file as working dir
source("../Functions/Default.R"); 
source("../Functions/f.R")

library(jsonlite)

url <- 'https://blog.kto.to/uploads/'
cache.filename <- 'oura_data_2022-08-19T03-36-46.json'
rawoura <- fromJSON(paste0(url, cache.filename))

sleep<-flatten(data.frame(rawoura$sleep))

cols <- c("temperature_delta", "rmssd", "breath_average", "hr_average", "duration")
library(dplyr)
ouratemp <- sleep %>% select(all_of(cols))

df <- na.omit(ouratemp)
df$rmssd <- as.numeric(df$rmssd)
df$duration <- df$duration / 3600

df.ori <- df

set.seed(1)

#1 decide if features should be standardized: usually when units are differs
scale.data <- T; scale.text <- "Unscaled features"

if(scale.data) {
  scale.text <- "Scaled features"
  for(i in 1:length(cols)) {
    df[, cols[i]] <- scale(df[, cols[i]])
  }
}

# 2 decide dissimilarity measure: 
# 2.1 euclidean distance: focuses on magnitude
# 2.2 correlation: correlation between observations for 1-n variables, focuses on shapes of observation profile rather than magnitude 

dist.method <- "pearson"
#dist.method <- "euclidean"

library(factoextra)
df.dist <- get_dist(x = df, method = dist.method)

# 3 decide for linkage of dendrogram: complete & average often prefered over single.
hc.methods <- c("average", "complete") #, "single")

#4 decide where to cut dendrogram and how many clusters for KNN
n.clusters <- 3

#make a matrix for KNN
mx <- as.matrix(df)

km.out <- kmeans (mx, n.clusters, nstart = 50)
km.out$tot.withinss

for(i in 1:length(hc.methods)) {
  hc.final <- hclust(df.dist, method = hc.methods[i])
  hc.clusters <- cutree(hc.final, n.clusters); table(hc.clusters)
}

df.ori$knn.cluster <- km.out$cluster
df.ori$hc.cluster <- hc.clusters

for(i in 1:n.clusters) {
  print(paste0("Cluster #", i, ", KNN"))
  print(describe.data(df.ori[df.ori$knn.cluster == i, cols]))
}

for(i in 1:n.clusters) {
  print(paste0("Cluster #", i, ", HC"))
  print(describe.data(df.ori[df.ori$hc.cluster == i, cols]))
}

df.r.hc <- aggregate(cbind(df.ori[, cols[1:length(cols)]]), by = list(cluster = df.ori$hc.cluster), FUN = median)
df.r.hc <- df.r.hc[order(df.r.hc$temperature_delta, decreasing = T), ]
df.r.hc$cluster <- NULL

par(mfrow = c(1, 2))
radar.graph(df.r.hc, length(cols), "HC Clusters")

df.r.knn <- aggregate(cbind(df.ori[, cols[1:length(cols)]]), by = list(cluster = df.ori$knn.cluster), FUN = median)
df.r.knn <- df.r.knn[order(df.r.knn$temperature_delta, decreasing = T), ]
df.r.knn$cluster <- NULL
radar.graph(df.r.knn, length(cols), "KNN Clusters")
