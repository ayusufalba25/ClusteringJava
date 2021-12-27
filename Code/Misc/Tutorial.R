# Tutorial
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

# US Arrest data
df <- USArrests
View(df)
str(df)
summary(df)

# Check the number of rows with missing values
sum(is.na(df))

# Standard scaling
df <- scale(df)

# Create a heat map for dissimilarities
# red: large dissimilarities; blue: fairly similar
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Computing cluster for 2 clusters
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

# Visualizing cluster
fviz_cluster(k2, data = df)

# Visualize using the data
df %>% 
  as_tibble() %>% 
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>% 
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

# Visualizing other k cluster
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Manual Elbow Method
set.seed(123)

wss <- function(k){
  kmeans(df, k, nstart = 25)$tot.withinss
}

k_val <- 1:15

wss_val <- map_dbl(k_val, wss)

plot(k_val, wss_val,
     type = "b", pch = 19, frame = F,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")

# Elbow method with the function from factoextra library
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

# Compute silhouette for k clusters
