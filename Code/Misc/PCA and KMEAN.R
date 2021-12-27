# Principal Component Analysis
# Introduction to Statistical Learning 2nd Edition
library(ISLR2)
library(tidyverse)

# Access the USArrests data
data("USArrests")
View(USArrests)

# Extract the states name
states <- row.names(USArrests)
states

# Columns
names(USArrests)

# Summary of the data
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
# There is a relatively different scale of the data's variance
summary(USArrests)
# Because of that, it is important to standardize the variables to have mean zero
# and standard deviation one before performing PCA

# Calculate the principal components
pr_out <- prcomp(USArrests, scale. = T)
names(pr_out) # There are 4 output that contains in the pr_out from the prcomp() function

pr_out$center
pr_out$scale

pr_out$rotation # Each column contains the corresponding principal component loading vector

pr_out$x # Principal component scores
dim(pr_out$x)

# Create a biplot
biplot(pr_out, scale = 0)

# Create a mirroring for the biplot
pr_out$rotation <- -pr_out$rotation
pr_out$x <- -pr_out$x
biplot(pr_out, scale = 0)

# Variance explained by each principal components
pr_var <- pr_out$sdev^2
# Proportion of variance explained
pve <- pr_var / sum(pr_var)
pve

# Plot the PVE and its cummulative
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cummulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Matrix Completion
X <- data.matrix(scale(USArrests))
pcob <- prcomp(X)
summary(pcob)

# Singular value decomposition (SVD) can be used to solve the optimization problem
sX <- svd(X)
names(sX)
round(sX$v, 3)
# The matrix v is equivalent to the loading matrix from principal components
# (up to an unimportant sign flip)
round(pcob$rotation, 3)

# The matrix u is equivalent to the matrix of standardized scores, and the
# standard deviations are in the vector d
t(sX$d * t(sX$u))
pcob$x

# Simulate missing data
nomit <- 20
set.seed(15)
ina <- sample(seq(50), nomit)
inb <- sample(1:4, nomit, replace = T)
Xna <- X
index.na <- cbind(ina, inb)
Xna[index.na] <- NA

# Create an svd function to solve the optimization problem
fit_svd <- function(X, M = 1){
  svdob <- svd(X)
  with(svdob, u[, 1:M, drop = F] %*% (d[1:M] * t(v[, 1:M, drop = F])))
}
# with() function used to make it easier to index the element of svdob
# svdob$u[, 1:M, drop = FALSE] %*%
#   (svdob$d[1:M]*t(svdob$v[, 1:M, drop = FALSE]))

# Create an Xhat for the first step in the algorithm for the matrix completion
Xhat <- Xna
xbar <- colMeans(Xna, na.rm = T)
Xhat[index.na] <- xbar[inb]

# Set up measures of the progress of the iterations of the algorithm
thresh <- 1e-7
rel_err <- 1
iter <- 0
ismiss <- is.na(Xna)
mssold <- mean((scale(Xna, xbar, F))[!ismiss]^2)
mss0 <- mean(Xna[!ismiss]^2)

# Step 2 of algorithm
while(rel_err > thresh){
  iter <- iter + 1
  # step 2(a)
  Xapp <- fit_svd(Xhat, M = 1)
  # step 2(b)
  Xhat[ismiss] <- Xapp[ismiss]
  # step 2(c)
  mss <- mean(((Xna - Xapp)[!ismiss])^2)
  rel_err <- (mssold - mss) / mss0
  mssold <- mss
  cat("Iter:", iter, "MSS:", mss, "Rel. Err:", rel_err, "\n")
}

# Compute the correlation between the 20 imputed values and the actual values
cor(Xapp[ismiss], X[ismiss])


# K-Mean Clustering
# Create a simulated data
set.seed(2)
X <- matrix(rnorm(50 * 2), ncol = 2)
X[1:25, 1] <- X[1:25, 1] + 3
X[1:25, 2] <- X[1:25, 1] - 4
# Perform K-means clustering with k = 2
km_out <- kmeans(X, 2, nstart = 20) # It is recommended to use nstart > 1 (20, 50)
km_out$cluster
km_out$withinss
km_out$tot.withinss
sum(km_out$withinss)
# plot the cluster
plot(X, col = (km_out$cluster + 1),
     main = "K-Means Clustering Results with K = 2",
     xlab = "", ylab = "", pch = 20, cex = 2)
