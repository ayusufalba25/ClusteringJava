# ============================= PROJECT ===============================
# Clustering Districts/Cities in Java Island of Indonesia
# Based on its Education Infrastructure (Schools - Formal and Informal)
# =====================================================================

# Created by: 
# Ahmad Yusuf Albadri
# Aspiring Data Scientist | Data Enthusiast
# Bogor, West Java, Indonesia

# Portfolio Website:
# https://ayusufalba25.github.io/ahmadyusufalbadri.github.io/

# =============================================
#   Here's the code script and its analysis!
# =============================================

# Import library
library(tidyverse)
library(factoextra)
library(readxl)
library(skimr)

# Import data
edu <- read_xlsx("Data/educational data.xlsx")

# View the data
View(edu)

# Check the data type of each column in the edu data set
glimpse(edu)

# ================ INFORMATION ================
# All of the variable description is available in the 'variable name.xls' file.
# =============================================

# ================== INSIGHT ==================
# 1. There's a column named '...1' that generated automatically when we import
# the data because there is no name for that particular column in default.
# You can check it by opening the data in MS Excel.
# 2. The data contains instances for each village/urban village in each province
# in the java island of Indonesia.
# 3. As you can see that there are villages/urban villages that have 0 value
# in several columns in the data. Because of that, we need to consider which
# instances that could give enough information for clustering. I choose to use
# 'R102N' for districts/cities in the clustering analysis because it's more
# informative and we could give recommendation based on the distribution of cluster
# for each district/city in each province in the java island of Indonesia.
# 4. We just need columns that represent the educational infrastructure
# which is schools. Next, we will slice the data for the required columns.
# =============================================

# Create a vector for the required columns
edu_col <- c("R101N", "R102N",
              "R701CK2", "R701CK3", "R701DK2", "R701DK3", "R701EK2", "R701EK3", 
              "R701FK2", "R701FK3", "R701GK2", "R701GK3", "R701HK3", "R701HK3",
              "R701IK2", "R701IK3", "R701JK2", "R701JK3", "R701KK3", "R701LK3",
              "R703AK2", "R703AK3", "R703BK2", "R703BK3", "R703CK2", "R703CK3",
              "R703DK2", "R703DK3", "R703EK2", "R703EK3", "R703FK2", "R703FK3",
              "R703GK2", "R703GK3")

# Slicing the data for the required columns and group it by the districts/cities
edu <- edu %>% 
  select(!!edu_col) %>% 
  group_by(R101N, R102N) %>% 
  summarise(across(everything(), sum)) %>% 
  as_tibble()

View(edu)

# Create a variable to accommodate the educational representatives
num_dat <- edu %>% select(-c(R101N, R102N))

# Standardize the data
num_dat <- scale(num_dat)

num_dat

# Visualize the data using principal component analysis for 2D graph
pr_dat <- prcomp(num_dat, center = F) # We don't need to center the data because it's already centered

# Create a biplot
biplot(pr_dat, scale = 0)

# Variance explained by each principal components
pr_var <- pr_dat$sdev^2
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

# ================== INSIGHT ==================
# 1. From the Proportion of Variance Explained (PVE), We can use the first two
# principal components to plot the data in 2D space because it has the highest
# PVE than the other principal components.
# =============================================

round(cumsum(pve) * 100, 2) # It's 53.67% Cummulative PVE from the first two components

plot(pr_dat$x[, 1], pr_dat$x[, 2],
     xlab = paste0("PC1 ", "(", round(pve[1] * 100, 2), "%)"),
     ylab = paste0("PC2 ", "(", round(pve[2] * 100, 2), "%)"))

# ================== INSIGHT ==================
# 1. From the pca plot we cannot see any clear pattern for the cluster.
# But, we can see that there are several data points that located around
# the center of the plot. Let's see how K-mean clustering perform for
# this kind of pattern in the data.
# =============================================

# Finding the "best" number of clusters
# 1. Find the number of clusters that could "effectively" minimize
# the total within sum of square
fviz_nbclust(num_dat, kmeans, method = "wss") # From this plot, two clusters is enough
# 2. Using silhouette technique
fviz_nbclust(num_dat, kmeans, method = "silhouette") # Two clusters is the optimal

# Using K=2 for K-mean clustering
clust_result <- kmeans(num_dat, 2, nstart = 50)
clust_result

# Adding the clusters in the data
edu$clust <- clust_result$cluster
View(edu)

# Plot the clusters in a pca plot
fviz_cluster(clust_result, geom = "point", data = num_dat) + ggtitle("K = 2")

# Check the first cluster
clust1_data <- edu %>% filter(clust == 1)
View(clust1_data)
skim(clust1_data)

nrow(clust1_data) # There are 85 districts/cities in the first cluster

# Check the second cluster
clust2_data <- edu %>% filter(clust == 2)
View(clust2_data)
skim(clust2_data)

nrow(clust2_data) # There are 15 districts/cities in the second cluster

# Create a bar chart to compare the two clusters characteristics for each variable
edu$clust <- as.factor(edu$clust)

edu_plot <- edu[3:ncol(edu)] %>% 
  group_by(clust) %>% 
  summarise(across(everything(), mean))

cluster <-  append(rep(1, ncol(edu_plot) - 1), rep(2, ncol(edu_plot) - 1))

value <-  append(unlist(edu_plot[1, 2:ncol(edu_plot)]), unlist(edu_plot[2, 2:ncol(edu_plot)]))

var_name <- rep(names(edu_plot)[2:ncol(edu_plot)], 2)

edu_plot <- as_tibble(cbind(cluster, var_name, value))

ggplot(edu_plot, aes(fill = cluster, y = value, x = var_name)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

# Create a horizontal bar chart to visualize the different
difference <- round((unlist(edu_plot[1, 2:ncol(edu_plot)]) - unlist(edu_plot[2, 2:ncol(edu_plot)])) / unlist(edu_plot[1, 2:ncol(edu_plot)]), 2)

diff_plot <- as_tibble(cbind(var_name = names(difference),
                             difference))

diff_plot$difference <- as.numeric(diff_plot$difference)

ggplot(diff_plot, aes(y = difference, x = var_name, fill = difference > 0)) +
  geom_col() +
  coord_flip()
