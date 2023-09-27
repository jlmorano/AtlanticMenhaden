# Cluster-menhaden.R
######################################
# Janelle L. Morano

# Objectives: K means cluster.

# Using predicted density data from VAST menhaden model

# last updated 6 July 2023
###############################################
###############################################

library(dplyr)


#----- Load predicted density data
men.sp <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20230111 Spring/PredDensity_spring_1972-2021.csv", header = TRUE)

# Filter 1 year, 2019, to start
men.sp.2019 <- filter(men.sp, Year == 2019)
# Eliminate absences
men.sp.2019 <- filter(men.sp.2019, D >0)
# Matrix of relevant data
m <- select(men.sp.2019, c(D, Lat, Lon))

library(ecodist)
d0 <- ecodist::bcdist(m)


d <- dist(d0)

# b) hclust() function, generate 4 different dendrograms based on the distance matrix, using four different methods for clustering groups with more than one member (“centroid”, "single linkage”, “complete linkage”, and “average”).
h.centroid <- hclust(d, "centroid")
h.single <- hclust(d, "single")
h.complete <- hclust(d, "complete")
h.average <- hclust(d, "average")

par(mfrow = c(2,2))
plot(h.centroid)
plot(h.single)
plot(h.complete)
plot(h.average)
par(mfrow = c(1,1))


# d) Using the complete-linkage dendrogram and the cutree function(), cut the tree to allocate different observations to two different clusters, #row id
cluster <- cutree(h.complete, k = 3) #row id
obs <- data.frame(cbind(cluster, men.sp.2019))

