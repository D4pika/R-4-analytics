################################################################################
# Hierarchical & k-Means Clustering 
# Matthew A. Lanham
################################################################################
library(cluster)
#?agnes
data <- c(2,5,9,15,16,18,25,33,33,45)

par(mfrow=c(1,3))
# single-linkage clustering
agn <- agnes(data, diss=F, stand=F, metric="euclidean", method="single")
# make and plot the dendrogram
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Single-linkage)")

# Complete-linkage clustering
agn_complete <- agnes(data, diss=F, stand=F, metric="euclidean", method="complete")
# make and plot the dendrogram
dend_agn_complete <- as.dendrogram(agn_complete)
plot(dend_agn_complete, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Complete-linkage)")

# Average-linkage clustering
agn_avg <- agnes(data, diss=F, stand=F, metric="euclidean", method="average")
# make and plot the dendrogram
dend_agn_avg <- as.dendrogram(agn_avg)
plot(dend_agn_avg, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Average-linkage)")

# Could also plot the dendograms like so if you like this view better
par(mfrow=c(1,3))
plot(agn, which.plot=2)
plot(agn_complete, which.plot=2)
plot(agn_avg, which.plot=2)

################################################################################
## cut the dendrogram and get cluster assignments
################################################################################
# Option 1: Specify how many clusters you want, using k=<value>
(c3 <- cutree(tree=agn_avg, k=3))

# Option 2: Specify the height of your dendrogram where to cut the obs into 
#           clusters, using h=<value>
(ck <- cutree(as.hclust(agn_avg), h=9))

# plot option 2 so students can see what it looks like.
par(mfrow=c(1,2))
plot(dend_agn_avg, xlab="Index of data points", ylab="Steps"
     , main="Dendrogram (Average-linkage)")
plot(dend_agn_avg, xlab="Index of data points", ylab="Steps"
     , main="Cut at height = 9")
abline(h=9, col="red")

################################################################################
# Clustering - Hands on skills training
################################################################################
# load data
getwd()
setwd("C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\DM\\8_Clustering Algorithms\\1_Hierarchical_and_kMeans")
df <- read.table(file="1990CAHousingData.csv", header=T, sep=",")
df <- df[,c(5,8,9)] # lets look at just latitude, longitude, and total bedrooms

################################################################################
# standardize variables
dfz <- scale(df)
dfz <- data.frame(scale(df))

# change variable names so they have a "_z" after them
for (i in 1:3) {
    names(dfz)[i] <- paste0(names(dfz)[i],"_z")
}
names(dfz)

################################################################################
# k-means clustering
################################################################################
set.seed(1234)  # set this to replicate results

# create a training and testing. This one is 50%/50% train/test
# here I'll just identify the rows that will be used for train
rows = sample(1:nrow(dfz), round(nrow(dfz)*.5,0))

#run kmeans for diff values of k so we can identify performance by # of clusters
cost_df <- data.frame() #accumulator for cost results
cost_df
for(k in 1:15){
    #allow up to 50 iterations to obtain convergence, and do 20 random starts
    # train set
    kmeans_tr <- kmeans(x=dfz[rows,], centers=k, nstart=20, iter.max=50)
    # test set
    kmeans_te <- kmeans(x=dfz[-rows,], centers=k, nstart=20, iter.max=50)
    
    #Combine cluster number and cost together, write to df
    cost_df <- rbind(cost_df, cbind(k, kmeans_tr$tot.withinss
                                    , kmeans_te$tot.withinss))
}

# the cost_df data.frame contains the # of clusters k and the MSE for each cluster
names(cost_df) <- c("cluster", "tr_cost", "te_cost")
cost_df

# create an elbow plot
par(mfrow=c(1,1))
cost_df[,2:3] <- cost_df[,2:3]/1000
plot(x=cost_df$cluster, y=cost_df$tr_cost, main="k-Means Elbow Plot"
     , col="blue", pch=19, type="b", cex.lab=1.2
     , xlab="Number of Clusters", ylab="MSE (in 1000s)")
points(x=cost_df$cluster, y=cost_df$te_cost, col="green")

################################################################################
# Looking at the information from your k-means clustering
# Lets just generate a k-means clustering for k=4
################################################################################
kmeans_tr <- kmeans(x=dfz[rows,], centers=4, nstart=20, iter.max=50)
kmeans_te <- kmeans(x=dfz[-rows,], centers=4, nstart=20, iter.max=50)

# You can look at the fit object to to see how many records are in each cluster,
# the final centroids, the final cluster assignments, statistics of within and
# between clusters
kmeans_tr

# get cluster means (couple different ways)
(centroids <- aggregate(dfz[rows,], by=list(kmeans_tr$cluster), FUN=mean))
kmeans_te$centers

# see the number of obs within each cluster
kmeans_tr$size 
kmeans_te$size

################################################################################
## k-Means clustering (evaluation by visually inspecting cluster formations)
# Does a PCA analysis and plots the clusters among the first two PCs
# This takes about 5 minutes to run on my laptop
library(useful)
plot(kmeans_tr, data=dfz[rows,])

################################################################################
# visualizing clusters using plot3D package
library(plot3D)
par(mfrow=c(1,2))
scatter3D(x = dfz[rows,2], y = dfz[rows,3], z = dfz[rows,1]
          , surface=F, gridlines=26, grid=T, pch=19, point.col = "blue"
          , colvar=kmeans_tr$cluster, colkey = F, border = "black"
          , ticktype = "detailed", bty = "g", lwd = 2, phi = 20
          , main = "k-Means clustering", xlab="lat", ylab="long"
          , zlab="bedrooms")

# see centroids of each cluster
scatter3D(x = centroids[,"latitude_z"], y = centroids[,"longitude_z"]
          , z = centroids[,"total_bedrooms_z"]
          , surface=F, gridlines=26, grid=T, pch=19, point.col = "blue"
          , colvar=centroids$Group.1, colkey = F, border = "black"
          , ticktype = "detailed", bty = "g", lwd = 2, phi = 20
          , main = "Cluster centroids", xlab="lat", ylab="long"
          , zlab="bedrooms")

################################################################################
# visualizing clusters using scatterplot3d package
library(scatterplot3d)
par(mfrow=c(1,2))
scatterplot3d(x = dfz[rows,2], y = dfz[rows,3], z = dfz[rows,1]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="k-Means clustering", xlab="lat", ylab="long"
              , zlab="bedrooms", bg=kmeans_tr$cluster)
# see centroids of each cluster
scatterplot3d(x = centroids[,"latitude_z"], y = centroids[,"longitude_z"]
              , z = centroids[,"total_bedrooms_z"]
              , color = "black", angle=40, cex.symbols=1.3, pch=21
              , main="Cluster centroids", xlab="lat", ylab="long"
              , zlab="bedrooms", bg=centroids$Group.1)

################################################################################
# visualizing clusters using rgl package
library(rgl)
plot3d(x = dfz[rows,2], y = dfz[rows,3], z = dfz[rows,1], col=kmeans_tr$cluster
       ,xlab="lat", ylab="long", zlab="bedrooms")

################################################################################
# Hierarchcial -linkage clustering using hclust() function
# Uses Euclidean distance as the dissimilarity measure
# the dist() function computes a 50x50 inter-observation Euclidean distance matrix
################################################################################
# trained clusterings
hc.single_tr <- hclust(dist(dfz[rows,1:3]), method="single")
hc.complete_tr <- hclust(dist(dfz[rows,1:3]), method="complete")
hc.average_tr <- hclust(dist(dfz[rows,1:3]), method="average")
# tested clusterings
hc.single_te <- hclust(dist(dfz[-rows,1:3]), method="single")
hc.complete_te <- hclust(dist(dfz[-rows,1:3]), method="complete")
hc.average_te <- hclust(dist(dfz[-rows,1:3]), method="average")

# plot the single-linkage dendogram to show messy view with typical datasets
par(mfrow=c(1,1))
dend_agn <- as.dendrogram(hc.single_tr)  
plot(dend_agn, xlab="data points", ylab="Steps", main="Single-linkage")

# cut the tree so the labels correspond to the rows appropriately
# all the clusterings look sparse for single-linkage
table(cutree(hc.single_tr, k=2))
table(cutree(hc.single_tr, k=3))
table(cutree(hc.single_tr, k=4))
table(cutree(hc.single_tr, k=5))

# it appears that maybe k=4 might be clustering to consider
table(cutree(hc.complete_tr, k=2))
table(cutree(hc.complete_tr, k=3))
table(cutree(hc.complete_tr, k=4))
table(cutree(hc.complete_tr, k=5))

# all the clusterings look sparse for average-linkage
table(cutree(hc.average_tr, k=2))
table(cutree(hc.average_tr, k=3))
table(cutree(hc.average_tr, k=4))
table(cutree(hc.average_tr, k=5))

# create k=4 clusters for the complete-linkage approach
c4_tr <- cutree(tree=hc.complete_tr, k=4)
c4_te <- cutree(tree=hc.complete_te, k=4)

################################################################################
# Lets evaluate our clusters geospatially using Tableau by first saving our
# generated clusters to our dataset (with our original data), then write out
# the results to file for Tableau to use
################################################################################
# create dataset with original features and new cluterings called 'results'
dfz$set <- NA
dfz[rows,"set"] <- "Train"
dfz[-rows,"set"] <- "Test"
dfz$kmeans <- NA
dfz[rows,"kmeans"] <- kmeans_tr$cluster
dfz[-rows,"kmeans"] <- kmeans_te$cluster
dfz$comp_link <- NA
dfz[rows,"comp_link"] <- c4_tr
dfz[-rows,"comp_link"] <- c4_te
results <- cbind(df,dfz)

# write out clusters.dlm file (this will be written to your working directory)
write.table(results, file="clusters.dlm", sep="|", quote=T
            , col.names=T, row.names=F)
getwd()

################################################################################
# You can perform Hierarchical clustering using agnes() function from the cluster
# library but it this will take awhile with just 20k observations. 
# 
# If you have datasets this large, either
# (1) use hclust, or
# (2) take a smaller random sample to cluster
################################################################################
library(cluster)
# For the three cases below you might notice that we changed stand=T 
# We did this because we have already standardized our features. In the above
# code we had stand=F because we our features were not already standardized
# and the thus the agnes function will do that for us before it runs.
?agnes

# Perform single-linkage clustering
agn <- agnes(dfz[,1:3], diss=F, stand=T, metric="euclidean", method="single")
# make and plot the dendrogram
dend_agn <- as.dendrogram(agn)  
plot(dend_agn, xlab="data points", ylab="Steps", main="Single-linkage")

# Perform complete-linkage clustering
agn_com <- agnes(dfz[,1:3], diss=F, stand=T, metric="euclidean", method="complete")
# make and plot the dendrogram
dend_agn_comp <- as.dendrogram(agn_com)
plot(dend_agn_comp, xlab="data points", ylab="Steps", main="Complete-linkage")

# Perform average-linkage clustering
agn_avg <- agnes(dfz[,1:3], diss=F, stand=T, metric="euclidean", method="average")
# make and plot the dendrogram
dend_agn_avg <- as.dendrogram(agn_avg)
plot(dend_agn_avg, xlab="data points", ylab="Steps", main="Average-linkage")
