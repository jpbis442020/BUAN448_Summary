?USArrests

data("USArrests")
RNGversion("3.5.2")
set.seed(1) 
#•	Use the scale function to z-score standardize the USArrests data frames and save the standardized USArrests on a data frame named USArrests _z. 
numeric_cols <- USArrests[, -1]

USArrests_z <- as.data.frame(scale(numeric_cols))


head(USArrests_z)


k <- 5

kmeans_result <- kmeans(USArrests_z, centers = k)

# Add the cluster assignments to the original dataset
USArrests_clusters <- cbind(USArrests, Cluster = kmeans_result$cluster)

#>•	What states are in each cluster? 
print(USArrests_clusters$Cluster)
#>•	What are the values of the cluster centers? 
print(kmeans_result$centers)
