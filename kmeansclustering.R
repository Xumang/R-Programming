
# Getting data from the source
flightrec <- read.csv('Sample2008_final.csv')

flightrec.new = flightrec

# Removing unnecessary colums from the data
flightrec.new$SN <- NULL
flightrec.new$Year <- NULL
flightrec.new$Month <- NULL
flightrec.new$DepTime <- NULL
flightrec.new$CRSDepTime <- NULL
flightrec.new$ArrTime <- NULL
flightrec.new$CRSArrTime <- NULL

# Getting summary of the data to see the new dataframe
summary(flightrec.new)

# Subset the flight record data
new.data = flightrec.new[,c(1,2,8,9,10,13)]

# Plot subset data
plot(new.data, main = "% of flight dealy with unique carriers", pch =20, cex =2)


# Checking the optimal number of clusters given the data from k=2 to k=15

new_data <- new.data
wss <- (nrow(new_data)-1)*sum(apply(new_data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(new_data,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Total Number of Clusters",
     ylab="Within groups sum of squares",
     main="Finding optimal cluster using Elbow Method",
     pch=20, cex=2)



# Perform K-Means with the optimal number of clusters identified from the Elbow method K=3
set.seed(7)
kmclustering = kmeans(new.data, 3, nstart=100)

# Examine the result of the clustering algorithm
kmclustering

#plotting the result
plot(new.data, col =(kmclustering$cluster +1) , main="K-Means result with 3 clusters", pch=20, cex=2)
