library(fpc)
library(cluster)
#read
data("unicef",package = "ks")
#EDA
colSums(is.na(unicef))
head(unicef)
summary(unicef)
boxplot(unicef)
x<-scale(unicef)
head(x)
boxplot(x)


#Using the elbow method to calculate the optimal number of clusters
set.seed(21)
wcss=vector()#Initialize the vector
for(i in 1:10) wcss[i]=sum(kmeans(x,i)$withinss)

#plot the elbow graph
plot(1:10,wcss,type = "b",main = paste("Unicef"),xlab = "Number of clusters",ylab = "WCSS")




#Applying kmeans to the unicef dataset
set.seed(7)
kmeans=kmeans(x,2,iter.max = 300,nstart = 10)


#Visualising the clusters
library(cluster)
clusplot(x,
         kmeans$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = paste("Clusters of clients"),
         xlab = "Under -5",
         ylab = "Avg life expectancy")

getwd()
