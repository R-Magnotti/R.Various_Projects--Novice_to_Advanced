#Author: Richard Magnotti

#normalized numeric subset of brand
cars = read.csv("cars.txt",header=T)
cars.num = cars[c(1:7)]
cars.int = as.integer(cars$brand)
cars.cluster = cbind(cars.int,cars.sub1)
str(cars.cluster)
cars.scale=as.data.frame(scale(cars.cluster))
str(cars.scale)

#compute WSS for 1-15 cluster centers
wss=(nrow(cars.scale) - 1) * sum(apply(cars.scale, 2, var))

for(i in 1:15)
{wss[i]=sum(kmeans(cars.scale,center=i)$withinss)}

plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within Cluster Sum of Sq Error or WSS")

#identify potential point of diminishing return
#the point of diminishing return would be around 12 clusters, K~12 

#use kmeans
#install.packages("fpc")
library(fpc)
cars.ch = kmeansruns(cars.scale, krange = 1:15, criterion ="ch")
cars.ch
cars.ch$bestk #k=2
cars.asw = kmeansruns(cars.scale, krange = 1:15, criterion ="asw")
cars.asw #k=2
#the two k-values agree, k=2

#use cluster.stats
fit.cars = kmeans(cars.scale,2,nstart = 100,iter.max = 100)
print.cluster = function(labels, k){
  for( i in 1:k){
    print(paste("Cluster #", i))
    print(cars[labels==i,c(1:7)])
  }
}
groups.fit.tax = fit.cars$cluster
fit.cars
fit.cars$cluster
print.cluster(groups.fit.tax, 2)
fit.cars$centers

dmatrix= dist(cars.scale, method = "euclidean")
library(cluster)
cluster.stats(dmatrix, fit.cars$cluster)

#use aggregate command
agg.min = aggregate(cars.cluster, by = list(fit.cars$cluster), FUN = min)
agg.max = aggregate(cars.cluster, by = list(fit.cars$cluster), FUN = max)
agg.mean = aggregate(cars.cluster, by = list(fit.cars$cluster), FUN = mean)
agg.min
agg.max
agg.mean

#print kmeans clusters
fit.cars

#use HEIRARCHICAL Ward method, "fith", the 'h' stants for h=heirarchical
fith.cars=hclust(dmatrix, method="ward.D") # uses ANOVA to form clusters
plot(fith.cars)
#best k value is 2
rect.hclust(fith.cars, k = 2, border = "red")
h.groups = cutree(fith.cars, k = 2)

#print heirarchichal groups
h.groups

library(som)
som2by2 = som(cars.scale, xdim = 2, ydim = 2)
som2by2

#how do clusters compare for kmeans and heirarchical?
#we need to print each cluster group to examine
fit.cars$cluster
h.groups
#it looks like h.groups has more same-records per cluster 