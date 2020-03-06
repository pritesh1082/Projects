install.packages("cluster", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cluster)

library(data.table)#Data. table is an extension of data. frame package in R. It is widely used for fast aggregation of large datasets,
library(Hmisc)#data analysis funs
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(ggthemes)
library(psych)
library(relaimpo)
library(e1071)

AirbnbIstanbul<-read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/R/AirbnbIstanbul.csv",stringsAsFactors = FALSE)
Istanbul <- copy(AirbnbIstanbul)
View(Istanbul)
str(Istanbul)
#Checking number f rows and columns
dim(Istanbul)
class(Istanbul)
names(Istanbul)
attach(Istanbul)
head(Istanbul,25)
#Removing neighbourhood_group and last_review
Istanbul_ip<-Istanbul[,-c(5,13)]
names(Istanbul_ip)
sum(is.na(Istanbul_ip)) #8484
#To get the column names that have null values
!!colSums(is.na(Istanbul_ip))
#reviews_per_month has NULL values
#how manu null values in reviews_per_month
sum(is.na(reviews_per_month)) #8484
summary(Istanbul_ip)
#Imputing zeros where reviews_per_month is null
reviews_per_month[is.na(reviews_per_month)] <- 0
#Aganin checking for null values after imputation
sum(is.na(reviews_per_month)) #op=0

names(Istanbul_ip)
#removing unneccessary columns
Clust_IP<-Istanbul[,-c(1,2,3,4,12)]
names(Clust_IP)
head(Clust_IP,5)
#Correlation
#Creating new data table with all the quantitative column named Istanbul_num
#Istanbul_num2 <- Istanbul[,c("latitude","longitude","price","minimum_nights","number_of_reviews","calculated_host_listings_count","availability_365")]

#/////////
#AirbnbIstanbul<-read.csv("",stringsAsFactors = FALSE)
Istanbul <- copy(AirbnbIstanbul)
View(Istanbul)

air_dist.mat <- read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/R/AirbnbIstanbul.csv", row.names=1)
View(air_dist.mat)

# Standardizing the data with scale()
matstd.Istanbul <- scale(employ[,2:10])
# Creating a (Euclidean) distance matrix of the standardized data
dist.employ <- dist(matstd.employ, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)
clusemploy.nn <- hclust(dist.employ, method = "single")

air_dist.mat5 <- DistancesTable

colnames(dist.mat5) <- rownames(dist.mat5)

dist.mat5 <- as.dist(dist.mat5)

dist.mat5

#Single
mat5.nn <- hclust(dist.mat5, method = "single")
plot(mat5.nn, hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Nearest neighbor linkage")

#If u say cut1 (1 2 3 4 5)
#cut2 (12 , 345)
#cut3 (12 3 45)
#cut 4 ( 1,2 3 4 5)
#Its horizontal cutting
#Cut tree is imp because u need to decide which grps u want

#Default - Complete
mat5.fn <- hclust(dist.mat5)
plot(mat5.fn,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Farthest neighbor linkage")
#In this case single and complete , avg giving same ans

#Average
mat5.avl <- hclust(dist.mat5,method="average")
plot(mat5.avl,hang=-1,xlab="Object",ylab="Distance",
     main="Dendrogram. Group average linkage")


#===== group assignment start

library(data.table)
library(fpp)
library(fpp2)
library(cowplot)
library(tidyverse)
library(psych)
library(e1071)
library(dplyr)
library(corrplot)
library(GGally)
library(reshape2)
AirbnbIstanbul <- read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester2/MVA/R/AirbnbIstanbul.csv", stringsAsFactors=FALSE)
Istanbul <- copy(AirbnbIstanbul)
class(Istanbul)
setDT(Istanbul)

str(Istanbul)
Istanbul[,room_type:=factor(room_type)]
Istanbul[,neighbourhood:=factor(neighbourhood)]
Istanbul[,last_review:=as.Date(last_review,'%Y-%m-%d')] ## converting last_review to date datatype
names(Istanbul)
# datatypes looks better now. hence will see again for NA values 
grep ('NA',Istanbul) # 2, 5, 13 and 14 column have NA values
Istanbul[is.na(neighbourhood_group),NROW(neighbourhood_group)] # entire obs. is blank, will drop this var
Istanbul[is.na(last_review),NROW(last_review)] ## there are 8484 NA values
Istanbul[is.na(reviews_per_month),NROW(reviews_per_month)] ## there are 8484 NA values

Istanbul$neighbourhood_group <- NULL ## removing neighbourhood_group column
Istanbul[is.na(reviews_per_month),reviews_per_month:=0] ## nearly 50% of the dataset is filled with NA.
# hence we can't simply remove these many rows. Hence imputing with 0 values.


range(Istanbul$price) ## range of price
avgNeighbourhood=Istanbul[,avgneighprice:=mean(price),by=neighbourhood]
Istanbul.1 <- avgNeighbourhood[price > avgneighprice]
View(avgNeighbourhood)
summary(Istanbul.1$price)
summary(Istanbul$price)
ggplot(Istanbul,aes(y=price)) + geom_boxplot(fill='yellow')
#View(Istanbul.1)
## no. of reviews and neighbourhood relation
summary(Istanbul$number_of_reviews)
nrow(Istanbul[price > 1000]) ## price > 1000
ggplot(Istanbul,aes(y=number_of_reviews)) + geom_boxplot(fill='yellow')
ggplot(Istanbul,aes(x=neighbourhood,y=number_of_reviews)) + geom_boxplot(fill='yellow') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

Istanbul.clust <- Istanbul[price < 1000 & number_of_reviews > 0] ## price > 1000
ggplot(Istanbul.clust,aes(y=price)) + geom_boxplot(fill='yellow')
grep('NA',Istanbul.clust)
names(Istanbul.clust)

#Now Istanbul.clust is the input dataset for clustering
######## hierarchial clustering ###
library(data.table)
Istanbul_clus <- Istanbul.clust[,c("latitude","longitude","price","minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")]
names(Istanbul_clus)
# Standardizing the data with scale()
matstd.Istanbul <- scale(Istanbul_clus[,2:8])

#Only 100 rows have been used to plot the dendogram
matstd_airbnb <- Istanbul_clus2[1:100,] 

# Creating a (Euclidean) distance matrix of the standardized data
dist_Istanbul <- dist(matstd.Istanbul, method="euclidean")
# Invoking hclust command (cluster analysis by single linkage method)
Istanbul.hclust <- hclust(dist_Istanbul, method = "single") 
#complete
Istanbul.hclust <- hclust(dist_Istanbul)

#Plotting dendogram
plot(as.dendrogram(Istanbul.hclust),ylab="Distance between..",ylim=c(0,2.5),main="Dendrogram of..")
#Dendogram not getting plot as data is very large 
dim(dist_Istanbul)

head(dist_Istanbul)

######

########## K-means Clustering #########
#library(cluster)
#Istanbul_clus1 = data.frame(
#  Istanbul.clust$price,
#  Istanbul.clust$number_of_reviews,
#  Istanbul.clust$reviews_per_month)

##Scaling done to make the data on one scale.
#Istanbul.Scale <- scale(Istanbul_clus1[,1:3])

#Here we have selected first row to see how our scaled matrix is like
#head(Istanbul.Scale,1)

# We will find K-means by taking k=2, 3, 4, 5, 6...
# Centers (k's) are numbers thus, 10 random sets are chosen

#For 2 clusters, k-means = 2
#kmeans2.Istanbul <- kmeans(Istanbul.Scale,2,nstart = 10)
# Computing the percentage of variation accounted for two clusters
#perc_var_kmeans2 <- round(100*(1 - kmeans2.Istanbul$betweenss/kmeans2.Istanbul$totss),1)
#names(perc_var_kmeans2) <- "Perc. 2 clus"
#perc_var_kmeans2
#######

#K Means Clustering for Clustering only with lattitude longitude and price


library(cluster)
Istanbul_clus2 = data.frame(
  Istanbul.clust$price,
  Istanbul.clust$latitude,
  Istanbul.clust$longitude)

View(Istanbul_clus2)
#Adding ID (property id from original datatset as index)
rownames(Istanbul_clus2) <- Istanbul.clust$id
##Scaling done to make the data on one scale.
Istanbul.Scale1 <- scale(Istanbul_clus2[,1:3])

#Here we have selected first row to see how our scaled matrix is like
head(Istanbul.Scale1,1)

# We will find K-means by taking k=2, 3, 4, 5, 6...
# Centers (k's) are numbers thus, 10 random sets are chosen

#Elbow Plot to Identify the Best number of K Clusters
wss=c()########## empty vector to hold wss
for(i in 2:10)#### from 2 to 10 cluster
{
  km = kmeans(Istanbul.Scale1[,1:3],i)
  wss[i-1]=km$tot.withinss
}
wss
## [1] 15197.254 10745.783  7987.996  6808.887  5980.367  5311.900  4846.853
## [8]  4240.790  3709.000
#Creating a 'elbowdt' data table with column names num and wss with the contents of wss
elbowdt = data.table(num=2:10,wss)
elbowdt
#Plotting
ggplot(elbowdt,aes(x=num,y=wss)) + geom_line()

#For k = 6 the between sum of square/total sum of square ratio tends to change slowly 
#and remain less changing as compared to others. Therefore, k = 6 should be a good choice for the number of clusters.

#For 6 clusters, k-means = 6

kmeans6.Istanbul <- kmeans(Istanbul.Scale1,6,nstart = 10)

#Printing cluster means and other output
kmeans6.Istanbul

#plotting output of kmeans for 6 clusters
library(factoextra)
fviz_cluster(kmeans6.Istanbul,data=Istanbul.Scale1)

#From above plot, one can not identify the cluster boundaries
#Especially for cluster 2
#Also, clusters 1 and 6 look bit overlapped.
#Hence, I infer that k=6 does not correctly apply clustering on my inpput dataset
#As per general idea about my dataset, the Airbnb property locations looks to be divided into 4 major groups
#So applying k-means clustering with '4' clusters 

kmeans4.Istanbul <- kmeans(Istanbul.Scale1,4,nstart = 10)

#Printing cluster means and other output
kmeans4.Istanbul

#plotting output of kmeans
library(factoextra)
fviz_cluster(kmeans4.Istanbul,data=Istanbul.Scale1)

#As per above plot, you can see 4 clusters with much clear distinction amongst them

# Computing the percentage of variation accounted for two clusters
perc_var_kmeans4 <- round(100*(1 - kmeans4.Istanbul$betweenss/kmeans4.Istanbul$totss),1)
names(perc_var_kmeans4) <- "Perc. 4 clus"
perc_var_kmeans4

# Saving four k-means clusters in a list
#View(Istanbul.Scale1)
#View(kmeans4.Istanbul)
#View(kmeans4.Istanbul$cluster)
head(kmeans4.Istanbul$cluster)
#USArrests <- mutate(Cluster = kmeans4.Istanbul$cluster) 
#a<-names(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 1])
#USArrests %>%
#  mutate(Cluster = kmeans4.Istanbul$cluster) %>%
#  group_by(Cluster) %>%
#  summarise_all("mean")

clus1 <- matrix(names(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 1]), 
                ncol=1, nrow=length(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 1]))
head(clus1)
colnames(clus1) <- "Cluster 1"
clus1l

clus2 <- matrix(names(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 2]), 
                ncol=1, nrow=length(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 2]))
colnames(clus2) <- "Cluster 2"
clus2

clus3 <- matrix(names(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 3]), 
                ncol=1, nrow=length(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 3]))
colnames(clus3) <- "Cluster 3"
clus3

clus4 <- matrix(names(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 4]), 
                ncol=1, nrow=length(kmeans4.Istanbul$cluster[kmeans4.Istanbul$cluster == 4]))
colnames(clus4) <- "Cluster 4"
clus4
list(clus1,clus2,clus3,clus4)
#This is the clusters having groups of property ids
#Trying to print the Price and longitude lattitude corresponding to these ids

out <- cbind(Istanbul.Scale1, clusterNum = kmeans4.Istanbul$cluster)
#This is the input dataset with respective Clusters assigned to them

head(out,5)
#View(kmeans4.Istanbul)

#Plotting these clusters
#fviz_cluster(kmeans4.Istanbul,data=Istanbul.Scale1)

#other way of plotting the clusters
library(fpc)
plotcluster(Istanbul.Scale1,kmeans4.Istanbul$cluster)
str(out)
#View(out)

#Trying plotting only with Lattitudes and Longitudes to see if the clustering 
#is done based on locations

#View(Istanbul_clus2)
names(Istanbul_clus2)
onlylattitudeLongitude<-Istanbul_clus2[,-c(1)]
#onlyprice<-data.frame(Istanbul_clus2$price)
names(onlylattitudeLongitude)
#View(onlyprice)
#Plotting for only Lattitude and Longitude 
fviz_cluster(kmeans4.Istanbul,data=onlylattitudeLongitude)
#They do not seem to be divided as per the lattitudes and longitudes
#plotcluster(onlylattitudeLongitude,kmeans4.Istanbul$cluster)

#Making Subsets for 4 clusters using Row filtering from the Original dataset
#(Not the scaled one)
#So below are the 4 cluster sets of Original entire dataset

AirIstanbul_clust1<-subset(Istanbul_ip,Istanbul_ip$id %in% clus1)
AirIstanbul_clust2<-subset(Istanbul_ip,Istanbul_ip$id %in% clus2)
AirIstanbul_clust3<-subset(Istanbul_ip,Istanbul_ip$id %in% clus3)
AirIstanbul_clust4<-subset(Istanbul_ip,Istanbul_ip$id %in% clus4)

head(AirIstanbul_clust1,3)
head(AirIstanbul_clust2,3)
head(AirIstanbul_clust3,3)
head(AirIstanbul_clust4,3)

#As per above head outputs, the clusters are formed based on locations
#Checking the means of these 4 clusters
kmeans4.Istanbul$centers

#Printing Neighbourhoods particular to the clusters to check if they are saggregated based on neighbourhoods
unique(Istanbul.1$neighbourhood) #We have total 39 unique neighbourhoods
unique(AirIstanbul_clust1$neighbourhood)
unique(AirIstanbul_clust2$neighbourhood)
unique(AirIstanbul_clust3$neighbourhood)
unique(AirIstanbul_clust4$neighbourhood)

#Lets check average Price in these clusters
mean(AirIstanbul_clust1$price)
mean(AirIstanbul_clust2$price)
mean(AirIstanbul_clust3$price)
mean(AirIstanbul_clust4$price)

#The Properties in clusters 1,3 and 4 are pretty much affordable as mean Price around $180
#Cluster 2 properties are very expensive ones
#Plotting cluster1
ggplot(AirIstanbul_clust1,
aes(x=AirIstanbul_clust1$latitude,y=AirIstanbul_clust1$longitude))+
geom_point(size=0.1,color='dark blue')

#Plotting cluster2

ggplot(AirIstanbul_clust2,
       aes(x=AirIstanbul_clust2$latitude,y=AirIstanbul_clust2$longitude))+
  geom_point(size=0.1,color='dark blue')

#Plotting cluster3

ggplot(AirIstanbul_clust3,
       aes(x=AirIstanbul_clust3$latitude,y=AirIstanbul_clust3$longitude))+
  geom_point(size=0.1,color='dark blue')

#Plotting cluster4

ggplot(AirIstanbul_clust4,
       aes(x=AirIstanbul_clust4$latitude,y=AirIstanbul_clust4$longitude))+
  geom_point(size=0.1,color='dark blue')

#The above 4 graphs show 
#How the Properties are clustered as per Price and longitudes and lattitudes.


#=========trying to resolve error start
#library(mclust)
#p<-ncol(Istanbul.Scale1) 
#p
#g<-4
#sigma <- array(NA, c(p, p, g))
#new <- as.data.frame(cbind(Istanbul.Scale1, k_fit$cluster))
#for (i in 1 : g) {
#  
#  subdata <- subset(new[, 1 : p], new[, (p+1)]==i) 
#  sigma[,, i] <- cov(subdata)
#}
#str(Istanbul.Scale1)
#
#variance <- mclustVariance("EEE", d = p, G = 4)
#par$variance <- variance
#par$variance$sigma <- sigma
#par <- vector("list", g)
#kk <- em(modelName = "EEE", data = Istanbul.Scale1, parameters = par)
#=========trying to resolve error end