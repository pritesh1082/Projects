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

#Creating new data table with all the quantitative column named Istanbul_num
Istanbul_num2 <- Istanbul[,c("latitude","longitude","price","minimum_nights","number_of_reviews","calculated_host_listings_count","availability_365")]
#Correlation

setDT(Istanbul_num2)
cor(Istanbul_num2)

#Very little correlation between 'Number of reviews and  calculated host listing' & 'calcHostlisting and availability365'
#PCA
#Applying PCA on numeric data as it's not much recommende for categorical data
Istanbul_ip_pca <- prcomp(Istanbul_num2,scale=TRUE)
Istanbul_ip_pca
#PC1--> Dominated by negative effect of calculated_host_listings_count and availability_365 and no of reviews
#PC2--> major +ve effect of latitude and negative effect of longitude
#PC3 --> Major +ve effect of minimum_nights and Price
#PC4 -->  Major negative effect of minimum_nights and +ve effect of price
#PC5 -->  Major negative effect of availability_365 and +ve effect of number_of_reviews

#Summary pf PCAs
summary(Istanbul_ip_pca)
#As per Summary output, 'Cumulative Proportion' field, 88.97% of Cummulative variance is explained by PC1, PC2,----PC6
#So we will have to include PC1 till PC6 to prevent loss of Information.

# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2
(eigen_Istanbul <- Istanbul_ip_pca$sdev^2)
names(eigen_Istanbul) <- paste("PC",1:7,sep="")
eigen_Istanbul
names(eigen_Istanbul)
#Taking Sum of all Eigen values
sumlambdas1 <- sum(eigen_Istanbul)
sumlambdas1 #sum of Eigenvalues is total var of ur dataset
propvar1 <- eigen_Istanbul/sumlambdas1
propvar1 #Propvar1 gives the percentage of variance for each PC component
#Percentage of total variance
percentvar <- (eigen_Istanbul/sumlambdas1) *100
percentvar
#Bar plot of Percentage variance 
barplot(percentvar, main = "Bar Plot", xlab = "Principal Component", ylab = "Percentage Variance")

#[1] 0.1833604 0.1702333 0.1473737 0.1405079 0.1322170 0.1159617 0.1103461
#OP says none of the component explains much variance so will have to imclude all
#Can do plotting at this stage
cumvar_Istanbul <- cumsum(propvar1)
cumvar_Istanbul #This variable has cummulative sum of variance
#PC1 to PC6 explain 88.96% of variance 

#Bar plot of Cummulative Percentage variance 
barplot(cumvar_Istanbul, main = "Bar Plot", xlab = "Principal Component", ylab = "Percentage Variance")

matlambdas <- rbind(eigen_Istanbul,propvar1,cumvar_Istanbul)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,4)
summary(Istanbul_ip_pca)

#As per Summary output, 'Cumulative Proportion' field, 88.97% of Cummulative variance is explained by PC1, PC2,----PC6
#So we will have to include PC1 till PC6 to prevent loss of Information.

#
Istanbul_ip_pca$rotation #= print(Istanbul_ip_pca)
#op of PCA cmd same as previous
print(Istanbul_ip_pca)
#Below command dives our new dataset
head(Istanbul_ip_pca$x,5) #This is our new dataset

#Scree Plot
plot(eigen_Istanbul, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_Istanbul), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")

#=====second part===
names(Istanbul_ip)
Istanbultyp_pca <- cbind(data.frame(neighbourhood,room_type),Istanbul_ip_pca$x)
names(Istanbultyp_pca)
#Istanbultyp_pca This is our new dataset
head(Istanbultyp_pca,5)

#Renaming Principal components
names(Istanbultyp_pca) <- c("Neighbourhood", "Room_Type", "calc_Review_365_Negative", "Lattitude_Positive_Longi_Negate", 
                            "MinNight_Price_Positive","MinNightNegative_PricePos","availabilityNegate_Reviews_Pos",
                            "Positive_Lat_Long","CalcHostListing_Pos")

#This is Our new dataset
names(Istanbultyp_pca)
#View(Istanbultyp_pca)
dim(Istanbultyp_pca)
head(Istanbultyp_pca,5)
#---

# Means of scores for all the PC's classified by Survival status so da u can perform ttest on that
tabmeansPC1 <- aggregate(Istanbultyp_pca[,c(3,4,5,6,7,8,9)],by=list(room_type=Istanbul$room_type),mean)
tabmeansPC1 #Means of all the columns per Room Type

#In this op coz of +ve -VE signs u can see the means are different
tabmeansPC1 <- tabmeansPC1[rev(order(tabmeansPC1$room_type)),]
tabmeansPC1

tabfmeans1 <- t(tabmeansPC1[,-1]) #transpose
tabfmeans1
colnames(tabfmeans1) <- t(as.vector(tabmeansPC1[1]))
tabfmeans1 #This is means for all PCs per room Type

# Standard deviations of scores for all the PC's classified by Room Type
tabsdsPC1 <- aggregate(Istanbultyp_pca[,c(3,4,5,6,7,8,9)],by=list(room_type=Istanbul$room_type),sd)
tabfsds1 <- t(tabsdsPC1[,-1])
colnames(tabfsds1) <- t(as.vector(tabsdsPC1[1]))
tabfsds1 #This is Std Deviation for all PCs per room Type
class(tabfsds1) #changed to matrix
# Levene's tests (one-sided)
library(car)
library(carData)
names(Istanbultyp_pca)
(LTPC1 <- leveneTest(calc_Review_365_Negative~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC1_1sided1 <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(Lattitude_Positive_Longi_Negate~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(MinNight_Price_Positive~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(MinNightNegative_PricePos~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(availabilityNegate_Reviews_Pos~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)

(LTPC6 <- leveneTest(Positive_Lat_Long~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC6_1sided <- LTPC6[[3]][1]/2)

(LTPC7 <- leveneTest(CalcHostListing_Pos~Istanbul$room_type,data=Istanbultyp_pca))
(p_PC7_1sided <- LTPC7[[3]][1]/2)

# Plotting the scores for the first and second components for Private Rooms

plot(Istanbultyp_pca$calc_Review_365_Negative, Istanbultyp_pca$Lattitude_Positive_Longi_Negate,
     pch=ifelse(Istanbultyp_pca$Room_Type == "Private room",1,16),xlab="PC1", ylab="PC2", main="Private rooms against values for PC1 & PC2")
abline(h=0)
abline(v=0)

legend("bottomleft", legend=c("Private Room","Other"), pch=c(1,16))

#names(Istanbul_ip_pca)
#Istanbul_ip_pca
#range(Istanbul_ip_pca$x[,1])
#Shows PCA details like center std devn
View(Istanbul_ip_pca)
diag(cov(Istanbul_ip_pca$x))
xlim <- range(Istanbul_ip_pca$x[,1])
#xlim
#Istanbul_ip_pca$x[,1]
#Istanbul_ip_pca$x
plot(Istanbul_ip_pca$x,xlim=xlim,ylim=xlim)
Istanbul_ip_pca$rotation[,1]
Istanbul_ip_pca$rotation
#plot(Istanbul[,-1])
#Plotting Variances 
plot(Istanbul_ip_pca)
#get the original value of the data based on PCA
center <- Istanbul_ip_pca$center
scale <- Istanbul_ip_pca$scale
new_Istanbul <- as.matrix(Istanbul[,-1])
new_Istanbul
#drop(scale(new_Istanbul,center=center, scale=scale)%*%Istanbul_ip_pca$rotation[,1])
predict(Istanbul_ip_pca)[,1]
names(Istanbul_ip_pca)
#The aboved two gives us the same thing. predict is a good function to know.
#out <- sapply(1:5, function(i){plot(Istanbul$room_type,Istanbul_ip_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="RoomType")})

pairs.panels(Istanbul_ip_pca$x[1:100,c(1,2,3,4,5,6)])

##Conclusion: As per Summary output, 'Cumulative Proportion' field, 88.97% of Cummulative variance is explained by PC1, PC2,----PC6
#So we will have to include PC1 till PC6 to prevent loss of Information.
