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
AirbnbIstanbul <- read.csv("C:/Pritesh/Rutgers/Courses/Projects/MVA/Dataset/AirbnbIstanbul.csv", stringsAsFactors=FALSE)
Istanbul <- copy(AirbnbIstanbul)
class(Istanbul)
setDT(Istanbul)

str(Istanbul)
Istanbul[,room_type:=factor(room_type)]
Istanbul[,neighbourhood:=factor(neighbourhood)]
Istanbul[,last_review:=as.Date(last_review,'%Y-%m-%d')] ## converting last_review to date datatype

# datatypes looks better now. hence will see again for NA values 
grep ('NA',Istanbul) # 2, 5, 13 and 14 column have NA values
Istanbul[is.na(neighbourhood_group),NROW(neighbourhood_group)] # entire obs. is blank, will drop this var
Istanbul[is.na(last_review),NROW(last_review)] ## there are 8484 NA values
Istanbul[is.na(reviews_per_month),NROW(reviews_per_month)] ## there are 8484 NA values

Istanbul$neighbourhood_group <- NULL ## removing neighbourhood_group column
Istanbul[is.na(reviews_per_month),reviews_per_month:=0] ## nearly 50% of the dataset is filled with NA.
# hence we can't simply remove these many rows. Hence imputing with 0 values.

#Creating new data table with all the quantitative column named Istanbul_num
Istanbul <- Istanbul[price < 1000] # removing outliers [1] 15638    15
dim(Istanbul)
Istanbul_factor <- Istanbul[,c("latitude","longitude","price","minimum_nights","number_of_reviews","reviews_per_month","calculated_host_listings_count","availability_365")]
corrm.Istanbul <- cor(Istanbul_factor)
corrm.Istanbul
plot(corrm.Istanbul)
Istanbul_pca <- prcomp(Istanbul_factor, scale=TRUE)
summary(Istanbul_pca)
plot(Istanbul_pca)
# A table containing eigenvalues and %'s accounted, follows. Eigenvalues are the sdev^2
(eigen_Istanbul <- round(Istanbul_pca$sdev^2,2))
names(eigen_Istanbul) <- paste("PC",1:8,sep="")
eigen_Istanbul
plot(eigen_Istanbul, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
sumlambdas <- sum(eigen_Istanbul) ## eigen values
sumlambdas

# as per scree plot, there should be 7 factors, will see what parallel analysis suggests

fa.parallel(Istanbul_factor) # See factor recommendation, didn't understand
#Parallel analysis suggests that the number of factors =  4  and the number of components =  3

vss(Istanbul_factor) # See Factor recommendations for a simple structure
# VSS complexity 1 achieves a maximimum of 0.56  with  4  factors
# VSS complexity 2 achieves a maximimum of 0.58  with  4  factors
# 
# The Velicer MAP achieves a minimum of NA  with  1  factors 
# BIC achieves a minimum of  NA  with  3  factors
# Sample Size adjusted BIC achieves a minimum of  NA  with  3  factors

nfactors(Istanbul_factor) 
# nfactors suggests we can either go with 3 factors or 4 factors

# with four factors
library(psych)
fit.pc4 <- principal(Istanbul_factor, nfactors=4, rotate="varimax") 
fit.pc4 #4 factors RC1, RC2, RC3, RC4 are created
round(fit.pc4$values, 3)
#Above are factor values for all 8 variables 
fit.pc4$loadings
# Above are the Loadings for all 8 variables

for (i in c(1,3,2,4)) { print(fit.pc1$loadings[[1,i]])}
# Communalities
fit.pc4$communality
#Above are the communalities for all 8 variabbles

# Rotated factor scores
head(fit.pc4$scores)

fa.diagram(fit.pc4) # Visualize the relationship


# with three factors
library(psych)
fit.pc3 <- principal(Istanbul_factor, nfactors=3, rotate="varimax") 
fit.pc3 #3 factors RC1, RC2, RC3, RC4 are created

round(fit.pc3$values, 3)
#Above are factor values for all 8 variables 

fit.pc3$loadings
# Above are the Loadings for all 8 variables

for (i in c(1,3,2,4)) { print(fit.pc3$loadings[[1,i]])}
# Communalities
fit.pc3$communality
#Above are the communalities for all 8 variabbles


fa.diagram(fit.pc3) # Visualize the relationship




