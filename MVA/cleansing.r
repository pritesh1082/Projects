library(ggthemes)
library(GGally)
library(RColorBrewer)
library(tidyverse)
library(data.table)
library(Hmisc)
Istanbul <- copy(AirbnbIstanbul)

setDT(Istanbul)
class(Istanbul)

# data exploration #
str(Istanbul)
grep('NA',Istanbul) ## indicates NA values are there in 2nd, 5th, 13th and 14th column
# i.e. name, neighbourhood_group, last_review and reviews_per_month
head(Istanbul,20) 
dim(Istanbul) # 16251 obs. and 16 vars

summary(Istanbul)

Istanbul[is.na(neighbourhood_group),NROW(neighbourhood_group)] ## there are 16251 NA values
Istanbul[is.na(name),NROW(name)] 
Istanbul[is.na(last_review),NROW(last_review)] ## there are 8484 NA values
Istanbul[is.na(reviews_per_month),NROW(reviews_per_month)] ## there are 8484 NA values

Istanbul$neighbourhood_group <- NULL ## removing neighbourhood_group column
Istanbul[,last_review:=as.Date(last_review,'%Y-%m-%d')]
Istanbul[is.na(reviews_per_month),reviews_per_month:=0] ## Impute with NA with 0 values.

# EDA # 

library(data.table)
str(Istanbul)
dim(Istanbul) # 16251 obs. and 15 vars, with last_review in date format
setDT(Istanbul.1)
Istanbul.1 <- data.frame(row.names(Istanbul), Istanbul[,c(6,7,9,10,11,13,14,15)])
 

## 
# samp <- Istanbul.1[1:100,-1] ## removing 1st column
# pairs(samp)
# randomsamp <- sample(NROW(Istanbul.1),NROW(Istanbul.1)*.005)
# rand <- Istanbul.1[randomsamp]
# pairs(rand[,-1]) ## from the random sample, there is no linearity between vars except no. of review and reviews per month
# pairs(rand[,-1], diag.panel = panel.boxplot)
# ggscatmat(rand[,-1])
# head(rand)
# plot(price ~ number_of_reviews, data=rand,xlab='reviews', ylab = 'price')
# plot(price ~ availability_365, data=rand,xlab='availability', ylab = 'price')
# 
# round(cor(rand[,-1]),4)
# 

Istanbul.2 <- Istanbul.1[,-1] ## remvoing 1st column, 16251 obs. and 8 vars.(all the quantitative ones)
# round(cor(Istanbul.2),4)
str(Istanbul.2)
setDT(Istanbul.2)
class(Istanbul.2)
Istanbul.2[is.na(reviews_per_month),NROW(reviews_per_month)]
pairs(Istanbul.2)
plot(price ~ number_of_reviews, data=Istanbul.2,xlab='reviews', ylab = 'price')
plot(price ~ availability_365, data=Istanbul.2,xlab='availability', ylab = 'price')
plot(price ~ calculated_host_listings_count, data=Istanbul.2,xlab='host listing', ylab = 'price')

## factoring categorical columns
str(Istanbul)
grep ('NA',Istanbul)
Istanbul[is.na(last_review),NROW(last_review)]
unique(Istanbul$room_type)
Istanbul[,room_type:=factor(room_type)]
Istanbul[,neighbourhood:=factor(neighbourhood)]


## plotting graphs for EDA ##
summary(Istanbul)
ggplot(Istanbul,aes(x=room_type)) + geom_bar()
ggplot(Istanbul,aes(x=room_type,y=price)) + geom_boxplot(fill='yellow')
summary(Istanbul$price)
ggplot(Istanbul,aes(y=price)) + geom_boxplot(fill='yellow')

Istanbul[,price_s:=scale(price)]
range(Istanbul$price) ## range of price

ggplot(Istanbul,aes(x = price)) + geom_histogram(fill='yellow',color='black',binwidth=30) + geom_density() 
ggplot(Istanbul,aes(x=price)) + geom_histogram(aes(y=..density..),fill='purple',bin=30) + geom_density(alpha = 0.2, fill = "purple") + scale_x_log10() 
nrow(Istanbul[price > 1000]) ## price > 1000

ylimit <- with(Istanbul, range(number_of_reviews)) * c(0.95, 1)
plot(latitude ~ longitude, data = Istanbul, xlab = "Longitude",ylab = "Latitude", pch = 10,ylim = ylimit)
with(Istanbul, symbols(latitude, longitude, circles = number_of_reviews, inches = 1, add = TRUE))

range(Istanbul$calculated_host_listings_count) ## range of host listing counts
  
#qqnorm(Istanbul[,"price"], main = "price")

## qqnorm plot ## indicating that the variables are not normalized
q1 = qqnorm(Istanbul$price)
q2 = qqnorm(Istanbul$number_of_reviews)
q2 = qqline(Istanbul$price)

## Istanbul.3 is excluding reviews per month, 16251 rows and 7 variables
Istanbul.3 <- Istanbul.2[, c("latitude","longitude","price","minimum_nights","number_of_reviews","calculated_host_listings_count","availability_365")]

Istanbul.cm <- colMeans(Istanbul.3) ## average of all the 7 variables
Istanbul.S <- cov(Istanbul.3) ## covariance of the 7 vars
Istanbul.d <- apply(Istanbul.3, MARGIN = 1, function(Istanbul.3)t(Istanbul.3 - Istanbul.cm) %*% solve(Istanbul.S) %*% (Istanbul.3 - Istanbul.cm))

## multi variate chi square plot ## to signify whether my variables are normally distributed
plot(qchisq((1:nrow(Istanbul.3) - 1/2) / nrow(Istanbul.3), df = 7), sort(Istanbul.d),
     xlab = expression(paste(chi[7]^2, " Quantile")),
     ylab = "Ordered distances")
abline(a = 0, b = 1) ## 

library(corrplot)
corrplot(cor(Istanbul.3)) ## correlation plot



Istanbul_nh <- Istanbul %>%
  group_by(neighbourhood) %>%
  summarise(price = round(mean(price), 2))



ggplot(Istanbul,aes(x=room_type,y=price)) + geom_boxplot(fill='yellow')



ggplot(Istanbul, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) +  scale_y_log10() +
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(Istanbul$price), color = "purple", linetype = 2)


summary(Istanbul$room_type)
summary(Istanbul$price)

ggplot(Istanbul, aes(x = neighbourhood, y = price)) +
  geom_boxplot(aes(fill = neighbourhood)) +  scale_y_log10() +
  xlab("neighbourhood") + 
  ylab("Price") +
  ggtitle("Boxplots of price by neighbourhood",
          subtitle = "to do") +
  geom_hline(yintercept = mean(Istanbul$price), color = "purple", linetype = 2)


Istanbul[,'price']
avgNeighbourhood=Istanbul[,mean(price),by=neighbourhood]
sort(avgNeighbourhood)
names(avgNeighbourhood) <- c("neighbourhood","avgPrice")


Istanbul %>% arrange(desc(price)) %>% top_n(10) %>% select(- host_name, -name) %>%  
  ggplot(aes(x = price, fill = neighbourhood)) +
  geom_histogram(bins = 50) +
  scale_x_log10() + 
  ggtitle("Summary of price distributions") +
  facet_wrap(~room_type + neighbourhood)
