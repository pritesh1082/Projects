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

# data exploration and cleansing #
str(Istanbul) ## to check data type of each var.
grep('NA',Istanbul) ## indicates NA values are there in 2nd, 5th and 14th column
# i.e. name, neighbourhood_group and reviews_per_month have NA values
head(Istanbul,10)  
dim(Istanbul) # 16251 obs. and 16 vars
summary(Istanbul) ## summarized view of all the feature/vars
unique(Istanbul$room_type) ## 3 unique room types
unique(Istanbul$neighbourhood) ## 39 unique neighbourhoods



## since, I used stringsAsFactors=FALSE while importing the dataset, few of the columns
## like name, host_name, neighbourhood and room_type belongs to character data type 
## hence, will factor neighbourhood and room_type for now. name and host_name doesn't seem
## to be much interest for now, hence will leave those. 
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

# performing exploratory data analysis # 

str(Istanbul)
dim(Istanbul) # 16251 obs. and 15 vars, with last_review in date format
# price looks to be our dependent variable, hence will see the distribution of price
summary(Istanbul$price)
ggplot(Istanbul, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of price",
          subtitle = "The distribution is very skewed") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(Istanbul$price), 2), size = 2, linetype = 3)

#As distribution is very skewed, performing logarithmic transformation to gain better insight
ggplot(Istanbul, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Transformed distribution of price",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(Istanbul$price), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Mean price = ", paste0(round(mean(Istanbul$price), 2), "$")),
           color =  "#32CD32", size = 8)

#What drives price? Checking Price values with respect to KPIs

#1 relationship between price and room type
describeBy(Istanbul$price,Istanbul$room_type)
ggplot(Istanbul, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(Istanbul$price), color = "purple", linetype = 2)
# We see that Entire Home/Apartments have the highest avg price. Also the private room's 
# prices are comparable to Entire Home/Apartments price

#2 price vs number of reviews
plot(price ~ number_of_reviews, data=Istanbul,xlab='reviews', ylab = 'price',col='blue')
#The most pricy listings have lesser number of reviews

#3 price vs room type and neighbourhoods
#Scatter plot in one screen, Price vs Room type & Neighbourhood
x <- ggplot(Istanbul, aes(room_type, price)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(Istanbul, aes(neighbourhood, price)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Price vs Room type & Neighbourhood", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# The scatter plot doesn't give clear picture, hence will draw the bar chart
# Above Average Price by Neighourhood Areas and room_type together.
Istanbul %>% filter(price >= mean(price)) %>% group_by(neighbourhood, room_type) %>% tally %>% 
  ggplot(aes(reorder(neighbourhood,desc(n)), n, fill = room_type)) +
  xlab(NULL) +
  ylab("Number of Room Types") +
  ggtitle("Number of Room Types having above average price",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Beyoglu, Sisli and Fatih neighbourhoods have more than the average price
# as well have more number of units than other neighbourhoods. 

# Top 10 most priced locations
range(Istanbul$price) ## range of price
avgNeighbourhood=Istanbul[,avgneighprice:=mean(price),by=neighbourhood]
Istanbul.1 <- avgNeighbourhood[price > avgneighprice]
top10localities <- head(arrange(Istanbul.1,desc(Istanbul.1$price)), n = 10)
top10localities

ggplot(top10localities, aes(x = neighbourhood, y = price)) +
  geom_boxplot(aes(fill = neighbourhood)) +  scale_y_log10() +
  xlab("neighbourhood") + 
  ylab("Price") +
  ggtitle("Boxplots of price by neighbourhood",
          subtitle = "to do") +
  geom_hline(yintercept = mean(Istanbul$price), color = "purple", linetype = 2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#4 no. of reviews and neighbourhood relation
summary(Istanbul$number_of_reviews)
ggplot(Istanbul,aes(x=neighbourhood,y=number_of_reviews)) + geom_boxplot(fill='yellow') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Top 10 neighbourhoods having most number of reviews, pending
# top10reviews_by_locality <- head(arrange(Istanbul,desc(Istanbul$number_of_reviews)), n = 10)
# top10reviews_by_locality
# ggplot(top10reviews_by_locality,aes(x=neighbourhood,y=number_of_reviews)) + geom_boxplot(fill='yellow') + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#5 price vs availability relation
plot(price ~ availability_365, data=Istanbul,xlab='availability', ylab = 'price')
#It is hard to see a clear pattern but the most priced listings have either very few days availability 
# or maximum days availability

#6 price vs minimum nights relation
plot(price ~ minimum_nights, data=Istanbul,xlab='minimum_nights', ylab = 'price')
#with lesser number of 'min no of nights' , Prices are high and Prices decrease with increase in Min no of nights

#7 listing vs room type relation
#no of listings vs room type
ggplot(Istanbul,aes(x=room_type)) + geom_bar(fill = 'blue')+
  ylab("Number of Listings") +
  ggtitle("Number of listings Roomtype wise")
#Private rooms are more in number

#8 no of listings neighbourhoodwise
ggplot(Istanbul,aes(x=neighbourhood)) + geom_bar(fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Number of Listings") +
  ggtitle("no of listings neighbourhoodwise")
# Beyoglu, Sisli and Fatih have most number of listings


# checking correlation between all the variables
Istanbul.2 <- Istanbul[,c(6,7,9,10,11,13,14,15)] ## filtering dataset containing only numberical data
## qqnorm plot ## indicating that the variables are not normalized
q1 = qqnorm(Istanbul.2$price)
q2 = qqnorm(Istanbul.2$number_of_reviews)
skewness(price)
#skewness is 28.86 so its high skewness as out of range of -1 to 1
#Price not normal

Istanbul.3 <- Istanbul.2[, c("latitude","longitude","price","minimum_nights","number_of_reviews","calculated_host_listings_count","availability_365")]
Istanbul.cm <- colMeans(Istanbul.3) ## average of all the 7 variables
Istanbul.S <- cov(Istanbul.3) ## covariance of the 7 vars
Istanbul.d <- apply(Istanbul.3, MARGIN = 1, function(Istanbul.3)t(Istanbul.3 - Istanbul.cm) %*% solve(Istanbul.S) %*% (Istanbul.3 - Istanbul.cm))

## multi variate chi square plot ## to signify whether my variables are normally distributed
plot(qchisq((1:nrow(Istanbul.3) - 1/2) / nrow(Istanbul.3), df = 7), sort(Istanbul.d),
     xlab = expression(paste(chi[7]^2, " Quantile")),
     ylab = "Ordered distances")
abline(a = 0, b = 1) 
## this signifies that multi variables are not normally distributed


#Correlation matrix
corr <- cor(Istanbul.3)
corrmelt <- melt(corr)
ggplot(corrmelt) + geom_tile(aes(Var1, Var2, fill=value)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Some correlation between Calculated host listing and noof reviews
#Some correlation between Price and calculated_host_listings_count
#Some correlation between Price and availability_365
#Some correlation between calculated_host_listings_count & availability_365
#A bit of relation between Price and Lattitude




