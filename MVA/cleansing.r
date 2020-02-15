library(data.table)
library(Hmisc)
Istanbul <- copy(AirbnbIstanbul)

setDT(Istanbul)
class(Istanbul)

# data exploration #
str(Istanbul)
grep('NA',Istanbul) ## indicates NA values are there in 2nd, 5th, 13th and 14th column
# i.e. name, neighbourhood_group, last_review and reviews_per_month
head(AbnbIstanbul,20) 
dim(AbnbIstanbul)
# [1] 16251    16 ## 16251 obs. and 16 columns

summary(AbnbIstanbul)

Istanbul[is.na(neighbourhood_group),NROW(neighbourhood_group)] ## there are 16251 NA values
Istanbul[is.na(name),NROW(name)] 
Istanbul[is.na(last_review),NROW(last_review)] ## there are 8484 NA values
Istanbul[is.na(reviews_per_month),NROW(reviews_per_month)] ## there are 8484 NA values

Istanbul$neighbourhood_group <- NULL ## removing neighbourhood_group column
Istanbul[,last_review:=as.Date(last_review,'%Y-%m-%d')]

grep('NA',Istanbul$name) 
head(Istanbul,200)

Istanbul[is.na(reviews_per_month),reviews_per_month:=0] ## Impute with NA with 0 values.

