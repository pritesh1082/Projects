# -*- coding: utf-8 -*-
"""
Created on Mon May 18 19:58:04 2020

@author: prite
"""

import numpy as np
import pandas as pd
import missingno as msno
import matplotlib.pyplot as plt
import seaborn as sns

# importing the dataset from a local drive
abnb = pd.read_csv("C:\Pritesh\Rutgers\Courses\MultiVariateAnalysis\ProjectRef\AirbnbIstanbul.csv")

# Initial Analysis and Cleansing

abnb.dtypes # similar to str fn in R
abnb.shape ## to know the dimension of df class, similar to dim in R
abnb.info()
abnb.head(10) ## first 10 rows andd all columns ##
abnb['room_type'].value_counts() ## to find the number of room types and their counts
abnb.neighbourhood.value_counts() ## to find the most popular neighbourhoods (count of neigh is decreasing order)
msno.bar(abnb) ## bar plot of missing values
pd.isnull(abnb.neighbourhood_group).value_counts() ## entire neigh group has NA values
pd.isnull(abnb.last_review).value_counts() ## 8484 NA values
pd.isnull(abnb.reviews_per_month).value_counts() ## 8484 NA values
abnb.drop(['neighbourhood_group'],axis=1,inplace=True) ## dropping neighbourhood_group column
abnb.last_review = pd.to_datetime(abnb.last_review) ## to datetime
abnb.shape
abnb.dtypes

# imputation
abnb.reviews_per_month.fillna(0,inplace=True)
abnb.host_name.fillna('Not Stated',inplace = True)
abnb.name.fillna('No Description',inplace = True)

# keep = False, means only unique values are taken and duplicated values are removed 
dup_abnb = abnb[abnb.duplicated(['host_id','neighbourhood'],keep=False)].sort_values(['host_id'])
dup_abnb.head(10)
dup_abnb[['host_id','neighbourhood']].head(10) # observe duplicate host ids and neighbourhoods
dup_dict = dup_abnb.host_id.value_counts()
dup_dict = dup_dict.reset_index() # there are 1838 host who have more than 1 listings

# which host id's have maximum listings?
dfhost_neighbour = dup_abnb.groupby('host_id')['neighbourhood'].count()
dfhost_neighbour = dfhost_neighbour.reset_index()
dfhost_neighbour.sort_values('neighbourhood',ascending=False,inplace=True) # host id - 21907588

# top 10 hosts
dfhost_neighbour.nlargest(10,'neighbourhood')

# how many neighbourhoods and unique function
distinct_neighbourhood = abnb.neighbourhood.unique() # 39 distinct neighbourhoods

# how many room_types?
distinct_roomtypes = abnb.room_type.unique()
distinct_roomtypes
abnb.shape
abnb.info()

# 2 EDA

# top 10 hosts avg rating
dup_dict = dup_abnb.host_id.value_counts()
import operator
sorted_x = sorted(dup_dict.items(), key=operator.itemgetter(1))
top10 = [i[0] for i in sorted_x[::-1][:10]]
top10_hosts = abnb[abnb.host_id.isin(top10)]
top10_hosts.host_name.value_counts()

# top 10 hosts avg rating

top10_host_numrv= top10_hosts.groupby('host_name')['number_of_reviews'].mean().reset_index()

# top 10 hosts avg price
top10_hosts_price = top10_hosts.groupby('host_name')['price'].mean().reset_index() # Alihan has max number of reviews and average price offered is also very less (110)


# visualization using barh

names,n = top10_host_numrv['host_name'],top10_host_numrv['number_of_reviews'] # names and n are numpy series objects

f,ax = plt.subplots(figsize=(12,8)) # size 12 by 8
ax.barh(names,n)
ax.axvline(abnb.number_of_reviews.mean(),color='r')
ax.set_title('Mean # of Reviews of Top10 Multiple Property Owners',fontsize=20,pad=20)
plt.show()

names,price = top10_hosts_price['host_name'],top10_hosts_price['price']

f,ax = plt.subplots(figsize=(12,8))
ax.barh(names,price)
ax.axvline(abnb.price.mean(),color='r')
ax.set_title('Mean # Prices set by Top10 Multiple Property Owners',fontsize=20,pad=20)
plt.show()


# top 10 most priced location by neighbourhood

tp10_price = abnb.sort_values('price',ascending=False).nlargest(10,'price')

neighbourhood,price = tp10_price['neighbourhood'], tp10_price['price']

f,ax = plt.subplots(figsize=(12,8))
ax.barh(neighbourhood,price)
ax.axvline(abnb.price.mean(),color='r')
ax.set_title('Mean # Prices set by Top neighbourhoods',fontsize=20,pad=20)
plt.show()

# identifying correlation
corr = abnb.corr() 

# Generate a mask for the upper triangle
mask = np.zeros_like(corr, dtype=np.bool)
mask[np.triu_indices_from(mask)] = True
# Set up the matplotlib figure
f, ax = plt.subplots(figsize=(11, 9))
# Generate a custom diverging colormap
cmap = sns.diverging_palette(220, 10, as_cmap=True)
# Draw the heatmap with the mask and correct aspect ratio
sns.heatmap(corr, mask=mask, cmap=cmap, vmax=.3, center=0,
            square=True, linewidths=.5, cbar_kws={"shrink": .5})
plt.show()

# price and neighbourhood relation
f,ax = plt.subplots(figsize=(12,8))
abnb.boxplot(column='price',by='neighbourhood',rot=90,ax=ax)
#ax.set_ylim([0,10000])
plt.suptitle('')
ax.set_title('Price Grouped by Neighbourhood',fontsize = 15)
ax.title.set(y=1.05)
ax.set_ylabel('Price (TL)',fontsize=15)
ax.set_xlabel('',fontsize=15)
plt.show()


# price and room type

f,ax = plt.subplots(figsize=(12,8))
abnb.boxplot(column='price',by='room_type',rot=90,ax=ax)
#ax.set_ylim([0,10000])
plt.suptitle('')
ax.set_title('Price Grouped by Room Type',fontsize = 15)
ax.title.set(y=1.05)
ax.set_ylabel('Price (TL)',fontsize=15)
ax.set_xlabel('',fontsize=15)
plt.show()


# number of reviews and days since last review relation
# create a new column date since last review
abnb['days_since_last_review'] = (abnb.last_review.max()-abnb.last_review).dt.days
f,ax = plt.subplots(figsize=(12,8))
abnb[abnb.price<1000].plot.scatter('days_since_last_review','number_of_reviews',ax=ax)
ax.set_ylabel('Number of Reviewers',fontsize = 15,labelpad=10)
ax.set_xlabel('Days Since Last Review',fontsize = 15,labelpad=10)
ax.set_title('Number of Reviews on a Listing vs. Days Past Since Last Review',fontsize=20,pad=20)
plt.show()

# more the days since last review, lesser the number of reviews


# price vs number of reviews.
f,ax = plt.subplots(figsize=(12,8))
abnb.plot.scatter('number_of_reviews','price',ax=ax)
ax.set_ylabel('Price',fontsize = 15,labelpad=10)
ax.set_xlabel('number_of_reviews',fontsize = 15,labelpad=10)
ax.set_title('Price Vs No. of reviews',fontsize=20,pad=20)
plt.show()

# no definite pattern, however there is a slight relation. very high prices have lower number of reviews.

# neighbourhood, price and reviews_per_month
# neighbourhood and price
nb_price = abnb.groupby('neighbourhood')['price'].agg([np.mean,np.std]).reset_index()
n = nb_price['neighbourhood'].values
m = nb_price['mean'].values
std = nb_price['std'].values

nb_price

f,ax = plt.subplots(figsize=(12,8))
ax.barh(n,m,xerr=std)
ax.set_xlabel('Price (TL)',fontsize=15,x=0.42)
plt.show()

# neighbourhood and no. of reviews
nb_reviews_pmonth = abnb.groupby('neighbourhood')['reviews_per_month'].agg([np.mean,np.std]).reset_index()
n = nb_reviews_pmonth['neighbourhood'].values
m = nb_reviews_pmonth['mean'].values
std = nb_reviews_pmonth['std'].values
mean = m.mean()
nb_reviews_pmonth

f,ax = plt.subplots(figsize=(12,8))
ax.barh(n,m,xerr=std)
ax.set_xlabel('reviews_per_month',fontsize=15,x=0.42)
ax.axvline(x=mean,ls='-.',color='r')
plt.show()


# Top neighbourhoods 

nb_group = abnb.groupby(['neighbourhood'])
nb_abnb = nb_group.agg({'number_of_reviews':['sum','max','mean'],
              'reviews_per_month':['sum','max','mean'],
              'price':['mean','median']}).reset_index()

# a) By median

fig,ax = plt.subplots(figsize=(12,10))
top20nb_price = nb_abnb.sort_values(('price','median'),ascending=True)[['neighbourhood','price']]
ax.barh(top20nb_price.neighbourhood.values,top20nb_price.price['median'].values)
plt.show()

# b) By sum of reviews

fig,ax = plt.subplots(figsize=(12,10))
top20nb_review_sum = nb_abnb.sort_values(('number_of_reviews','sum'),ascending=True)[['neighbourhood','number_of_reviews']]
ax.barh(top20nb_review_sum.neighbourhood.values,top20nb_review_sum.number_of_reviews['sum'].values)
plt.show()


# c. By Mean Monthly Review

fig,ax = plt.subplots(figsize=(12,10))
top20nb_review_monthly_mean = nb_abnb.sort_values(('reviews_per_month','mean'), ascending=True)[['neighbourhood','reviews_per_month']]
ax.barh(top20nb_review_monthly_mean.neighbourhood.values,top20nb_review_monthly_mean.reviews_per_month['mean'].values)
plt.show()



#3 Preprocessing data for ML model prediction

# removing the outliers for price variable

abnb_unbiased = abnb[abnb.price<1000]
abnb_unbiased.shape
#Out[149]: (15638, 16)
abnb_predictors=abnb_unbiased[['room_type','minimum_nights','number_of_reviews','reviews_per_month','calculated_host_listings_count','availability_365']]
abnb_predictors.loc[0:3]

# One hot encoding - for room type as its a categorical column
dummy_roomtype=pd.get_dummies(abnb_predictors['room_type'], prefix='dummy')
abnb_predictors = pd.concat([abnb_predictors,dummy_roomtype],axis=1)
abnb_predictors.drop(['room_type'],axis=1, inplace=True)
abnb_predictors.head(10)

y=abnb_unbiased.price
X_abnb = abnb_predictors.iloc[:, 0:].values
y_abnb = abnb_unbiased.iloc[:,8].values

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X_abnb, y_abnb, test_size = 0.2, random_state = 353)

from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train[:,0:5] = sc.fit_transform(X_train[:, 0:5])
X_test[:,0:5] = sc.fit_transform(X_test[:, 0:5])

from sklearn.linear_model import LinearRegression
regressor = LinearRegression()
regressor.fit(X_train, y_train)

y_pred1 = regressor.predict(X_test)

from sklearn.metrics import r2_score
r2_score(y_test,y_pred1)

# Out[124]: 0.213021150453662

error = pd.DataFrame({'Actual Values': np.array(y_test).flatten(), 'Predicted Values': y_pred1.flatten()})
error.head(10)

