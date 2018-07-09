# clear the workspace
rm(list=ls())
# Objective of this script: EDA of Sberbank Russian Housing Market (https://www.kaggle.com/c/sberbank-russian-housing-market)
# dataset: In this competition, Sberbank is challenging Kagglers to develop algorithms which use a broad spectrum of features to predict realty prices. 


# Load the required libraries
library(tidyverse)
library(ggplot2)
library(corrplot) # for corrplot

# Read the data
data_train<- read.csv("data/train.csv", header = TRUE, sep = ",")
dim(data_train) # 30,471 observations in 292 variables

# Check for missing values
sum(is.na(data_train)) #2,61,026 missing values
colSums(is.na(data_train))

#How much data is missing?
miss_pct <- map_dbl(data_train, function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
miss_pct <- miss_pct[miss_pct > 0]

data.frame(miss=miss_pct, var=names(miss_pct), row.names = NULL) %>%
  ggplot(aes(x=reorder(var, -miss), y=miss))+
  geom_bar(stat = "identity", fill="red")+
  labs(x='', y='% missing', title='Percent missing data by feature') +
  theme(axis.text.x=element_text(angle=90, hjust=1))
## Of 292 columns, 51 have missing values. The percentage of values missing ranges from 0.1% in metro_min_walk to 47.4% in hospital_beds_raion.

names(data_train)

# Lets look at the Internal Home characterstics
# variables that are internal to home
internal_home_chars <- c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                    'kitch_sq', 'state', 'price_doc')

corrplot(cor(data_train[,internal_home_chars], use="complete.obs"))
## full_sq(or total area in square meters) is highly correlated with `price`
## full_sq(or total area in square meters) is highly correlated with `number of living rooms`

## Correlation treatment (by subset the data) but first I will plot the high correlated vars
ggplot(aes(x=full_sq, y=price_doc), data = data_train)+
  geom_point(color="red")
## There is an outlier, lets remove and plot the data again
data_train %>%
  filter(full_sq<2000) %>%
  ggplot(aes(x=full_sq, y=price_doc))+
  geom_point(color="red", alpha=0.5)+
  labs(x='Area', y='Price', title='Price by area in sq meters')

names(data_train)

## Lets look at the home built year
table(data_train$build_year) # some nonsense built years like 4965,20052009,0,1,3,20,71,215
## The distribution appears bimodal with a peak somewhere in the early 1970s and somewhere in the past few years.
data_train %>%
  filter(full_sq<2000 & build_year>1690 & build_year<=2018) %>%
  ggplot(aes(x=build_year))+
  geom_histogram(fill="blue", binwidth = 10)+
  ggtitle("Distribution by build year")

## Lets see if the build year and home price are related
data_train %>% 
  filter(build_year > 1691 & build_year < 2018) %>%
  group_by(build_year) %>% 
  summarize(mean_build_price=mean(price_doc)) %>%
  ggplot(aes(x=build_year, y=mean_build_price)) +
  geom_line(stat='identity', color='red') + 
  geom_smooth(color='darkgrey') +
  ggtitle('Mean price by year of build')



  
  
