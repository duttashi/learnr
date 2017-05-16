# clear the workspace
rm(list=ls())
# Objective of this script: EDA of Sberbank Russian Housing Market (https://www.kaggle.com/c/sberbank-russian-housing-market)
# dataset: In this competition, Sberbank is challenging Kagglers to develop algorithms which use a broad spectrum of features to predict realty prices. 


# Load the required libraries
library(ggplot2)
library(tidyverse)
library(corrplot) # for corrplot
library(dplyr) # for select()
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



boxplot(data_train) %>%
  select(full_sq)