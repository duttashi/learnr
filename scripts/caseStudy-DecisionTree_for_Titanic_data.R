
# Objective: Titanic: Machine Learning using Decision Tree working example

# Clean the workspace environment
rm(list = ls())
# Required libraries

# Download data
# Browse to (https://github.com/duttashi/learnr/blob/master/data/titanic_train_data.csv) and download this file on your computer.
# Browse to (https://github.com/duttashi/learnr/blob/master/data/titanic_test_data.csv) and  download this file on your computer.

# Load data
train.data<- read.csv("data/titanic_train_data.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE, 
                      na.strings = c("","NA"))
test.data<- read.csv("data/titanic_test_data.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE,
                     na.strings = c("","NA"))

# Exploratory Data Analysis
# 1. check data dimension
dim(train.data)
dim(test.data)
#2. check data structure
str(train.data)
str(test.data)
#3. check missing data
sum(is.na(train.data))
sum(is.na(test.data))
#3.1 check which columns have missing data
colSums(is.na(train.data)) 
colSums(is.na(test.data))
# The column `Age` has missing values

## Data Cleaning
# Lets combine the train and test data but you will have to add an additional column to the test.data for the joining to work
test.data$Survived<- 0
titanic.data<- rbind(train.data, test.data)

library(zoo)
# replace the NA with mean in `Age` column
df<- titanic.data
# create a logical index for numeric missing values
logical_index <- sapply(titanic.data, is.numeric)
# use the logical index to replace missing numeric values with mean
titanic.data[logical_index] <- lapply(titanic.data[logical_index], na.aggregate)
# check if imputation done
sum(is.na(titanic.data$Age)) # Sucess

