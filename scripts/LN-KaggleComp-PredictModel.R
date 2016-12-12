# Data Source information
# Data Source: https://www.kaggle.com/aljarah/xAPI-Edu-Data
# clear the workspace
rm(list = ls())
# Load the data
stud.data <- read.table("C:/Users/Ashoo/Downloads/references/New folder/xAPI-Edu-Data.csv",
                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
# load the libraries
library(caret)
library(caTools)
library(rpart)
library(rpart.plot)
# split the data into train and cross validation sets
set.seed(28)
split <- sample.split(stud.data$Class, SplitRatio = 0.75)
train <- subset(stud.data, split == T)
cv <- subset(stud.data, split == F)

# Decision Trees
tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 1)
prp(tree.model) 
tree.predict <- predict(tree.model, cv, type = "class")
table(cv$Class, tree.predict)
