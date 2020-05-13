
# Objective: Titanic: Machine Learning using Decision Tree working example

# Clean the workspace environment
rm(list = ls())
# Required libraries

# Download data
# Browse to (https://github.com/duttashi/learnr/blob/master/data/titanic_train_data.csv) and download this file on your computer.
# Browse to (https://github.com/duttashi/learnr/blob/master/data/titanic_test_data.csv) and  download this file on your computer.

# Load data
train.data<- read.csv("data/titanic_train_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE, 
                      na.strings = c("","NA"))
test.data<- read.csv("data/titanic_test_data.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE,
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

# Visualize the missing data pattern
library(VIM)
aggr_plot <- aggr(titanic.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(titanic.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
# if you get an error message, Error in plot.aggr(res, ...) :  (converted from warning) not enough horizontal space to display frequencies
# Then you need to increase the plot space in RStudio

# impute missing data using the mice package
# install the package mice, if not already installed
# install.packages("mice", dependencies = TRUE)
# load the package in the environment
library(mice)
# imputation for continuous data using predictive mean matching (pmm)
tempdata<- mice(titanic.data[,-c(11,12)], m=5, maxit=10, meth='pmm', seed=2018)
tempdata.cmplt<- complete(tempdata,2)
# check for missing data
colSums(is.na(tempdata.cmplt))
str(tempdata.cmplt)
# Drop Ticket from the data frame
tempdata.cmplt$Ticket<- NULL

# Divide the imputed data into training and test 

# Method # 1: Manual method and its not statistically correct
titanic.train<- tempdata.cmplt[1:891,]
titanic.test<- tempdata.cmplt[892:1309,]
table(titanic.test$Survived)
# notice that titanic.test$Survived has only 1 level which is wrong

# Method # 2: Use the sample approach
# There are numerous approaches to achieve data partition. For a more complete approach take a look at the createDataPartition function in the caret package
## 75% of the sample size
smp_size <- floor(0.75 * nrow(titanic.data))
## set the seed to make your partition reproductible
set.seed(2018)
train_ind <- sample(seq_len(nrow(titanic.data)), size = smp_size, replace = FALSE)
# split the data
titanic.train <- titanic.data[train_ind, ]
titanic.test <- titanic.data[-train_ind, ]

# Model Building- Decision Tree
# Create a new model `my_tree`
# install.packages("rpart", dependencies=TRUE)
library(rpart) # for rpart()
library(rpart.plot) # for function prp()
my_tree <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch , 
                 data = titanic.train, method = "class")
summary(my_tree)
# Visualize the decision Tree
prp(my_tree, type = 4, extra = 100)

# Make predictions
my_prediction <- predict(my_tree, titanic.test, type = "class")
#  check the confusion matrix
table(my_prediction, titanic.test$Survived)

# To interpret the confusion matrix see http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
# Accuracy: Overall, how often is the classifier correct?
#       (TP+TN)/total = (216+35)/328 = 76%
# Misclassification Rate: Overall, how often is it wrong? its also equivalent to 1-Accuracy
#       (FP+FN)/total = (33+44)/328= 23%
# Specificity: When it's actually no, how often does it predict no?
#       Actual No= (216+44)=260 ; Actual Yes= 33+35=68; Predicted No=(216+33)= 249; Predicted Yes=(44+35)=79
# Specificity= TN/actual no = (216/260)=83%