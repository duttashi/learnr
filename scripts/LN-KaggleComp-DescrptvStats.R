# Data Source information
# The data is derived from https://www.kaggle.com/aljarah/xAPI-Edu-Data
# clear the workspace
rm(list = ls())
# Load the libraries
library(ggplot2) # data visualization
library(dplyr)

# Load the data
stud.data <- read.table("C:/Users/Ashoo/Downloads/references/New folder/xAPI-Edu-Data.csv",
                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
# A quick peek at the data
dim(stud.data) # 480 rows in 17 columns
names(stud.data) # prints the 17 column headers
str(stud.data)
summary(stud.data)
head(stud.data)
# check for missing data
sum(is.na(stud.data)) # no missing values
# check for zero variance predictors
library(caret)
nzv_cols <- nearZeroVar(stud.data)
nzv_cols # no, near zero variance predictor present

# Check for high correlated predictors 
# Now correlation works only for numeric data. Therefore, I will subset the numeric predictors
stud.data.cont<-stud.data[c(10:13)]
str(stud.data.cont)
stud.data.cor<-cor(stud.data.cont)
stud.data.higCor<-findCorrelation(stud.data.cor, cutoff = 0.80)
str(stud.data.higCor) # none of the continuous predictors are highly correlated




