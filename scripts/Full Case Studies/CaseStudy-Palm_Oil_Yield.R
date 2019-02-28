# Data analysis for Palm Oil Yield
# Objective: A team of plantation planners are concerned about the yield of oil palm trees, which seems to
# fluctuate. They have collected a set of data and needed help in analysing on how external
# factors influence fresh fruit bunch (FFB) yield. 
# Script author: Ashish Dutt
# Script create date: 28/2/2019
# Email: ashishdutt@yahoo.com.my

# clean the workspace
rm(list = ls())


# required libraries
library(caret) # for nearZeroVar()

# Read the data
palmoil.data<- read.csv("data/palm_ffb.csv", sep = ",", stringsAsFactors = FALSE)

# PART A: Basic EDA 
dim(palmoil.data) # 130 observations in 9 columns
colnames(palmoil.data)
sum(is.na(palmoil.data)) # Zero missing values
str(palmoil.data) # all numeric columns except for Date
# check for near zero variance in variables
badCols<- nearZeroVar(palmoil.data) # no variable with near zero variance property
