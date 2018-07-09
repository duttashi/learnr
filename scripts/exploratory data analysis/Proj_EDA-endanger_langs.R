# RQ: To analyse and plot the endangered languages of the world.
# Dataset: The dataset was found at https://www.reddit.com/r/datasets/
# Project Motivation: https://www.theguardian.com/news/datablog/2011/apr/15/language-extinct-endangered#data

## Clean the workspace
rm(list = ls())
## Read the data
extlang<-read.csv("data/extinctlangs.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")

# Exploratory data analysis
# check the data structure
dim(extlang) # check the number of rows and cols
str(extlang)
#levels(extlang) # there are 6 levels with 1 empty level
summary(extlang)

# Data Pre-processing

## Task 1.1: rename the column names to make them short
# More info: help("colnames")
colnames(extlang)<-c("LangName","NofSpeakers","EndangerDeg","Lat","Long")
str(extlang)

## Task 1.2: Check for missing data
sum(is.na(extlang)) # will give a total of all missing values
colSums(is.na(extlang)) # will list columnwise mising data. Here we see NofSpeakers has 183 missing values.let us explore this column further
summary(extlang$NofSpeakers)

## Missing data visualization
library(VIM)
aggr_plot <- aggr(extlang, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

library(Amelia)
missmap(extlang, legend = TRUE, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)

### Task 1.2.1: Missing data treatment
## see the post: https://www.kaggle.com/captcalculator/house-prices-advanced-regression-techniques/imputing-missing-data-with-the-mice-package-in-r
## easiest method is to remove the missing data but then you are loosing information
## The function complete.cases() returns a logical vector indicating which cases are complete.
mydata<-extlang[complete.cases(extlang),]
mydata<-na.omit(extlang)
## Since the missing data here is the count of people measured in numeric value, we can impute it with mean.
library(mice)
imp.extlang <- mice(extlang, m=1, method='cart', printFlag=FALSE)
## plot the imputed data to see if it makes some sense
library(lattice)
xyplot(imp.extlang, NofSpeakers ~ Lat) # the imputed data is in red and the actual data is in blue
# Let’s look at the distribution of the imputed data. Does it have a similar distribution to the actual NofSpeakers data
densityplot(imp.extlang, ~NofSpeakers)

# Let’s compare to how this would look if instead we had used a simple mean to replace NAs. Here we do the same as before, except we tell the mice function that we want to use the simple mean for imputing NAs in numeric columns:
imp.extlang.mean <- mice(extlang, 
                         m=1, 
                         defaultMethod=c('mean', 'cart', 'cart', 'cart'),
                         printFlag=FALSE)
xyplot(imp.extlang.mean, NofSpeakers ~ Lat)
densityplot(imp.extlang.mean, ~NofSpeakers)

table(imp.extlang$imp$NofSpeakers) # to see the imputed data

## Lets get the imputed data. 
extlang_complete<- complete(imp.extlang)
sum(is.na(extlang_complete))

### Task 1.3: Check for Near Zero variance predictors
library(caret)
nzv<-nearZeroVar(extlang_complete)
nzv # none found

### 
