# Case Study: Income prediction
# research question: Predict if the income exceeds >$50K/year based on census data. Also known as "Census Income" dataset.
# data source: http://archive.ics.uci.edu/ml/datasets/Adult

# Import the data from a url
adult.data<- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"),
                      header = FALSE, sep = ",", stringsAsFactors = TRUE)

# Exploratory Data Analysis
## a. Structure 
dim(adult.data)
str(adult.data) # Observations: add column headers; 
sum(is.na(adult.data)) # No missing values
colSums(is.na(adult.data))
head(adult.data)
# add column header
colnames(adult.data)<- c("age","workclass","fnlwgt","education","education-num","marital-status","occupation",
               "relationship","race","sex","capital-gain","capital-loss","hours-per-week","native-country",
               "income")
str(adult.data)

# explore the factor levels to ensure none have a missing level name

levels(adult.data$workclass) # missing level name coded as `?`
levels(adult.data$education) 
levels(adult.data$`marital-status`)
levels(adult.data$occupation) # missing level name coded as `?`
levels(adult.data$relationship)
levels(adult.data$race)
levels(adult.data$sex)
levels(adult.data$`native-country`) # missing level name coded as `?`
levels(adult.data$income)

# rename the variable names
fix(adult.data)

# recode the ? values as NA 
str(adult.data)
adult.data$workclass[adult.data$workclass==" ?"]<- NA
adult.data$occupation[adult.data$occupation==" ?"]<- NA
adult.data$native.country[adult.data$native.country==" ?"]<- NA

# check for missing values
colSums(is.na(adult.data))

# Missing data visualization
library(VIM)
aggr_plot <- aggr(adult.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## Missing data imputation
library(mice)
adult.data.c<- mice(adult.data, m=5, maxit = 10,method = "pmm", seed = 800) # will give error mice error Error in solve.default(xtx + diag(pen)) : system is computationally singular: reciprocal condition number = 2.7107e-17. This error is generated because The problem with using mice for imputation here is the large number of unbalanced factor variables in this dataset. When these are turned into dummy variables there is a high probability that you will have one column a linear combination of another. Since the default imputation methods involve linear regression, this results in a X matrix that cannot be inverted. One solution is to change the default imputation method to one that is not stochastic. For example: mice(data1, m=1, maxit=500, method='cart', seed=500). Here we use 'cart'(classification and regression trees) as the imputation method. Now R does not need to do any X matrix inversion, and you will not get this error.
adult.data.c<- mice(adult.data, m=1, maxit = 10,method = "cart", seed = 800) # the mice imputation for factor variables takes more than 40 mins. So keep the maxit <=10

adult.cmplt<- mice::complete(adult.data.c,1)
colSums(is.na(adult.cmplt))
