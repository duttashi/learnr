# Simple Linear Regression

# Load the library
library(MASS)
library(ISLR)

# load the dataset. from the mass library
fix(Boston)
names(Boston)
help("Boston")
# create a simple regression model on medv as the dependent variable and lstat as the independent variable
# medv= median value of owner-occupied homes in \$1000s.
# lstat= lower status of the population (percent).
lm.fit=lm(medv~lstat, data = Boston)
lm.fit
summary(lm.fit)
plot(Boston$lstat, Boston$medv)
abline(lm.fit, col="red")
par(mfrow=c(2,2)) # The R function par() tells R to split the screen into separate panels so that multiple plots can be viewed simultaneously
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))

# Multiple linear regression
lm.fit=lm(medv~., data = Boston) # here ~. indicates to use all the predictors
summary(lm.fit)
?summary.lm # to access all components of summary.lm
summary(lm.fit)$r.sq # gives the Rsquared
summary(lm.fit)$sigma # gives the RSE

library(car) # to use the VIF()
vif(lm.fit) # VIF is used to detect multicollinearity among the predictors. 
# The smallest possible value of VIF is 1 which indicates complete absence of multicollinearity
# As a rule of thumb (ISLR book), a VIF value that exceeds 5 or 10 ndicates a problematic amount of multicollinearity
# The predictors rad (index of accessibility to radial highways) and tax (full-value property-tax rate per \$10,000.) are highly multicollinear with values greater than 5

# Method 1: remove one of the highly multicolinear variable and then compute the lm and vif
# Reference: http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
# Reference: http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
boston.new<-Boston[,c(1:9,11:14)]  # here dropping the tax variable
lm.fit.new<- lm(medv~.,data = boston.new)
vif(lm.fit.new) # multicolinearity is under control now

# Method 2: Use Partial Least Squares (PLS) or Principal Component Ananlysis (PCA) regression methods that cut the number of predictors to a smaller set of uncorrelated components.
#compute standard deviation of each principal component
#Reference: https://www.analyticsvidhya.com/blog/2016/03/practical-guide-principal-component-analysis-python/
std_dev<-PC$sdev
# compute variance
pr_var<-std_dev^2
#proportion of variance explained
prop_varex<- pr_var/sum(pr_var)
prop_varex
#This shows that first principal component explains 46.7% variance. 
#Second component explains 11% variance. 
#Third component explains 9.6% variance and so on. 
#So, how do we decide how many components should we select for modeling stage ?
#The answer to this question is provided by a scree plot. 
#A scree plot is used to access components or factors which explains the most of variability in the data. It represents values in descending order.
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",type = "b")
# This plot shows that ~ 8 components explain maximum variance in the dataset. In other words we have reduced the predictors from 14 to 4
#Let’s do a confirmation check, by plotting a cumulative variance plot. This will give us a clear picture of number of components.
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
# Select the first 10 predictors
boston.train<-data.frame(medv=Boston$medv,PC$x)
#we are interested in first 10 PCAs
boston.train <- boston.train[,1:10]
#run a decision tree
library(rpart)
rpart.model<- rpart(medv~., data = boston.train, method = "anova")
library(rpart.plot)
plot(rpart.model) # will make a dirty plot
text(rpart.model)
# Reference: http://blog.revolutionanalytics.com/2013/06/plotting-classification-and-regression-trees-with-plotrpart.html
prp(rpart.model) # will make a nice looking plot
prp(rpart.model, varlen = 3) # Shorten variable names
# testing testing
