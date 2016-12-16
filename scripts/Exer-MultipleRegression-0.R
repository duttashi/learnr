# Book: An Introduction to Statistical Learning with Applications in R, Springer ed
# Exercise 9: Use the Multiple Linear regression on Auto dataset
# Date: 16/12/2016

# Load the auto dataset
data<-Auto

# Q1. create a scatterplot matrix for all variables
# A1. scatterplot matrices are a great way to determine if there is a linear correlation between multiple variables
pairs(data)
# In essence, the boxes on the upper right hand side of the whole scatterplot are mirror images of the plots on the lower left hand.
# Clearly mpg~displ, mpg~horsepwr, mpg~weight have a correlation because the plot looks like a line
# To show pairwise relationship between the variables use the ggpairs()
library(GGally)
# reference: https://www.r-bloggers.com/example-9-17-much-better-pairs-plots/
ggpairs(data)

#Q2. Compute the matrix of correlation between the variables using the cor(). You will need to excluse the name variable which is qualitative
#A2.
str(data)
data<-data[,c(1:8)]
str(data)
pairs(cor(data))

#Q3. Use the lm() to perform a multiple linear regression with mpg as the response (dependent) and all other variables except name as the predictors. Use the summary() to print the results.
#comment on the ouput.
#a. Is there are relationship between the predictors and the response
lm.fit<- lm(mpg~., data = data)
summary(lm.fit)
plot(lm.fit)
