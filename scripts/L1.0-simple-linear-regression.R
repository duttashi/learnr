# Simple Linear Regression

# Load the data
enroll.data<- read.csv("data/data_simple_regression.csv", header = T, sep = ",")

# check data structure
dim(enroll.data)
str(enroll.data)
names(enroll.data)
# Create a simple linear regression model for predicting the fall enrollment (ROLL) using the unemployment rate (RATE)
lm.model.1<- lm(ROLL~UNEM, data = enroll.data)
# display the linear model
lm.model.1
3957 + 1134 * 9
# summarizing the model
summary(lm.model.1)


