# Logistic Regression Exercise
# install the ISLR package and load the stock market data from it
library(ISLR)
attach(Smarket)
# This dataset consists of percentage returns for the S&P 500 stock index over 1,250 days from the begining of year 2001 to the end of year 2005.
# For each date, the percentage of returns for each of the five previous trading days Lag1 through Lag5 are given.
# The variable, 'Volume' is the number of shares tradedon the previous day in billions
# The variable, 'Today' is the percentage return on the date in question
# The variable, 'Direction' is whether the market was'Up' or 'Down' on this date.

# Step 1: Examine the numerical and graphical summaries
# check data dimension and column names
dim(Smarket)
str(Smarket)
names(Smarket)
# check the data summary and make inferences
summary(Smarket)
# Check for correlations 
cor(Smarket) # you will get error, because Direction is a qualitative variable

cor(Smarket[,-9]) # Notice the correlation between the lag variables and the variable 'Today' are close to zero. This means that there does not appear to be any correlation between today's return and previous day returns.
                  # The only substantial correlation is between Year and Volume
plot(Volume) # By plotting the Volume, we see that its increasing over time.

# Now, lets fit a Logistic Regression model to predict Direction using Lag1 through Lag5 and Volume.
# we use the glm() and pass the argument, family=binomial, in order to tell R to run a logistic regression rather than some other type of generalised linear model
glm.fit<- glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, family = binomial)
summary(glm.fit)
# Inferences from the summary for glm.fit 
## The smallest p-value is associated to Lag1 (0.155). The negative coeffecient for this predictor (Lag1= -0.071325) suggest that if the market had a positive return yesterday, it is less likely to go up today. 
## The p-value at 0.155 is stilllarge and show no clear evidence of a relationship between Lag1 and Direction.
coef(glm.fit) # You can also check the coeffecients using the coef() method
summary(glm.fit)$coef # You can also use the summary() to access the coeffecients for the model
glm.probs<- predict(glm.fit, type = "response") # Here the predict() will compute the probabilities for the market going up
glm.probs[1:10]

## To make predictions if market will go up or down on a particular day, we must convert the predicted probabilities into class labels 'Up' or 'Down'

glm.pred<- rep("Down",1250) # This command will create a vector of 1,250 Down elements
glm.pred[glm.probs>0.5]="Up" # This command will transform to 'Up' all of the elements for which predicted probabilities are greater than 0.5
table(glm.pred, Direction) # We use the table(). to produce a confusion matrix to determine how many observations were correctly or incorrectly classified
                          # The diagonal elements of the confusion matrix indicate correct predictions while the off-diagonals indicate incorrect predictions
(116+550)/1250 # Thus, we can say that our model correctly predicted that the market will go up on 550 days and will go down on 116 days.
# And that our logisitic regression model correctly predicted the movement of the market 53% of the time

mean(glm.pred==Direction) # The mean() can also be used to compute the fraction of days for which the prediction was correct

## NOTE: Do not be fooled that you are getting a good prediction rate, because you are using the same dataset both for training and testing purpose. That is why the misclassification rate (100-53) is 47%
## To make accurate predictions, you must divide the dataset into training and testing data and perform predictions on the testing or holdout sample data.
## There are several ways to divide the data into training and testing samples. 
## Here, I will create a vector to contain observations from year 2001 to 2004. The test data will contain observations for the year 2005
train<- (Year<2005)
Smarket.2005<- Smarket[!train,]
dim(Smarket.2005)
Direction.2005<- Direction[!train]

# Fit a logistic model on the train data
glm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
             data=Smarket, family = binomial, subset = train)
glm.probs<- predict(glm.fit, Smarket.2005, type = "response")
glm.pred<- rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
(77+44)/252 # Now, we see that 48% times we predict correctly, but then this value is less than the random guessing of above. So to fine tune the model, you must remove the predictors from the model who do not have any relationship with the response variable
            # So, let's create another model in which we keep only those variables which have small p-values

# Again, compute the glm model to check the p-values
glm.fit<- glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket, family = binomial)
summary(glm.fit)
# remove the predictors with high p-value & recompute the logistic model
glm.fit<- glm(Direction ~Lag1+Lag2, data=Smarket, family = binomial, subset=train)
glm.probs<- predict(glm.fit, Smarket.2005, type = "response")
glm.pred<- rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction.2005)
(35+106)/252 # 56% of predictions are correctly classified which is a great improvement over the previous 48%

