# Random Forest example
# In this example, I show both categorical and continuous classification using Random Forest

# Classifcation
require(caTools)
data(mtcars)
set.seed(2018) 
# Split the data into train and test
sample = sample.split(mtcars$mpg, SplitRatio = .75)
train = subset(mtcars, sample == TRUE)
test  = subset(mtcars, sample == FALSE)
# Apply the random forest model
fit<- randomForest( as.factor(gear) ~.,
                    data=train, 
                    importance=TRUE, 
                    ntree=500)
# Look at variable importance
varImpPlot(fit)

letspredict<- predict(fit, test)
finalresult<- data.frame(ActualGear= test$gear, PredictedGear=letspredict)
View(finalresult)
finalresult


# Regression 
require(MASS)
attach(Boston)
str(Boston)
# Split the data into train and test
set.seed(2018) 
sample = sample.split(Boston$medv, SplitRatio = .75)
train = subset(Boston, sample == TRUE)
test  = subset(Boston, sample == FALSE)
fit<- randomForest( medv ~.,
                    data=train, 
                    importance=TRUE, 
                    ntree=500)
# Look at variable importance
varImpPlot(fit)
plot(fit)
# Predicting 
letspredict<- predict(fit, test)
finalresult<- data.frame(ActualHousePrice= test$medv, PredictedHousePrice=letspredict)
# view the result
View(finalresult)
head(finalresult, 5)
