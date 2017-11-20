# install the packages
install.packages("titanic")
install.packages('RGtk2',depen=T, type="source")
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

# load the packages
library(titanic)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
# divide the dataset into train and test
# Step 1: check the data dimensions
dim(titanic_train)
str(titanic_train)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=titanic_train,
             method="class")
# lets examine the tree
# Method: 1
plot(fit)
text(fit)
# Hmm, not very pretty or insightful. To get some more informative graphics, you will need to install some external packages.
fancyRpartPlot(fit)

# Make prediction
Prediction<- predict(fit, titanic_test, type="class")
# view predictions by creating a readable data frame
ViewPredictions<- data.frame(PassengerID= titanic_test$PassengerId, Survived= Prediction)
# view the predictions
ViewPredictions
