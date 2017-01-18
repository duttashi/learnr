# Random Forest

#load required libraries – rpart for classification and regression trees
library(rpart)
#mlbench for Glass dataset
library(mlbench)
#load Glass
data("Glass")
#set seed to ensure reproducible results
set.seed(51)
#split into training and test sets
Glass[,"train"] <- ifelse(runif(nrow(Glass))<0.8,1,0)
#separate training and test sets
trainGlass <- Glass[Glass$train==1,]
testGlass <- Glass[Glass$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainGlass))
#remove train flag column from train and test sets
trainGlass <- trainGlass[,-trainColNum]
testGlass <- testGlass[,-trainColNum]
#get column index of predicted variable in dataset
typeColNum <- grep("Type",names(Glass))
#build model
rpart_model <- rpart(Type ~.,data = trainGlass, method="class")
#plot tree
plot(rpart_model);text(rpart_model)
#…and the moment of truth
rpart_predict <- predict(rpart_model,testGlass[,-typeColNum],type="class")
mean(rpart_predict==testGlass$Type)

# random forest
#build model
library(randomForest)
Glass.rf <- randomForest(Type ~.,data = trainGlass, importance=TRUE, xtest=testGlass[,-typeColNum],ntree=1000)
#Get summary info
Glass.rf

#accuracy for test set
mean(Glass.rf$test$predicted==testGlass$Type)
#confusion matrix
table(Glass.rf$test$predicted,testGlass$Type)

#variable importance plot
varImpPlot(Glass.rf)
