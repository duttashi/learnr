# Building a Decision Tree when the response/predicted variable is discrete in nature

library(mlbench)
library(rpart)
library(rpart.plot) # for fancyrplot()

# load the ionosphere data
data("Ionosphere")
#set seed to ensure reproducible results
set.seed(51)
#split into training and test sets
Ionosphere[,"train"] <- ifelse(runif(nrow(Ionosphere))<0.8,1,0)
#separate training and test sets
trainset <- Ionosphere[Ionosphere$train==1,]
testset <- Ionosphere[Ionosphere$train==0,]
#get column index of train flag
trainColNum <- grep("train",names(trainset))
#remove train flag column from train and test sets
trainset <- trainset[,-trainColNum]
testset <- testset[,-trainColNum]

#get column index of predicted variable in dataset
typeColNum <- grep("Class",names(Ionosphere))
#build model
rpart_model <- rpart(Class~.,data = trainset, method="class")
#plot tree
plot(rpart_model);text(rpart_model)

rpart_predict <- predict(rpart_model,testset[,-typeColNum],type="class")
mean(rpart_predict==testset$Class)
#confusion matrix
table(pred=rpart_predict,true=testset$Class)
#cost-complexity pruning
printcp(rpart_model)
# get index of CP with lowest xerror
opt <- which.min(rpart_model$cptable[,"xerror"])
#get its value
cp <- rpart_model$cptable[opt, "CP"]
#prune tree
pruned_model <- prune(rpart_model,cp)
#plot tree
plot(pruned_model);text(pruned_model)

#find proportion of correct predictions using test set
rpart_pruned_predict <- predict(pruned_model,testset[,-typeColNum],type="class")
mean(rpart_pruned_predict==testset$Class)

# End of code

