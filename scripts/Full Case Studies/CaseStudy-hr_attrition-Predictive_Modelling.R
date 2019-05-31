# Predictive Modeling for Human Resource analytics 
# Objective: Predictive analytics for Human Resource analytics 
# Script author: Ashish Dutt
# Email: ashishdutt@yahoo.com.my
# Script create date: 13/3/2019
# Script last modified date: 17/3/2019


# clean the workspace
rm(list = ls())

# load the required libraries
library(data.table) # for fread(), data.table()
library(plyr) # for rename()
library(caret)

# read the data
df.train<- fread("data/hr_attrition_train.csv", header = TRUE, stringsAsFactors = TRUE)
df.test<- fread("data/hr_attrition_test.csv", header = TRUE, stringsAsFactors = TRUE)

# data reshaping
df.train<- rename(df.train, c("sales"="role"))
df.train<- rename(df.train, c("time_spend_company"="exp_in_company"))
df.test<- rename(df.test, c("?id"="id"))
df.test<- rename(df.test, c("sales"="role"))
df.test<- rename(df.test, c("time_spend_company"="exp_in_company"))

df.train$role<- mapvalues(df.train$role, from = levels(df.train$role), to=c(1:10))
df.train$salary<- mapvalues(df.train$salary, from = levels(df.train$salary), to=c(1:3))
df.test$role<- mapvalues(df.test$role, from = levels(df.test$role), to=c(1:10))
df.test$salary<- mapvalues(df.test$salary, from = levels(df.test$salary), to=c(1:3))

df.train$left<- as.factor(df.train$left)

### Save the id column in a separate variable
test.id<- df.test$id

### Drop the id columns
df.train$id<- NULL
df.test$id<- NULL

### Data splitting for initial model building
set.seed(2019)
smp_size <- floor(0.75 * nrow(df.train))
# set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(df.train)), size = smp_size)
train <- df.train[train_ind, ]
test <- df.train[-train_ind, ]
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

### Data Modeling
set.seed(2019)
# CART
fit.cart <- train(left~., data=train, method="rpart", metric=metric, trControl=control)
# kNN
fit.knn <- train(left~., data=train, method="knn", metric=metric, trControl=control)
# Logistic Regression
fit.logreg <- train(left~., data=train, method="glm", metric=metric, trControl=control)
# SVM (note: takes some time to complete)
fit.svm <- train(left~., data=train, method="svmRadial", metric=metric, trControl=control)
# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm, logreg=fit.logreg))
summary(results)
# compare accuracy of models
dotplot(results) # best models are knn and cart

# Make Predictions using the best model 
predictions.1 <- predict(fit.cart, test) # #accuracy of 95.5%
confusionMatrix(predictions.1, test$left)  

predictions.2 <- predict(fit.svm, test) #accuracy of 95.5%
confusionMatrix(predictions.2, test$left)  

final_model<- fit.cart

# Add the id column from the df.test dataframe that was saved earlier to the df.test dataframe
df.test$id<- test.id

## Modeling on the supplied test data
model.final<- train(left~., data= df.train, method="rpart", metric=metric, trControl=control)
preds<- predict(model.final, newdata =  df.test, type = "raw")

# write the predicted values to file
pred_vals<- data.frame(cbind(df.test$id, pred=preds))
write.csv(pred_vals, file="data/hr_attrition_predictions.csv", row.names=FALSE)


## Final Summary
# Summary: With all of this information, this is what the management should know about his company and why his employees probably left:
#   
# Employees generally left when they are underworked (less than 150hr/month or 6hr/day)
# Employees generally left when they are overworked (more than 250hr/month or 10hr/day)
# Employees with either really high or low evaluations should be taken into consideration for high turnover rate
# Employees with low to medium salaries are the bulk of employee turnover
# Employees that had 2,6, or 7 project count was at risk of leaving the company
# Employee satisfaction is the highest indicator for employee turnover.
# Employee that had 4 and 5 yearsAtCompany should be taken into consideration for high turnover rate

