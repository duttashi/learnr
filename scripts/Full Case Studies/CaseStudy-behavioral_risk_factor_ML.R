# SCRIPT NAME: ML_behavioral_risk_factor.R
# SCRIPT CREATE DATE: 03-Nov-2018
# SCRIPT LAST MODIFIED DATE: 02-Nov-2018

# Required libraries
library(randomForest)
library(caret)


##### Baseline model on partially clean data- the data where the missing values are imputed. And the continuous vars are scaled
# Split the data into 70% training and 30% testing
# Use data frame df.master for initial ML model
# Replace surveydata with df.master
## 75% of the sample size
smp_size <- floor(0.70 * nrow(df.master))
## set the seed to make your partition reproducible
set.seed(2018)
train_ind <- sample(seq_len(nrow(df.master)), size = smp_size)
df.train <- df.master[train_ind, ]
df.test <- df.master[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.train$X<- NULL

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Build models
# reference: https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

# a) nonlinear algorithms
# CART
set.seed(2018)
fit.cart <- train(DIABETE3~., data=df.train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(2018)
fit.knn <- train(DIABETE3~., data=df.train, method="knn", metric=metric, trControl=control)

# b) advanced algorithms
# SVM
# note: the SVM model takes more than 10 minutes to run on dirty data
# set.seed(7)
# fit.svm <- train(DIABETE3~., data=df.train, method="svmRadial", metric=metric, trControl=control)
# Random Forest
# note: the SVM model takes more than >25 minutes to run on dirty data
#set.seed(7)
#fit.rf <- train(DIABETE3~., data=df.train, method="rf", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn))
summary(results)

# compare accuracy of models
dotplot(results)

# Make Predictions
predictions <- predict(fit.cart, df.test)
confusionMatrix(predictions, df.test$DIABETE3)  
# using CART, accuracy is 90%, Kappa is 0.487

predictions <- predict(fit.knn, df.test)
confusionMatrix(predictions, df.test$DIABETE3)  #using KNN accuracy is 85% and kappa is 0.201

# predictions <- predict(fit.svm, df.test)
# confusionMatrix(predictions, df.test$DIABETE3)  #using SVM accuracy is 90% and kappa is 0.486





###### ML models on clean data (using clustering to determine variable importance)
# Use data frame `df.master.final`
# VARIABLE IMPORTANCE
#install.packages("FSelector", dependencies = TRUE)
library(FSelector)
# imp_vars<- gain.ratio(DIABETE3~., data = df.master.final)
# imp_vars
res <- cfs(DIABETE3~., data = df.master.final)
res
impvars<- c("DIFFWALK","AGE","USEEQUIP","HAVARTH3","PREGNANT","EMPLOY1","DIABETE3")
df.master.final.impvars<- df.master.final[,impvars]

# Split the data into 70% training and 30% testing
smp_size <- floor(0.70 * nrow(df.master.final.impvars))
## set the seed to make your partition reproducible
set.seed(2018)
train_ind <- sample(seq_len(nrow(df.master.final.impvars)), size = smp_size)
df.train <- df.master.final.impvars[train_ind, ]
df.test <- df.master.final.impvars[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.test$X<- NULL

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Build models
# reference: https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

set.seed(7)
#fit.lda <- train(DIABETE3~., data=df.train, method="lda", metric=metric, trControl=control)
# a) nonlinear algorithms
# CART
fit.cart <- train(DIABETE3~., data=df.train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(DIABETE3~., data=df.train, method="knn", metric=metric, trControl=control)

# b) advanced algorithms
# SVM
# note: the SVM model takes more than 10 minutes to run on dirty data
# set.seed(7)
# fit.svm <- train(DIABETE3~., data=df.train, method="svmRadial", metric=metric, trControl=control)
# Random Forest
# note: the RF model takes more than >25 minutes to run on dirty data
# set.seed(7)
# fit.rf <- train(DIABETE3~., data=df.train, method="rf", metric=metric, trControl=control)

###### Modelling the clustered data obtained through PCA and MCA

library(FSelector)
# use the data frame obtained after PCA and MCA analysis
res <- cfs(DIABETE3~., data = df.master.final)
res
# adding the response variable at the end of the data frame
impvars<- c("DIFFWALK","AGE","USEEQUIP","HAVARTH3","PREGNANT","EMPLOY1","DIABETE3")
df.master.final.impvars<- df.master.final[,impvars]

# Split the data into 70% training and 30% testing
smp_size <- floor(0.70 * nrow(df.master.final.impvars))
## set the seed to make your partition reproducible
set.seed(2018)
train_ind <- sample(seq_len(nrow(df.master.final.impvars)), size = smp_size)
df.train <- df.master.final.impvars[train_ind, ]
df.test <- df.master.final.impvars[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.test$X<- NULL

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Build models
set.seed(2018)
# a) nonlinear algorithms
# CART
fit.cart <- train(DIABETE3~., data=df.train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(2018)
fit.knn <- train(DIABETE3~., data=df.train, method="knn", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn))
summary(results)

# compare accuracy of models
dotplot(results)

# Make Predictions
predictions <- predict(fit.cart, df.test)
confusionMatrix(predictions, df.test$DIABETE3)  
# using CART, accuracy is 85%, Kappa is 0.009

predictions <- predict(fit.knn, df.test)
confusionMatrix(predictions, df.test$DIABETE3) 
# using KNN, accuracy is 84%, Kappa is -0.001
# predictions <- predict(fit.svm, df.test)
# confusionMatrix(predictions, df.test$DIABETE3)  #using SVM accuracy is 85% and kappa is 0

