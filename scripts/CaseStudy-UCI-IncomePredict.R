# Case Study: Income prediction
# research question: Predict if the income exceeds >$50K/year based on census data. Also known as "Census Income" dataset.
# data source: http://archive.ics.uci.edu/ml/datasets/Adult

## clear screen
rm(list = ls())

# Import the data from a url
theUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult.data<- read.table(file = theUrl, header = FALSE, sep = ",", 
                        strip.white = TRUE, stringsAsFactors = TRUE,
                        col.names=c("age","workclass","fnlwgt","education","educationnum","maritalstatus",
                          "occupation","relationship","race","sex","capitalgain","capitalloss",
                          "hoursperweek","nativecountry","income")
                        )
adult<- adult.data # make a copy of the data 

# load the required libraries
library(gridExtra) # for grid.arrange()

# Exploratory Data Analysis
## a. Structure 
dim(adult.data)
str(adult.data) # Observations: add column headers; 

# collapse the factor levels and recode level with no name (coded as ?in original data) to missing

levels(adult.data$workclass)<- c("misLevel","FedGov","LocGov","NeverWorked","Private","SelfEmpNotInc",
                                 "SelfEmpInc","StateGov","NoPay")

levels(adult.data$education)<- list(presch=c("Preschool"), primary=c("1st-4th","5th-6th"),
                                    upperprim=c("7th-8th"), highsch=c("9th","Assoc-acdm","Assoc-voc","10th"),
                                    secndrysch=c("11th","12th"), graduate=c("Bachelors","Some-college"),
                                    master=c("Masters"), phd=c("Doctorate"))

levels(adult.data$maritalstatus)<- list(divorce=c("Divorced","Separated"), 
                                           married=c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent"),
                                           notmarried=c("Never-married"), widowed=c("Widowed"))

levels(adult.data$occupation) # missing level name coded as `?`
levels(adult.data$occupation)<- list(misLevel=c("?"), clerical=c("Adm-clerical"), 
                                     lowskillabr=c("Craft-repair","Handlers-cleaners","Machine-op-inspct",
                                                  "Other-service","Priv-house-serv","Prof-specialty",
                                                  "Protective-serv"),
                                     highskillabr=c("Sales","Tech-support","Transport-moving","Armed-Forces"),
                                     agricultr=c("Farming-fishing")
                                     )
table(adult.data$occupation)
table(adult.data$relationship)

levels(adult.data$relationship)<- list(husband=c("Husband"), wife=c("Wife"), outofamily=c("Not-in-family"),
                                       unmarried=c("Unmarried"), relative=c("Other-relative"), 
                                       ownchild=c("Own-child"))

levels(adult.data$race)
levels(adult.data$sex)

levels(adult.data$nativecountry)<- list(misLevel=c("?","South"),SEAsia=c("Vietnam","Laos","Cambodia","Thailand"),
                                           Asia=c("China","India","HongKong","Iran","Philippines","Taiwan"),
                                           NorthAmerica=c("Canada","Cuba","Dominican-Republic","Guatemala","Haiti",
                                                          "Honduras","Jamaica","Mexico","Nicaragua","Puerto-Rico",
                                                          "El-Salvador","United-States"),
                                           SouthAmerica=c("Ecuador","Peru","Columbia","Trinadad&Tobago"),
                                           Europe=c("France","Germany","Greece","Holand-Netherlands","Italy",
                                                    "Hungary","Ireland","Poland","Portugal","Scotland","England",
                                                    "Yugoslavia"),
                                           PacificIslands=c("Japan","France"),
                                           Oceania=c("Outlying-US(Guam-USVI-etc)")
                                           )

levels(adult.data$income)

# check for missing values
colSums(is.na(adult.data)) # missing values in, education(11077) occupation(4066) and native.country(20)
str(adult.data)
# Missing data visualization
library(VIM)
aggr_plot <- aggr(adult.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## Missing data imputation
library(missForest)

str(adult.data) # generally it is advisable not to impute the categorical missing values, if they are less than they should be removed

imputdata<- missForest(adult.data) 
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
adult.cmplt<- imputdata$ximp
aggr_plot <- aggr(adult.cmplt, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# Get the data summary
summary(adult.cmplt) 
# avg age is 38 years, maximum workforce is in private class, graduates are maximum followed by secondaryschool; majority are married; 

# Data Visualization
library(ggplot2)

# Univariate data visualization. Density plots for continuous predictors and bar plots for categorical predictors
str(adult.cmplt)
ggplot(adult.cmplt)+
  geom_density(aes(x=age, fill="red"))

ggplot(adult.cmplt)+
  geom_density(aes(x=capital.gain, fill="red")) # majority data between 0 to <20000
ggplot(adult.cmplt)+
  geom_density(aes(x=capital.loss, fill="red")) # majority data between 0 to 20 and 1000 to 2000
ggplot(adult.cmplt)+
  geom_density(aes(x=hoursperweek, fill="red")) # majority data between 25 to 60
ggplot(adult.cmplt)+
  geom_density(aes(x=education.num , fill="red")) # majority data between 8 to 15



# DATA VISUALIZATION for cleaned data
library(ggplot2)
library(gridExtra)
boxplot (age ~ income, data = adult.cmplt, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "salmon")

incomeBelow50K = (adult.cmplt$income == "<=50K")
xlimit = c (min (adult.cmplt$age), max (adult.cmplt$age))
ylimit = c (0, 1600)

hist1 = qplot (age, data = adult.cmplt[incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = income)

hist2 = qplot (age, data = adult.cmplt[!incomeBelow50K,], margins = TRUE, 
               binwidth = 2, xlim = xlimit, ylim = ylimit, colour = income)

grid.arrange (hist1, hist2, nrow = 2)

# Bar plot for categorical data
# Exploring the workclass, occupation, maritalstatus, relationship and educaton variables
str(adult.cmplt)
qplot(income, data = adult.cmplt, fill = workclass) + facet_grid (. ~ workclass)
qplot(income, data = adult.cmplt, fill = occupation) + facet_grid (. ~ occupation)
qplot(income, data = adult.cmplt, fill = maritalstatus) + facet_grid (. ~ maritalstatus)
qplot(income, data = adult.cmplt, fill = relationship) + facet_grid (. ~ relationship)
qplot(income, data = adult.cmplt, fill = race) + facet_grid (. ~ race)
qplot(income, data = adult.cmplt, fill = nativecountry) + facet_grid (. ~ nativecountry)
qplot(income, data = adult.cmplt, fill = education) + facet_grid (. ~ education)

# Building the prediction model
# https://www.knowbigdata.com/blog/predicting-income-level-analytics-casestudy-r
levels(adult.cmplt$income)<- list(leseq50K=c("<=50K"), gr50K=c(">50K"))

ratio = sample(1:nrow(adult.cmplt), size = 0.25*nrow(adult.cmplt))
test.data = adult.cmplt[ratio,] #Test dataset 25% of total
train.data = adult.cmplt[-ratio,] #Train dataset 75% of total
# scaling quantitative predictors
train.data$fnlwgt<- scale(train.data$fnlwgt)
test.data$fnlwgt<- scale(test.data$fnlwgt)

dim(train.data)
dim(test.data)

# Logistic Regression Model
glm.fit<- glm(income~., family=binomial(link='logit'),data = train.data)
# You will get a warning. This Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred means that the data is possibly linearely separable

summary(glm.fit) # significant predictors are age, workclassSelfEmpInc,fnlwgt,educationnum and maritalstatusmarried
# As for the statistical significant variables, age and educationnum has the lowest p value suggesting a strong association with the response income
# Now we can run the anova() function on the model to analyze the table of deviance
anova(glm.fit, test="Chisq")

glm.pred<- predict(glm.fit, test.data, type = "response")
round(glm.pred,2)
hist(glm.pred, breaks=20)
hist(glm.pred[test.data$income], col="red", breaks=20, add=TRUE)
# check classification performance
table(actual= test.data$income, predicted= glm.pred>0.5)
# classification accuracy is (1384+5655)/8140 which gives an 86% accuracy rate
# Note: if missing categorical data is not imputed, then the logistic regression accuracy is 85% and it increases by 1% when missing data is treated.

##########################

library(randomForest)
library(caret) # for VarImp()
#fit the randomforest model
model.rf <- randomForest(income~., 
                      data = train.data, 
                      importance=TRUE,
                      keep.forest=TRUE
)
print(model.rf)
#what are the important variables (via permutation)
varImpPlot(model.rf, type=1)
#predict the outcome of the testing data
predict<- predict(model.rf, test.data)
# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test.data$income, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy # 66% accuracy

# classification accuracy: https://www.r-bloggers.com/computing-classification-evaluation-metrics-in-r/
library(gbm) # GBM models
str(train.data)
trainX <-train.data[,-15]        # Pull out the dependent variable
testX <- test.data[,-15]
sapply(trainX,summary) # Look at a summary of the training data
str(train.data)
## GENERALIZED BOOSTED RGRESSION MODEL (BGM)  

# Set up training control
ctrl <- trainControl(method = "repeatedcv",   # 10fold cross validation
                     number = 5,							# do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE,
                     allowParallel = TRUE)
# Use the expand.grid to specify the search space	
# Note that the default search grid selects multiple values of each tuning parameter

grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                    n.trees=c(10,20),	        # Num trees to fit
                    shrinkage=c(0.01,0.1),		# Try 2 values for learning rate 
                    n.minobsinnode = 20)

set.seed(1234)  # set the seed
# Set up to do parallel processing
library(doParallel)
registerDoParallel(4)		# Registrer a parallel backend for train
getDoParWorkers()

gbm.tune <- train(x=trainX,y=train.data$income,
                  method = "gbm",
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid=grid,
                  verbose=FALSE)


# Look at the tuning results
# Note that ROC was the performance criterion used to select the optimal model.   

gbm.tune$bestTune
plot(gbm.tune)  		# Plot the performance of the training models
res <- gbm.tune$results
res

### GBM Model Predictions and Performance
# Make predictions using the test data set
gbm.pred <- predict(gbm.tune,testX)

#Look at the confusion matrix  
confusionMatrix(gbm.pred,test.data$income)   # accuracy is 84%

