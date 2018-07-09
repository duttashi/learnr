# Case Study: Income prediction
# research question: Predict if the income exceeds >$50K/year based on census data. Also known as "Census Income" dataset.
# data source: http://archive.ics.uci.edu/ml/datasets/Adult

## clear screen
rm(list = ls())
## Load the required libraries
library(VIM) # for aggr_plot() in missing data visualization
library(ggplot2) # for boxplot(), densityplot(), barplot(), qplot()
library(rpart)
library(rpart.plot)
library(caret) # for findCorrelation()
library(e1071) # for svm()
library(corrplot) # for corrplot()
library(randomForest) # for randomForest()
library(missForest) # for missForest() in missing data imputation

# Import the data from a url
theUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult.data<- read.table(file = theUrl, header = FALSE, sep = ",", 
                        strip.white = TRUE, stringsAsFactors = TRUE,
                        col.names=c("age","workclass","fnlwgt","education","educationnum","maritalstatus",
                          "occupation","relationship","race","sex","capitalgain","capitalloss",
                        "hoursperweek","nativecountry","income")
                        )
adult<- adult.data # make a copy of the data 

# Exploratory Data Analysis
## a. Structure 
dim(adult.data)
str(adult.data)

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

levels(adult.data$relationship)<- list(husband=c("Husband"), wife=c("Wife"), outofamily=c("Not-in-family"),
                                       unmarried=c("Unmarried"), relative=c("Other-relative"), 
                                       ownchild=c("Own-child"))

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

aggr_plot <- aggr(adult.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern")
                  )

## Missing data imputation

str(adult.data) # generally it is advisable not to impute the categorical missing values, if they are less than they should be removed

#levels(adult.data$income)<- list(less50K=c("<=50K"), gr50K=c(">50K"))

library(missForest)
imputdata<- missForest(adult.data) 
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
adult.cmplt<- imputdata$ximp
df.master<- adult.cmplt # save a copy
aggr_plot <- aggr(adult.cmplt, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# DATA VISUALIZATION for cleaned data
#par(mfrow=c(4,2)) # divide the screen into 4 rows and 1 column
boxplot (age ~ income, data = adult.cmplt, 
         main = "Age distribution for different income levels",
         xlab = "Income Levels", ylab = "Age", col = "salmon")

boxplot (educationnum ~ income, data = adult.cmplt, 
         main = "Does education affect income",
         xlab = "Income Levels", ylab = "Education", col = "salmon")

boxplot (hoursperweek ~ income, data = adult.cmplt, 
         main = "More work hours, more income",
         xlab = "Income Levels", ylab = "Hours per week", col = "salmon")

#dev.off() # reset the graphics device

# Bar plot for categorical data
# Exploring the workclass, occupation, maritalstatus, relationship and educaton variables

qplot(income, data = adult.cmplt, fill = occupation) + facet_grid (. ~ occupation)
qplot(income, data = adult.cmplt, fill = education) + facet_grid (. ~ education)
qplot(income, data = adult.cmplt, fill = income) + facet_grid (. ~ education)

table(adult.cmplt$education, adult.cmplt$occupation, adult.cmplt$income)
qplot(income, data = adult.cmplt, fill = relationship) + facet_grid (. ~ race)

# FEATURE SELECTION 
# Check for skewed variables
skewedVars<- NA
library(moments) # for skewness()
for(i in names(adult.cmplt)){
  if(is.numeric(adult.cmplt[,i])){
    if(i != "income"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(adult.cmplt[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}   # The predictors, `fnlwgt`,`capitalgain` and `capitalloss` are highly skewed as their absolute value is greater than 0.5

## SKEWED VARIABLE TREATMENT
# Post identifying the skewed variables, we proceed to treating them by taking the log transformation.
# But first, we will rearrange the predictors such that the identified high skewed predictors are first followed by other predictors
adult.cmplt<- adult.cmplt[c(3,11:12,1,5,13,2,4,6:10,14:15)]
str(adult.cmplt)
# Next, we treat the skewed variables by log base 2 transformation
# Log transform of the skewed variables
adult.cmplt.norm<- adult.cmplt
adult.cmplt.norm[,1:3]<- log(adult.cmplt[1:3],2) # where 2 is log base 2
# post skewed treatment we notice that capitalgain & capital loss have infinite values so we drop them from subsequent analysis
adult.cmplt.norm$capitalgain<- NULL
adult.cmplt.norm$capitalloss<-NULL

# check for skewed variables again to test if log transformation was sucess or not
skewedVars<- NA

for(i in names(adult.cmplt.norm)){
  if(is.numeric(adult.cmplt.norm[,i])){
    if(i != "income"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(adult.cmplt.norm[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}

## Check for correlation in numeric variables


str(adult.cmplt.norm)

correlat<- cor(adult.cmplt.norm[c(1:4)])

corrplot(correlat, method = "pie")
#Apply correlation filter at 0.70,
highlyCor <- colnames(adult.cmplt.num)[findCorrelation(correlat, cutoff = 0.7, verbose = TRUE)]
highlyCor # No high Correlations found
?corrplot

# PREDICTIVE ANALYTICS

# Building the prediction model
#levels(adult.cmplt$income)<- list(leseq50K=c("<=50K"), gr50K=c(">50K"))

set.seed(1234)
ratio = sample(1:nrow(adult.cmplt), size = 0.25*nrow(adult.cmplt))
test.data = adult.cmplt[ratio,] #Test dataset 25% of total
train.data = adult.cmplt[-ratio,] #Train dataset 75% of total

dim(train.data)
dim(test.data)
str(train.data)
# Logistic Regression Model
glm.fit<- glm(income~., family=binomial(link='logit'),data = train.data)
glm.fit1<- glm(income ~ age + workclass + educationnum + fnlwgt + maritalstatus, family=binomial(link='logit'),data = train.data)
# You will get a warning. This Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred means that the data is possibly linearely separable
summary(glm.fit) # Its evident that the significant predictors are age, workclassSelfEmpInc,fnlwgt,educationnum and maritalstatusmarried
# As for the statistical significant variables, age and educationnum has the lowest p value suggesting a strong association with the response income

# Interpreting the null and residual deviance in the glm fit model
# The null deviance shows how well the response is predicted by the model with nothing but an intercept.
# Deviance is a measure of goodness of fit of a generalized linear model. it’s a measure of badness of fit–higher numbers indicate worse fit. The residual deviance shows how well the response is predicted by the model when the predictors are included. From your example, it can be seen that the residual deviance decreases by 12115 (27001-14886) when 15 predictors were added to it.(note: degrees of freedom = no. of observations – no. of predictors). This decrease in deviance is evidence of significant fit. If the deviance would have increased it would indicate a significant lack of fit. 
# The AIC is 14976. The Akaike Information Criterion (AIC) provides a method for assessing the quality of your model through comparison of related models.  It’s based on the Deviance, but penalizes you for making the model more complicated.  Much like adjusted R-squared, it’s intent is to prevent you from including irrelevant predictors. However, unlike adjusted R-squared, the number itself is not meaningful. If you have more than one similar candidate models (where all of the variables of the simpler model occur in the more complex models), then you should select the model that has the smallest AIC. So AIC is useful for comparing models, but isn’t interpretable on its own.

# Now we can run the anova() function on the model to analyze the table of deviance
anova(glm.fit, glm.fit1, test="Chisq") # By conducting the anova test, it performs the Chi-square test to compare glm.fit and glm.fit1 (i.e. it tests whether reduction in the residual sum of squares are statistically significant or not). The test shows that Model 2 is statistically significant as the p value is less than 0.05. Therefore, the predictors (age + workclass + educationnum + fnlwgt + maritalstatus) are relevant for the model.  See this http://stats.stackexchange.com/questions/172782/how-to-use-r-anova-results-to-select-best-model and http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models/30909#30909 and http://stats.idre.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/
# categorical predictor with several levels: answers like, http://stats.stackexchange.com/questions/146907/principled-way-of-collapsing-categorical-variables-with-many-categories
# http://stats.stackexchange.com/questions/252982/in-what-cases-is-it-ok-to-use-categorical-predictors-with-many-levels-in-regress?noredirect=1&lq=1
# http://stats.stackexchange.com/questions/67938/how-to-handle-categorical-predictors-with-too-many-levels?rq=1

set.seed(1234)
glm.pred<- predict(glm.fit, test.data, type = "response")
glm.pred1<- predict(glm.fit1, test.data, type = "response")

hist(glm.pred, breaks=20)
hist(glm.pred[test.data$income], col="red", breaks=20, add=TRUE)

# check classification performance
table(actual= test.data$income, predicted= glm.pred>0.5)
# Note: if missing categorical data is not imputed, then the logistic regression accuracy is 82%.
(5674+1306)/8140 # 86% accuracy on dirty data 

table(actual= test.data$income, predicted= glm.pred1>0.5)
(5683+987)/8140 # 82% accuracy on model with significant predictors only 

# Decision Tree
str(train.data)
set.seed(1234)
tree.model<- rpart(income~., data=train.data, method="class", minbucket=20)
prp(tree.model)
tree.predict<- predict(tree.model, test.data, type = "class")
confusionMatrix(test.data$income, tree.predict) # 86% accuracy 
(5832+1224)/8140

# Decision Tree with significant predictors only
tree.model1<- rpart(income~age + workclass + educationnum + fnlwgt + maritalstatus, data=train.data, method="class", minbucket=20)
prp(tree.model)
tree.predict1<- predict(tree.model1, test.data, type = "class")
confusionMatrix(test.data$income, tree.predict1) # 82% accuracy 
(5837+820)/8140

# Support Vector Machine
svm.model<- svm(income~., data = train.data,kernel = "radial", cost = 1, gamma = 0.1)
svm.predict <- predict(svm.model, test.data)
confusionMatrix(test.data$income, svm.predict) # 87% accuracy

svm.model1<- svm(income~age + workclass + educationnum + fnlwgt + maritalstatus, data = train.data,kernel = "radial", cost = 1, gamma = 0.1)
svm.predict1 <- predict(svm.model1, test.data)
confusionMatrix(test.data$income, svm.predict1)

# Random Forest
rf.model<- randomForest(income~., 
                        data = train.data, 
                        importance=TRUE,
                        keep.forest=TRUE)
rf.predict <- predict(rf.model, test.data)
confusionMatrix(test.data$income, rf.predict) # 88%

# check the important predictors 
varImpPlot(rf.model, type = 1)

# End of script