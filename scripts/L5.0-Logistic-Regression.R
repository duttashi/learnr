# Logistic Regression
getwd()
# Load the raw training data.titanic and replace missing values with NA
training.data.raw<-read.csv("data/titanic/train.csv", header = TRUE, sep = ",", na.strings = c(""))
# Output the number of missing values for each column
sapply(training.data.raw,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(training.data.raw, function(x) length(unique(x)))

# A visual way to check for missing data.titanic
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

# Subsetting the data.titanic
data.titanic <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

# Substitute the missing values with the average value

data.titanic$Age[is.na(data.titanic$Age)] <- mean(data.titanic$Age,na.rm=T)

# R should automatically code Embarked as a factor(). A factor is R's way of dealing with
# categorical variables
is.factor(data.titanic$Sex)         
is.factor(data.titanic$Embarked)
str(data.titanic)

data.titanic$Sex<-as.factor(data.titanic$Sex)
data.titanic$Embarked<-as.factor(data.titanic$Embarked)
is.factor(data.titanic$Sex)         
is.factor(data.titanic$Embarked)
# Check categorical variables encoding for better understanding of the fitted model
contrasts(data.titanic$Sex)
contrasts(data.titanic$Embarked)

# Remove rows (Embarked) with NAs
data.titanic <- data.titanic[!is.na(data.titanic$Embarked),]
rownames(data.titanic) <- NULL

# Train test splitting
train <- data.titanic[1:800,]
test <- data.titanic[801:889,]

# Model fitting
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

# McFadden R^2
library(pscl)
pR2(model)

#-------------------------------------------------------------------------------
# MEASURING THE PREDICTIVE ABILITY OF THE MODEL

# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata.titanic=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))
