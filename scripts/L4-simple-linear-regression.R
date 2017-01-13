# Simple Linear Regression

# check data structure
dim(cars) # 50 rows, 2 cols
str(cars) # speed, dist are quantitaive variables
head(cars)
tail(cars)
names(cars) # will print the predictor names

# Exploratory Data analysis
# Graphical visualization of predictors
# Scatterplot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
# Boxplot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", paste(boxplot.stats(cars$speed)$out, collapse=" ")))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", paste(boxplot.stats(cars$dist)$out, collapse=" ")))  # box plot for 'distance'

# Density plot
library(e1071)

par(mfrow=c(1, 2))  # divide graph area in 2 columns
# create density plot for 'speed'
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col="red")

# create density plot for 'dist'
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))
polygon(density(cars$dist), col="red")

# Correlation
cor(cars$speed, cars$dist)  # calculate correlation between speed and distance 

# Model Building
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data

# print the model
print(linearMod)
# summarize the model
summary(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value

t_value
p_value

f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

# AIC and BIC
AIC(linearMod)  # AIC => 419.1569
BIC(linearMod)  # BIC => 424.8929

# Predicting with Linear Models
# Create Training and Test data -------------------------------------------------------
set.seed(90)  # setting seed to reproduce results of random sampling

trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row incices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

### Build the model on training data ----------------------------------------------------
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

### Review Diagnostic Measures
summary (lmMod)  # model summary
# Calculate: akaike information criterion
AIC (lmMod)  #338.4489

## Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

# min_max accuracy
min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape

# Cross validate on random samples (k-Fold cross validation)
library(DAAG)
par(new=TRUE); par(bg="aliceblue"); par(mar=c(4,4,4,4))  # set margins
# Check if the dashed lines are parallel and small and big symbols are not over dispersed for any one specific color.
CrossVal<- CVlm(data=cars,form.lm=dist ~ speed,m=5, dots=FALSE, seed=29, legend.pos="topleft",
                main="Small symbols are predicted values while bigger ones are actuals.")
attr(CrossVal, 'ms')


# Linear regression Diagnostics
# Variance Inflation factor
library(car)
vif(linearMod)
# Check for Autocorrelation of residuals
durbinWatsonTest(linearMod)




