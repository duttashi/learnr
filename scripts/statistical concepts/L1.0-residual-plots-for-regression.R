# Residual Plots for Regression

# Load the data
enroll.data<- read.csv("data/data_simple_regression.csv", header = T, sep = ",")

# check data structure
dim(enroll.data)
str(enroll.data)
names(enroll.data)

#RESIDUALS PLOT
#get unstandardized predicted and residual values
unstandardizedPredicted <- predict(lm.model.1)
unstandardizedResiduals <- resid(lm.model.1)

#get standardized values
standardizedPredicted <- (unstandardizedPredicted - mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)
standardizedResiduals <- (unstandardizedResiduals - mean(unstandardizedResiduals)) / sd(unstandardizedResiduals)

#create standardized residuals plot
plot(standardizedPredicted, standardizedResiduals, xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")
plot(standardizedPredicted, standardizedResiduals, main = "Standardized Residuals Plot", xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")

#add horizontal line
abline(0,0)

 #RESIDUALS HISTOGRAM
#create residuals histogram
hist(standardizedResiduals, freq = FALSE)

 #add normal curve
curve(dnorm, add = TRUE)

#PP PLOT
#get probability distribution for residuals
probDist <- pnorm(standardizedResiduals)

#create PP plot
plot(ppoints(length(standardizedResiduals)), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")

#add diagonal line
abline(0,1)
