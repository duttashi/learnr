# Regression                                                 #
# requires packages car, gvlma, MASS, leaps to be installed  #
# install.packages(c("car", "gvlma", "MASS", "leaps"))       #
#------------------------------------------------------------#
par(ask=TRUE)
opar <- par(no.readonly=TRUE)

# Simple linear regression
fit <- lm(weight ~ height, data=women)
summary(fit)
women$weight
fitted(fit)
residuals(fit)
plot(women$height,women$weight,
     main="Women Age 30-39", 
     xlab="Height (in inches)", 
     ylab="Weight (in pounds)")
# add the line of best fit
abline(fit)
# Polynomial regression
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
plot(women$height,women$weight,
     main="Women Age 30-39",
     xlab="Height (in inches)",
     ylab="Weight (in lbs)")
lines(women$height,fitted(fit2))

# Enhanced scatterplot for women data
library(car)
scatterplot(weight ~ height, data=women,
            spread=FALSE, smoother.args=list(lty=2), pch=19,
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")

# Examining bivariate relationships
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
cor(states)
library(car)
scatterplotMatrix(states, spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix")

# Multiple linear regression
states <- as.data.frame(state.x77[,c("Murder", "Population", 
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

# Mutiple linear regression with a significant interaction term
fit <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(fit)

library(effects)
plot(effect("hp:wt", fit,, list(wt=c(2.2, 3.2, 4.2))), multiline=TRUE)

# simple regression diagnostics
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
newfit <- lm(weight ~ height + I(height^2), data=women)
par(opar)
par(mfrow=c(2,2))
plot(newfit)
par(opar)

# # basic regression diagnostics for states data
opar <- par(no.readonly=TRUE)
fit <- lm(weight ~ height, data=women)
par(mfrow=c(2,2))
plot(fit)
par(opar)

fit2 <- lm(weight ~ height + I(height^2), data=women)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(fit2)
par(opar)

# Assessing normality
library(car)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy", "Income", "Frost")])
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
