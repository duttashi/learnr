# Linear Regression + EDA, and Normality tests [Linear Regression in R
#clear worspace
rm(list=ls())
#### read data
library(datasets)
data(iris)

### iris data set description:
# https://stat.ethz.ch/R-manual/R-devel/library/iriss/html/iris.html

### Exploratory Data Analysis (EDA)
str(iris)
head(iris)
summary(iris)

### EDA plots
# plot layout: 2 x 2
par(mfcol = c(2,2))
help("par")

# boxplot iris$Sepal.Length
boxplot(iris$Sepal.Length,horizontal = TRUE, xlab="Sepal Length")

# histogram: iris$Sepal.Length
hist(iris$Sepal.Length, main="", xlab="Sepal.Length", prob=T)

# overlay iris$Sepal.Length density function over the empirical distribution
lines(density(iris$Sepal.Length),lty="dashed", lwd=2.5, col="red")

# boxplot iris$Petal.Length
boxplot(iris$Petal.Length,horizontal = TRUE, xlab="Petal Length")
# histogram: iris$Petal.Length,
hist(iris$Petal.Length,main="", xlab="Petal Length", prob=T)
# overlay iris$Petal.Length density function over the empirical distribution
lines(density(iris$Petal.Length),lty="dashed", lwd=2.5, col="red")
