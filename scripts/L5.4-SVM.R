# Support Vector Machine Example
# load the library e1071 for svm()

## Example 1.0
library(e1071)
# attach the iris data
attach(iris)
# see the first five data points
head(iris,5)

iris.part = subset(iris, Species != 'setosa')

#str(iris.part)
# convert the class or the response variable to factor. By converting the response variable to factor we are performing SVM classification.
# Note: If the response variable was continuous, then it would be SVM Regression.
iris.part$Species = factor(iris.part$Species)
# lets keep only the speal.length, sepal.width and class
iris.part = iris.part[, c(1,2,5)]

# fit a svm model
fit = svm(Species ~ ., data=iris.part, type='C-classification', kernel='linear')
plot(fit, iris.part)

## Example 1.1
day = c(0,1,2,3,4,5,6)
weather = c(1,0,0,0,0,0,0)
happy = factor(c(T,F,F,F,F,F,F))

d = data.frame(day=day, weather=weather, happy=happy)
model = svm(happy ~ day + weather, data = d)
plot(model, d)

## Example 1.2: use the kerlab package
library(kernlab)
model.ksvm = ksvm(happy ~ day + weather, data = d, type="C-svc")
plot(model.ksvm, data=d)

