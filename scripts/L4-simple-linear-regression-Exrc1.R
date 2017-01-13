# Load the data
enroll.data<- read.csv("data/data_simple_regression.csv", header = T, sep = ",")

# check data structure
dim(enroll.data)
str(enroll.data)
head(enroll.data)
tail(enroll.data)
names(enroll.data)
# Exploratory Data analysis
# Graphical visualization of predictors
# Scatterplot
scatter.smooth(x=enroll.data$HGRAD, y=enroll.data$INC, main="HGRAD ~ Income")  # scatterplot

# Boxplot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(enroll.data$HGRAD, main="HGRAD", sub=paste("Outlier rows: ", paste(boxplot.stats(enroll.data$HGRAD)$out, collapse=" ")))  # box plot for 'HGRAD'
boxplot(enroll.data$INC, main="INCOME", sub=paste("Outlier rows: ", paste(boxplot.stats(enroll.data$INC)$out, collapse=" ")))  # box plot for 'INC'

# Density plot
library(e1071)

par(mfrow=c(1, 2))  # divide graph area in 2 columns
# create density plot for 'speed'
plot(density(enroll.data$HGRAD), main="Density Plot: HGrad", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(enroll.data$HGRAD), 2)))
polygon(density(enroll.data$HGRAD), col="red")

# create density plot for 'dist'
plot(density(enroll.data$INC), main="Density Plot: Income", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(enroll.data$INC), 2)))
polygon(density(enroll.data$INC), col="red")

# Correlation
cor(enroll.data$HGRAD, enroll.data$INC) # high corr 0.82

# Model Building
linearMod <- lm(HGRAD ~ INC, data=enroll.data)

# print the model
print(linearMod)
# summarize the model
summary(linearMod)

# Create a simple linear regression model for predicting the fall enrollment (ROLL) using the unemployment rate (RATE)
lm.model.1<- lm(ROLL~UNEM, data = enroll.data)
# display the linear model
lm.model.1
3957 + 1134 * 9
# summarizing the model
summary(lm.model.1)