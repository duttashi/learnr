# clear the workspace
rm(list=ls())
# Load the required libraries
library(ggplot2)
library(GGally) # for the ggpairs()
library(corrplot)
library(plyr)
library(mice)
library(car)
library(rpart)
library(randomForest)
library(gbm)
library(moments) # for skewness function
library(tidyr) # for the gather()
# Data source
# Department of Statistics, Malaysia: http://www.dosm.gov.my/v1/index.php?r=column3/accordion&menu_id=aHhRYUpWS3B4VXlYaVBOeUF0WFpWUT09
# load the rubber estate data
df1<- read.csv("data/rubberestate/rubber-paidemployee.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 
df2<- read.csv("data/rubberestate/rubber-plantedarea.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 
df3<- read.csv("data/rubberestate/rubber-production.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 
df4<- read.csv("data/rubberestate/rubber-taparea.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 
df5<- read.csv("data/rubberestate/rubber-yield.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE) 

# Exploratory Data Analysis
dim(df1)
dim(df2)
dim(df3)
dim(df4)
dim(df5)

names(df1) # observation: column name too long. rename them
names(df2) # additional space after column names. do formatting
names(df3)
names(df4)
names(df5)

head(df1) # You cannot have employees in decimals. Round this variable

names(df.master)

# Basic Data Management 
# Renaming the column name
# Reference: http://stackoverflow.com/questions/9283171/named-columns-renaming
df1<- rename(df1, c("Total.Number.of.Paid.Employee.During.the.Last.Pay.Period..Estate." = "TotalPaidEmployee"))
df2<-rename(df2, c("Planted.Area..Estate....000..Hectare" = "AreaPlantedHect"))
df3<-rename(df3, c("Production..Estate....000..Tonne" = "ProduceTonne"))
df4<-rename(df4, c("Tapped.Area..Estate....000..Hectare" = "TapAreaHect"))
df5<-rename(df5, c("Yeild.per.Hectare..Estate...Kg." = "YieldperHectKg"))

# Rounding the column value for TotalPaidEmployee because there cant be for example 2.5 employees
df1$TotalPaidEmployee<- round(df1$TotalPaidEmployee)
# Inner Join the data frames on common column
df.m1<- merge(df1,df2, by="Year")
df.m2<- merge(df3,df4, by="Year")
df.m3<- merge(df.m2, df5, by="Year")
df.master<- merge(df.m1, df.m3, by="Year")
summary(df.master)

# Missing data treatment
colSums(is.na(df.master)) # There is only 1 missing row
tempData <- mice(df.master,m=5,maxit=50,meth='pmm',seed=1234)
df.master<- mice::complete(tempData,1) 
colSums(is.na(df.master))

# Describe the data
library(Hmisc)
describe(df.master)
library(psych)
describe(df.master)
# Basic Data Visualization

# Univariate visualization- Histogram (Continuous data- use density plot, histogram)
ggplot(df.master) +
  geom_histogram(aes(x=AreaPlantedHect), fill="gray", binwidth = 250, stat = "bin", position = "stack")


# density plot
d<- density(df.master$AreaPlantedHect)
plot(d, main = "Kernel Density of Area Planted per Hectare")
polygon(d, col="red", border = "blue")
rug(df.master$AreaPlantedHect, col = "brown")

ggplot(df.master) + geom_density(aes(x=TotalPaidEmployee, colour="red"))
ggplot(df.master) + geom_density(aes(x=AreaPlantedHect))
ggplot(df.master) + geom_density(aes(x=ProduceTonne))
ggplot(df.master) + geom_density(aes(x=TapAreaHect))
ggplot(df.master) + geom_density(aes(x=YieldperHectKg))

# boxplots for continuous variables
library(magrittr) # for the pipe operator
library(dplyr) # for select() 

# Method 1: selecting individual predictor name
boxplot(df.master %>% 
          # Note that the ‘%>%’ (pipe) passes data from the command before to the one after.
          select(AreaPlantedHect,YieldperHectKg,ProduceTonne,TapAreaHect,TotalPaidEmployee))
# Method 2: Use the minus sign before the predictor you dont want to plot such that the remaining predictors are plotted
boxplot(df.master %>%
          select(-Year),
        col = c("red","sienna","palevioletred1","royalblue2","brown"),
        ylab="Count", xlab="Predictors"
        )

#	Line plots- visualizing relationship between two variables
ggplot(df.master)+ geom_line(aes(x=AreaPlantedHect, y=YieldperHectKg, color="red")) # interesting: the yield per hectare decline (after 600 hectares) as planted area size increases
ggplot(df.master)+ geom_line(aes(x=AreaPlantedHect, y=ProduceTonne, color="red")) # produce increases with area but then it begins to decline after 600 hectares
ggplot(df.master)+ geom_line(aes(x=AreaPlantedHect, y=TapAreaHect, color="red")) # there is a positive linear relationship between area planted and tap area
ggplot(df.master)+ geom_line(aes(x=AreaPlantedHect, y=TotalPaidEmployee, color="red")) # Again, a positive linear realtionship between area planted and paid employees but there is a sharp decline at 600 hectares

## Checking for High correlation
cor(df.master$AreaPlantedHect, df.master$YieldperHectKg) # negative correlation, proving the point above that the yield per hectare decreases as plantation size increases
cor(df.master$AreaPlantedHect, df.master$TapAreaHect) # very strong positive correlation
cor(df.master$AreaPlantedHect, df.master$ProduceTonne) # very strong positive correlation
cor(df.master$AreaPlantedHect, df.master$TotalPaidEmployee) # very strong positive correlation, as land size increases more labour is required

correlations<- cor(df.master)
# Visualizing the high correlations
corrplot(correlations, method="number") 
# we can see that vars 'TotalPaidEmployee','AreaPlantedHect','ProduceTonee' and 'TapAreaHect' are high positive correlated.
# Year and YieldPerHect have low positive correlation; TotalPaidEmployee and YieldHect have semi-strong negative correlation;
# AreaPlantedHect and YieldPerHect has strong netaive correlation
# ProduceTonne and YieldperhectKg has low negative correlation
# YieldperHect and TapAreaHect has low negative correlation
highCor<- c("TotalPaidEmployee","AreaPlantedHect","ProduceTonne","TapAreaHect")

# Scatterplot for variables that have high correlations
attach(df.master)
plot(TotalPaidEmployee,AreaPlantedHect) # strong positive correlation
plot(TapAreaHect,ProduceTonne) # strong positive correlation
plot(TotalPaidEmployee, YieldperHectKg) #semi-strong negative correlation
plot(ProduceTonne,YieldperHectKg) # low negative correlation
plot(TapAreaHect,YieldperHectKg) # low negative correlation
plot(Year,AreaPlantedHect) # strong negative correlation
plot(Year,TotalPaidEmployee)# strong negative correlation

# Basic scatterplot matrix
pairs(~., data = df.master, lower.panel=panel.smooth,pch=20, 
      main="Rubber production Scatterplot Matrix") # where panel.smooth can be used to plot a loess curve for each plot in a scatterplot matrix

ggpairs(~., data=df.master)
pm= ggpairs(data = df.master,
            columns = 1:ncol(df.master),
            upper = list(continuous="density"),
            title = "Correlations among predictors"
            )
print(pm)

### Data Transformation: skewed variable treatment

# A variable is considered ‘highly skewed’ if its absolute value is greater than 1.
# A variable is considered ‘moderately skewed’ if its absolute value is greater than 0.5.
skewedVars <- NA

for(i in names(df.master)){
  if(is.numeric(df.master[,i])){
    if(i != "YieldperHectKg"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(df.master[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}
# None of the predictors are skewed

### Feature Importance
library(Boruta)
# run boruta analysis
set.seed(1234)
# pull out the response variable
response <- df.master$YieldperHectKg
bor.results <- Boruta(df.master,response,
                      maxRuns=101,
                      doTrace=0)
bor.results # basically, all the variables are considered important for response variable prediction
cat("\n\nRelevant Attributes:\n")
getSelectedAttributes(bor.results)
plot(bor.results)

# create a sample vector of test values
test.n <- sample(1:nrow(df.master), nrow(df.master)/3, replace = F)
# test dataset
test <- df.master[test.n,]
# train dataset
train <- df.master[-test.n,]

dim(train)
dim(test)

# Evaluation metric function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# Multiple Linear regression
linear.mod<- lm(YieldperHectKg~., data = train)
summary(linear.mod)
plot(linear.mod, pch=16, which = 1)
predict<- predict(linear.mod, test)

RMSE0<- RMSE(predict, test$YieldperHectKg)
RMSE0 # 0.05
plot0 <- predict-test$YieldperHectKg
plot(plot0)

# Regression model with significant predictors only
linear.mod.1<- lm(YieldperHectKg~ TotalPaidEmployee+AreaPlantedHect+Year, data = train)
summary(linear.mod.1)
predict<- predict(linear.mod.1, test)
RMSE0<- RMSE(predict, test$YieldperHectKg)
RMSE0

#### R, Regression Trees, function rpart(), method "anova" ####
model <- rpart(YieldperHectKg ~., data = train, method = "anova")
predict <- predict(model, test)
# RMSE
RMSE1 <- RMSE(predict, test$YieldperHectKg)
RMSE1 <- round(RMSE1, digits = 3)
RMSE1 #0.07
plot1 <- predict-test$YieldperHectKg

### R, Random Forests, function randomForest(), method "anova" ####
help("randomForest")
model.forest <- randomForest(YieldperHectKg ~., data = train, method = "anova",
                      ntree = 300,
                      mtry = 2, #mtry is sqrt(6)
                      replace = F,
                      nodesize = 1,
                      importance = T)
varImpPlot(model.forest) # Look at the IncNodePurity plot. From this plot we see that important vars are year, areaplantedhect. tapareahect. totalpaidemployee
prediction <- predict(model.forest,test)
rmse <- sqrt(mean((log(prediction)-log(test$YieldperHectKg))^2))
round(rmse, digits = 3) # 0.028
# Alternative way to caluclaute RMSE
RMSE2 <- RMSE(predict, test$YieldperHectKg)
RMSE2 <- round(RMSE2, digits = 3) #.029
plot2 <- predict-test$YieldperHectKg
#### CONCLUSION: TO PREDICT Yield per Hectare in Kg
#Regression Tree RMSE: 0.07
#Random Forest RMSE:  0.028
#Random Forest gives the most accutate result in predicting


