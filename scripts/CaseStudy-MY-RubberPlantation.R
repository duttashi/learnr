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
library(VIM) # for missing data visualization
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
aggr_plot <- aggr(df.master, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df.master), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

colSums(is.na(df.master)) # There is only 1 missing row
tempData <- mice(df.master,m=5,maxit=50,meth='pmm',seed=1234)
df.master<- mice::complete(tempData,1) 
colSums(is.na(df.master))

# Describe the data
library(Hmisc)
describe(df.master)
library(psych)
describe(df.master)

######### Data Visualization

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

## Checking relationship between TotalPaidEmployee, AreaPlantedHect, ProduceTonne, TapAreaHect
plot1<-ggpairs(data=Training, columns=2:5,
               mapping = aes(color = "dark green"),
               axisLabels="show")
plot1

boxplot1=boxplot(YieldperHectKg~TapAreaHect, data = Training, 
                 col=(c("gold","darkgreen")),
                 main="Yield (Hectare) vs. Rubber tapped area (Hectare)", 
                 xlab="Rubber tapped area (Hectare)", ylab="Yield (Hectare)")

library(RColorBrewer)
#we will select the first 4 colors in the Set1 palette
cols<-brewer.pal(n=4,name="Set1")
#cols contain the names of four different colors

plot(Training$AreaPlantedHect, Training$YieldperHectKg, pch=16, col=cols,
     main=" Does larger plantation area yield more rubber?",
     xlab = "Area planted (in hectare)",
     ylab = "Yield in Kg (per hectare)"
     )


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

######## Splitting the dataset into train and test#####
ratio = sample(1:nrow(df.master), size = 0.25*nrow(df.master))
Test = df.master[ratio,] #Test dataset 25% of total
Training = df.master[-ratio,] #Train dataset 75% of total

dim(Training)
dim(Test)

# Evaluation metric function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# Multiple Linear regression
linear.mod<- lm(YieldperHectKg~., data = Training)
summary(linear.mod)
plot(linear.mod, pch=16, which = 1)
predict<- predict(linear.mod, Test)

RMSE0<- RMSE(predict, Test$YieldperHectKg)
RMSE0

# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=Test$YieldperHectKg, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# Model Diagnostics
# Check the AIC and BIC
AIC(linear.mod)
BIC(linear.mod)

## creating another model to compare the AIC and BIC
linear.mod1<- lm(YieldperHectKg~ProduceTonne+TapAreaHect, data = Training)
AIC(linear.mod1)
BIC(linear.mod1)

# Regression model with significant predictors only
linear.mod.1<- lm(YieldperHectKg~ TotalPaidEmployee+AreaPlantedHect+Year, data = train)
summary(linear.mod.1)

predict<- predict(linear.mod, Test)
RMSE0<- RMSE(predict, Test$YieldperHectKg)
RMSE0



#### R, Regression Trees, function rpart(), method "anova" ####
model <- rpart(YieldperHectKg ~., data = Training, method = "anova")
predict <- predict(model, Test)
# RMSE
RMSE1 <- RMSE(predict, Test$YieldperHectKg)
RMSE1 <- round(RMSE1, digits = 3)
RMSE1 #0.098
plot1 <- predict-Test$YieldperHectKg

### R, Random Forests, function randomForest(), method "anova" ####
help("randomForest")
model.forest <- randomForest(YieldperHectKg ~., data = Training, method = "anova",
                      ntree = 300,
                      mtry = 2, #mtry is sqrt(6)
                      replace = F,
                      nodesize = 1,
                      importance = T)
varImpPlot(model.forest) # Look at the IncNodePurity plot. From this plot we see that important vars are `TotalPaidEmployee`, `ProduceTonne` and `TapAreaHect`
prediction <- predict(model.forest,Test)
rmse <- sqrt(mean((log(prediction)-log(Test$YieldperHectKg))^2))
round(rmse, digits = 3) # 0.049

# Alternative way to caluclaute RMSE
RMSE2 <- RMSE(predict, Test$YieldperHectKg)
RMSE2 <- round(RMSE2, digits = 3) #.098
plot2 <- predict-Test$YieldperHectKg
#### CONCLUSION: TO PREDICT Yield per Hectare in Kg
# Linear Regression: 0.04533296
#Regression Tree RMSE: 0.098
#Random Forest RMSE:  0.049
#Regression Tree gives the most accutate result in predicting


