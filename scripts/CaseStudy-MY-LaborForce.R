# clear the workspace
rm(list=ls())

# Data Source Info: Labor Statistics Data from 1982-2014
# Labour Force and Social Statistic http://www.dosm.gov.my/v1/index.php?r=column3/accordion&menu_id=aHhRYUpWS3B4VXlYaVBOeUF0WFpWUT09
# Load the required libraries
library(ggplot2)
library(plyr) # for the rename ()

#library(tidyr) # for the gather()
library(missForest) # for missForest()

# Load the data
df1<- read.csv("data/bptms-Employed_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2<- read.csv("data/bptms-Labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df3<- read.csv("data/bptms-Labour_Force_Participation_rate_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df4<- read.csv("data/bptms-Outside_labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5<- read.csv("data/bptms-Unemployment_Rate.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df6<- read.csv("data/bptms-Employed_less_than_30_hours.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Exploratory Data Analysis
dim(df1)
dim(df2)
dim(df3)
dim(df4)
dim(df5)


str(df1) # OBSERVATION: remove the comma between the numbers in Employed attribute and change data type to numeric, rename the col header to short names
str(df2) # OBSERVATION: remove the comma between the numbers in Employed attribute and change data type to numeric, rename the col header to short names
str(df3) # OBSERVATION: rename the col header to short names
str(df4) # OBSERVATION: remove the comma between the numbers in Employed attribute and change data type to numeric, rename the col header to short names
str(df5) # OBSERVATION: rename the col header to short names
table(df6$State) # State with 2 levels, one is blank and other is Malaysia. Drop df6. 
rm(df6) # dropped df6

sum(is.na(df1))
sum(is.na(df2))
sum(is.na(df3)) # 29 missing values
colSums(is.na(df3)) # check which col has missing values
sum(is.na(df4))
sum(is.na(df5)) # 29 missing values
colSums(is.na(df5))

# Basic Data Management 

## Renaming the column name. I'm following the CamelCase attribute naming convention
names(df1)
df1<- rename(df1, c("State.Country" = "State"))
df1<- rename(df1, c("Employed...000." = "Employed"))
names(df2)
df2<- rename(df2, c("State.Country" = "State"))
df2<- rename(df2, c("Labour.Force...000." = "LabrFrc"))
names(df3)
df3<- rename(df3, c("State.Country" = "State"))
df3<- rename(df3, c("Labour.Force.Participation.Rate..Percentage." = "LabrFrcPerct"))
names(df4)
df4<- rename(df4, c("State.Country" = "State"))
df4<- rename(df4, c("Outside.Labour.Force...000." = "OutLabrFrc"))
names(df5)
df5<- rename(df5, c("State.Country" = "State"))
df5<- rename(df5, c("Unemployment.Rate..Percentage." = "UnempRatePerct"))

## Change data type
df1$State<- as.factor(df1$State)
df1$Employed<- as.numeric(gsub(",","", df1$Employed))
df2$State<- as.factor(df2$State)
df2$LabrFrc<- as.numeric(gsub(",","", df2$LabrFrc))
df3$State<- as.factor(df3$State)
df4$State<- as.factor(df4$State)
df4$OutLabrFrc<- as.numeric(gsub(",","", df4$OutLabrFrc))
df5$State<- as.factor(df5$State)

## Joining the data frames
## using the dplyr library
library(dplyr)
system.time(join1<- inner_join(df1,df2))
system.time(join2<- inner_join(df3,df4))
system.time(join3<- inner_join(join1,join2))
system.time(df.master<- inner_join(join3,df5))

str(df.master)
# change Year to Factor
#df.master$Year<- as.factor(df.master$Year)

# Missing value treatment
## visualization
library(VIM)
aggr_plot <- aggr(df.master, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df.master), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
colSums(is.na(df.master))

## MISSING DATA IMPUTATION 
imputdata<- missForest(df.master)
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
df.cmplt<- imputdata$ximp
# check for missing values in the new data frame
colSums(is.na(df.cmplt))

# Describe the data or Basic Statistics
library(Hmisc) # for the describe function
describe(df.cmplt)

# Frequency and Contigency tables for categorical data
# One way table
str(df.cmplt)

mytable<- with(data=df.cmplt, table(State))
mytable
# contigency table
mytable<- xtabs(~ State+LabrFrcPerct, data = df.cmplt)
mytable
### Proportion table
prop.table(mytable)

### Percentage table
prop.table(mytable)*100

# Test of independence for the categorical variable
library(vcd) # for xtabs() and assocstats()
mytable<- xtabs(~State+Employed, data= df.cmplt)
chisq.test(mytable) # p value is greater than 0.05, indicating no relationship between state & employed
                    # Note: The warning message is generated because one of the 17 cells (State- some Year) has an expected value less than five, which may invalidate the chi-square approximation.  
mytable<- xtabs(~State+UnempRatePerct, data= df.cmplt)
chisq.test(mytable) # # p value is less than 0.05, indicating a relationship between state & Unemployed rate percent
summary(mytable)

mytable<- xtabs(~State+LabrFrcPerct, data= df.cmplt)
chisq.test(mytable) # # p value is less than 0.05, indicating a relationship between state & labour force percent
# Summary: The significance test conducted using chi-square test of independence evaluates whether or not sufficient evidence exists to reject a null hypothesis of independence between the variables. We could not reject the null hypothesis for State vs Employed, Labour Force and Outside Labour Force. 
# Unfortunately we cannot test the association between the two categorical variables State and Year for our dataset because the measures of association like Phi and Cramer's V require the categorical variables to have at least two levels example "Sex" got two levels, "Male", "Female". For reference purpose, use the assocstats() from the vcd package to test association

# Univariate Data Visualizations
# Histogram's: NO GOOD IN SHOWING CONTINUOUS DATA
ggplot(df.cmplt) +
  geom_histogram(aes(x=Year), fill="gray", binwidth = 20, stat = "bin", position = "stack")

ggplot(df.cmplt) +
  geom_histogram(aes(x=Employed), fill="gray", binwidth = 5000, stat = "bin", position = "stack")

ggplot(df.cmplt) +
  geom_histogram(aes(x=LabrFrc), fill="gray", binwidth = 5000, stat = "bin", position = "stack")
ggplot(df.cmplt) +
  geom_histogram(aes(x=LabrFrcPerct), fill="gray", binwidth = 100, stat = "bin", position = "stack")

# Density plots # FAR BETTER THAN HISTOGRAMS
ggplot(df.cmplt)+
  geom_density(aes(x=Employed, fill="red"))
ggplot(df.cmplt)+
  geom_density(aes(x=LabrFrc, fill="red")) # majority of the labour force is >5000
ggplot(df.cmplt)+
  geom_density(aes(x=LabrFrcPerct, fill="red")) # majority lies between 60-70%
ggplot(df.cmplt)+
  geom_density(aes(x=OutLabrFrc, fill="red")) # majority of outside labour force is >2000
ggplot(df.cmplt)+
  geom_density(aes(x=UnempRatePerct, fill="red")) # majority lies between 2.5 and 5.0
ggplot(df.cmplt)+
  geom_density(aes(x=Year, fill="red")) # twin peaked, first peak at about 1988 and second peak at 2000-2005

# subset the data based on observations from denisty plot
subst.data.1<- subset(df.cmplt, LabrFrc<=1600 & LabrFrcPerct <=70 & UnempRatePerct>=2.5
                   & UnempRatePerct<=5.0)

subst.data.2<- subset(df.cmplt, 
                      (LabrFrcPerct>=60 & LabrFrcPerct <=70) & 
                        (UnempRatePerct>=2.5 & UnempRatePerct<=5.0)
                      )

dim(subst.data.2)
# OBSERVATION: when LabrFrc <= 2000, there were outliers in the data. when LabrFrc<=1600 the outliers were removed

# Boxplot
library(magrittr) # for the pipe operator
library(dplyr) # for select() 
str(subst.data.1)

boxplot(subst.data.1 %>% 
          # Note that the ‘%>%’ (pipe) passes data from the command before to the one after.
          select(Employed,LabrFrc,OutLabrFrc))
boxplot(subst.data.1 %>%
          select(UnempRatePerct))
boxplot(subst.data.1 %>%
          select(LabrFrcPerct))

boxplot(subst.data.2 %>% 
          # Note that the ‘%>%’ (pipe) passes data from the command before to the one after.
          select(Employed,LabrFrc,OutLabrFrc))


#	Line plots- visualizing relationship between two variables
ggplot(subst.data.1)+ geom_line(aes(x=Employed, y=LabrFrc, color="red"))
ggplot(subst.data.1)+ geom_line(aes(x=LabrFrc, y=OutLabrFrc, color="red"))
ggplot(subst.data.1)+ geom_line(aes(x=Employed, y=OutLabrFrc, color="red"))
ggplot(subst.data.1)+ geom_line(aes(x=LabrFrcPerct, y=UnempRatePerct, color="red"))

# Barplot for categorical data visualization
counts<- table(df.cmplt$State)
barplot(counts, main="State Distribution", xlab="Count of States")


## OUTLIER DETECTION ##
# Outliers treatment is a vital part of descriptive analytics since outliers can lead to misleading conclusions regarding our data.
# For continuous variables, the values that lie outside the 1.5 * IQR limits
# For categorical variables, outliers are considered to be the values of which frequency is less than 10%
# outliers gets the extreme most observation from the mean. If you set the argument opposite=TRUE, it fetches from the other side.

## VISUALLY DETECTING OUTLIER using BOXPLOTS
# When reviewing a boxplot, an outlier is defined as a data point that is located outside the fences (“whiskers”) of the boxplot (e.g: outside 1.5 times the interquartile range above the upper quartile and bellow the lower quartile).
# remember, ggplot2 requires both an x and y variable of a boxplot. Here is how to make a single boxplot

p1<-ggplot(data= subst.data.2, aes(x="", y=Employed))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p2<-ggplot(data= subst.data.2, aes(x="", y=LabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p3<-ggplot(data= subst.data.2, aes(x="", y=OutLabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p1+ ggtitle("Employed in Malaysia (1982-2014)")+
  xlab("")+ylab("Employed")
p2+ ggtitle("Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Labour Force")
p3+ ggtitle("Outside Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Outside Labour Force")

# reset the par for graphics
par(mfrow=c(1,1))

# OUTLIER TREATMENT
# subset the data 
subst.data.3<- subset(df.cmplt, 
                      (LabrFrc<=1600 & LabrFrcPerct>=60 & LabrFrcPerct <=70) & 
                        (UnempRatePerct>=2.5 & UnempRatePerct<=5.0)
                      )
dim(subst.data.3)
str(subst.data.3)

p1<-ggplot(data= subst.data.3, aes(x="", y=Employed))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p2<-ggplot(data= subst.data.3, aes(x="", y=LabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p3<-ggplot(data= subst.data.3, aes(x="", y=OutLabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p1+ ggtitle("Employed in Malaysia (1982-2014)")+
  xlab("")+ylab("Employed")
p2+ ggtitle("Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Labour Force")
p3+ ggtitle("Outside Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Outside Labour Force")

## DATA VISUALIZATION for the Subset containing no outlier values to determine relationships
str(subst.data.3)
 
# Univariate Visualization: Plots you can use to understand each attribute standalone.

## bar plot for categorical data
ggplot(data = subst.data.3, aes(x=State))+
  geom_bar(stat = "count", show.legend = TRUE)

## Desnity and Boxplot for continuous data
ggplot(subst.data.3)+
  geom_density(aes(x=Employed, fill="red")) # two-peaked
ggplot(subst.data.3)+
  geom_density(aes(x=LabrFrc, fill="red")) # two-peaked
ggplot(subst.data.3)+
  geom_density(aes(x=LabrFrcPerct, fill="red")) # twin peaked 
ggplot(subst.data.3)+
  geom_density(aes(x=OutLabrFrc, fill="red")) # twin peaked 
ggplot(subst.data.3)+
  geom_density(aes(x=UnempRatePerct, fill="red")) # skewed with a righ tail

par(mfrow=c(1,2))
for(i in 1:2){
  counts<- table(subst.data.3[,i])
  name<- names(subst.data.3[,i])
  barplot(counts, main = name)
}

par(mfrow=c(1,5),col.lab="blue", fg="indianred") # divide the screen into 1 row and five columns
for(i in 3:7){
  boxplot(subst.data.2[,i], main=names(subst.data.3[i]))
}

par(mfrow=c(1,5), col.lab="blue", fg="violetred4") # divide the graphics screen into 1 row and four columns
par(mfrow=c(1,5)) # divide the screen into 1 row and four columns
for(i in 3:7){
  boxplot(subst.data.3[,i], main=names(subst.data.3[i]))
}

# For more colors in R, see this http://research.stowers-institute.org/efg/R/Color/Chart/index.htm
for(i in 3:7){
  plot(density(subst.data.3[,i]), main=names(subst.data.3[i]))
}

# NOTE: RESET GRAPHICAL PARAMETERS TO DEFAULT- In RStudio, You can just navigate to 'Plots' and select 'Remove plots' USE THE BROOM ICON

## SCATTERPLOT MATRIX
# pairwise scatterplot for continuous variables
pairs(subst.data.3[,3:7])
## scatterplot matrix by class
str(subst.data.3)
pairs(State~., data = subst.data.3, col=subst.data.3$State)

# Generate scatterplots with Pearson correlations- BETTER PLOT THAN pairs()
library(psych) #Calls: pairs.panels
pairs.panels(subst.data.3)

## Convert Year to 
temp<- subst.data.3
temp$Year<- as.numeric(temp$Year)

## CORRELATION PLOTS

# We can calculate the correlation between each pair of numeric attributes. These pair-wise correlations can be plotted in a correlation matrix plot to given an idea of which attributes change together.
#LOAD LIBRARY
library(corrplot)
# calculate the correlations
correlations<- cor(subst.data.3[,3:7])
# plot the correlations
corrplot(correlations, method = "number")
# High Correlations: Employed - LaborForce; Employed - OutsideLaborForce; LaborForce - OutsideLaborForce;
# OutsideLaborForce - Employed
# correlation measures the relationship between two variables. When these two variables are so highly correlated that they explain each other (to the point that you can predict the one variable with the other), then we have collinearity (or multicollinearity).
# One way to measure multicollinearity is the variance inflation factor (VIF), which assesses how much the variance of an estimated regression coefficient increases if your predictors are correlated.  If no factors are correlated, the VIFs will all be 1.

## VIF plot for checking multicollinearity
# A simple approach to identify collinearity among explanatory variables is the use of variance inflation factors (VIF).
library(car) # for the vif () 
mod<- lm(UnempRatePerct~., data=subst.data.3[,3:7])
# Use function `vif` to calculate variance inflation factors for each variable in the model
vif(mod)

## MULTICOLLINEARITY TREATMENT
## Method 1: Principal Component Analysis (PCA) reduces the number of predictors to a smaller set of uncorrelated components.
# PCA can be applied only on numerical data. We’ll convert these categorical variables into numeric using one hot encoding.

library(dummies)
## find out how to dummy code the factors to numeric
## reference: 
df.convrtd<- subst.data.3


for(level in unique(subst.data.3$State)){
  df.convrtd[paste("dummy", level, sep = "_")] <- ifelse(subst.data.3$State == level, 1, 0)
}
head(df.convrtd)
df.convrtd$Year<- as.numeric(df.convrtd$Year)
str(df.convrtd)
# drop variable State as its been dummy coded now
df.convrtd$State<-NULL

######## Splitting the dataset into train and test#####
ratio = sample(1:nrow(df.convrtd), size = 0.25*nrow(df.convrtd))
pca.test = df.convrtd[ratio,] #Test dataset 25% of total
pca.train = df.convrtd[-ratio,] #Train dataset 75% of total

#principal component analysis

prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)
biplot(prin_comp, scale = 0)
#compute standard deviation of each principal component
std_dev <- prin_comp$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var[1:10]
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:22]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b") #15 components show max variance

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

train.data <- df.convrtd[,1:15]
library(rpart)
rpart.model <- rpart(UnempRatePerct ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA
test.data <- predict(prin_comp, newdata = pca.test)
test.data <- as.data.frame(test.data)
#select the first 15 components
test.data <- test.data[,1:15]
#make prediction on test data
rpart.prediction <- predict(rpart.model, test.data) # ERROR, object Year not found

### Feature Importance ###
### work done on 15/Feb/2017 ###

library(Boruta)
# run boruta analysis
set.seed(1234)
# pull out the response variable
response <- subst.data.3$UnempRatePerct
bor.results <- Boruta(subst.data.3,response,
                      maxRuns=101,
                      doTrace=0)
bor.results # basically, all the variables are considered important for response variable prediction
cat("\n\nRelevant Attributes:\n")
getSelectedAttributes(bor.results) # "Year"           "State"          "Employed"       "LabrFrc"        "OutLabrFrc"     "UnempRatePerct"
plot(bor.results)

######## Splitting the dataset into train and test#####
ratio = sample(1:nrow(subst.data.3), size = 0.25*nrow(subst.data.3))
Test = subst.data.3[ratio,] #Test dataset 25% of total
Training = subst.data.3[-ratio,] #Train dataset 75% of total
# Evaluation metric function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# Multiple Linear regression
linear.mod<- lm(UnempRatePerct~., data = Training)
summary(linear.mod)
plot(linear.mod, pch=16, which = 1)
predict<- predict(linear.mod, Test)

RMSE0<- RMSE(predict, Test$UnempRatePerct)
RMSE0

# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=Test$UnempRatePerct, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy # 73% prediction accuracy

min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy # 89%

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape # 11%
