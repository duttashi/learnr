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
df1<- read.csv("data/misc/bptms-Employed_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2<- read.csv("data/misc/bptms-Labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df3<- read.csv("data/misc/bptms-Labour_Force_Participation_rate_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df4<- read.csv("data/misc/bptms-Outside_labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5<- read.csv("data/misc/bptms-Unemployment_Rate.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
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
str(df.cmplt)


#### DETERMINE RELATIONSHIP BETWEEN VARIABLES ######
library(vcd) # for xtabs() and assocstats()
str(df.complete)

mytable<- xtabs(~State+OutLabrFrc, data= df.complete)

assocstats(mytable) # since pearsons chi-square p value = 0.15 which is greater than 0.05 indicating no relationship between state & employed 
mytable<- xtabs(~State+LabrFrcPerct, data= df.complete)
assocstats(mytable) # since pearsons chi-square p value = 0.00000049 which is less than 0.05 indicating relationship between state & Outside Labor Force 

# 
# 
# ## FACTOR TO NUMERIC CONVERSION ##
# str(df.cmplt)
# levels(df.cmplt$State)
# table(df.cmplt$State)
# 
# df.cmplt$State<-as.factor(gsub("W.P.Putrajaya","Putrajaya", df.cmplt$State,ignore.case=T))
# df.cmplt$State<-as.factor(gsub("W.P. Kuala Lumpur","Kuala Lumpur", df.cmplt$State,ignore.case=T))
# df.cmplt$State<-as.factor(gsub("W.P Labuan","Labuan", df.cmplt$State,ignore.case=T))
# df.cmplt$State<- as.numeric(df.cmplt$State)

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
# subset the data and name it as subst.data.3

subst.data.3<- subset(df.cmplt, 
                      (LabrFrc<=1200 & LabrFrcPerct>=60 & LabrFrcPerct <=70) & 
                        (UnempRatePerct>=2.5 & UnempRatePerct<=5.0)
                      )

## DATA VISUALIZATION for the Subset containing no outlier values to determine relationships
str(subst.data.3)
library(ggplot2)
p1<-ggplot(data= df.cmplt, aes(x="", y=Employed))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p2<-ggplot(data= df.cmplt, aes(x="", y=LabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p3<-ggplot(data= df.cmplt, aes(x="", y=OutLabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p1+ ggtitle("Employed in Malaysia (1982-2014)")+
  xlab("")+ylab("Employed")
p2+ ggtitle("Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Labour Force")
p3+ ggtitle("Outside Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Outside Labour Force")

# Univariate Visualization: Plots you can use to understand each attribute standalone.

## bar plot for categorical data
ggplot(data = subst.data.3, aes(x=State))+
  geom_bar(stat = "count", show.legend = TRUE)

## Density and Boxplot for continuous data
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

## FACTOR TO NUMERIC CONVERSION ##
str(subst.data.3)
levels(subst.data.3$State)

subst.data.3$State<-as.factor(gsub("W.P.Putrajaya","Putrajaya", subst.data.3$State,ignore.case=T))
subst.data.3$State<-as.factor(gsub("W.P. Kuala Lumpur","Kuala Lumpur", subst.data.3$State,ignore.case=T))
subst.data.3$State<-as.factor(gsub("W.P Labuan","Labuan", subst.data.3$State,ignore.case=T))
subst.data.3$State<- as.numeric(subst.data.3$State)
table(subst.data.3$State)

### Data Transformation: skewed variable treatment
library(moments) # for skewness function
# A variable is considered ‘highly skewed’ if its absolute value is greater than 1.
# A variable is considered ‘moderately skewed’ if its absolute value is greater than 0.5.
skewedVars <- NA

for(i in names(df.cmplt)){
  if(is.numeric(df.cmplt[,i])){
    if(i != "UnempRatePerct"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(df.cmplt[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}
# We find that the variables, `Employed`, `LabrFrc` and `OutLabrFrc` are highly skewed.

##### SKEWED VARIABLE TREATMENT

## reorder the columns in df.cmplt data frame
df.cmplt<- df.cmplt[c(1:2,4:5,3,6:7)]
str(df.cmplt)
# Log transform the skewed variables
df.cmplt.norm<-df.cmplt
str(df.cmplt.norm)

df.cmplt.norm[,3:7]<- log(df.cmplt[3:7],2) # where 2 is log base 2
# check for skewness again
for(i in names(df.cmplt.norm)){
  if(is.numeric(df.cmplt.norm[,i])){
    if(i != "UnempRatePerct"){
      # Enters this block if variable is non-categorical
      skewVal <- skewness(df.cmplt.norm[,i])
      print(paste(i, skewVal, sep = ": "))
      if(abs(skewVal) > 0.5){
        skewedVars <- c(skewedVars, i)
      }
    }
  }
}

## CORRELATION PLOTS

# We can calculate the correlation between each pair of numeric attributes. These pair-wise correlations can be plotted in a correlation matrix plot to given an idea of which attributes change together.
#LOAD LIBRARY
library(corrplot)
# calculate the correlations
correlations<- cor(df.cmplt.norm)

# plot the correlations
corrplot(correlations, method = "number")

# High Correlations: Employed - LaborForce; Employed - OutsideLaborForce; LaborForce - OutsideLaborForce;
# OutsideLaborForce - Employed
# correlation measures the relationship between two variables. When these two variables are so highly correlated that they explain each other (to the point that you can predict the one variable with the other), then we have collinearity (or multicollinearity).
# One way to measure multicollinearity is the variance inflation factor (VIF), which assesses how much the variance of an estimated regression coefficient increases if your predictors are correlated.  If no factors are correlated, the VIFs will all be 1.

## VIF plot for checking multicollinearity
# A simple approach to identify collinearity among explanatory variables is the use of variance inflation factors (VIF).
library(DAAG) # for the vif () 
mod<- lm(Employed~., data=df.cmplt.norm)
# Use function `vif` to calculate variance inflation factors for each variable in the model
vfit<-vif(mod)
sqrt(vif(mod)) > 2

## MULTICOLLINEARITY TREATMENT

## Method: Principal Component Analysis (PCA) reduces the number of predictors to a smaller set of uncorrelated components.
# PCA can be applied only on numerical data. 
library(stats) # for princomp()
df.cmplt.norm.pca<- princomp(df.cmplt.norm, cor = TRUE)
summary(df.cmplt.norm.pca) # Here, Comp.1 explains 44% variance, Comp.2 explains 20 variance and so on. Also we can see that Comp.1 to Comp.5 have the highest standard deviation

# Plotting
biplot(df.cmplt.norm.pca) # Notice the closeness of the arrows for variables, `OutLabrFrc`,`Employed` and `LabrFrc` indicates strong correlation. Again, notice the mild closeness of arrows for variable `LabrFrcPerct`,`State` and `UnempRatePerct` indicate mild correlation. Finally, notice the perpendicular distance between variables, `Year` and `OutLabrFrc` indicates no correlation.
# http://stackoverflow.com/questions/12760108/principal-components-analysis-how-to-get-the-contribution-of-each-paramete

##########################################
#compute variance
std_dev<- df.cmplt.norm.pca$sdev
df.cmplt.norm.pca.var<- std_dev^2
round(df.cmplt.norm.pca.var)
#proportion of variance explained
prop_varex <- df.cmplt.norm.pca.var/sum(df.cmplt.norm.pca.var)
round(prop_varex,3)
#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b") # 5 principal components

biplot(df.cmplt.norm.pca, scale = 0)

######################################

df.cmplt.norm.pca$loadings[1:5,1:5]
df.cmplt.norm.pca$loadings # the relevant data is in the loadings component. If using the prcomp method then the relevant loadings are in the rotation component
# If you want this as a relative contribution then sum up the loadings per column and express each loading as a proportion of the column (loading) sum, taking care to use the absolute values to account for negative loadings.

load <- with(df.cmplt.norm.pca, unclass(loadings))
round(load,3)

# This final step then yields the proportional contribution to the each principal component
aload <- abs(load) ## save absolute values

round(sweep(aload, 2, colSums(aload), "/"),3)
colSums(sweep(aload, 2, colSums(aload), "/"))

##########################################

# To make inference from the biplot above, focus on the extreme ends (top, bottom, left, right) of this graph.
# We infer than first principal component corresponds to a measure of UnempRatePerct, State, LabrFrcPerct, the second principal component refers to Employed, LaborForce, Outside Labor Force

# Therefore, the principal components to retain for further modeling are the variables, Employed, LabrFrc, OutLabrFrc, Year and LabrFrcPerct
vars_to_retain<- c("Year","Employed","UnempRatePerct","LabrFrc","LabrFrcPerct","OutLabrFrc")
newdata<- df.cmplt.norm[,vars_to_retain]
dim(newdata)
str(newdata)
### Now that we have determined the variables with maximum variance. Let’s divide the data into test and train.
ratio = sample(1:nrow(newdata), size = 0.25*nrow(newdata))
test.data = newdata[ratio,] #Test dataset 25% of total
train.data = newdata[-ratio,] #Train dataset 75% of total
dim(train.data)
dim(test.data)

# Evaluation metric function
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

# Multiple Linear regression
linear.mod<- lm(Employed~., data = train.data)
summary(linear.mod)
plot(linear.mod, pch=16, which = 1)
predict<- predict(linear.mod, test.data)

# RMSE
RMSE0<- RMSE(predict, test.data$Employed)
RMSE0<- round(RMSE0, digits = 3) #0.003
RMSE0

# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test.data$Employed, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy # 99%

min_max_accuracy <- mean (apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy #.99%

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

# Model Diagnostics
# Check the AIC and BIC
AIC(linear.mod)
BIC(linear.mod)

### Model Performance on various supervised algorithms

### Regression Tree method

library(rpart)
model <- rpart(Employed ~., data = train.data, method = "anova")
predict <- predict(model, test.data)
# RMSE
RMSE1 <- RMSE(predict, test.data$Employed)
RMSE1 <- round(RMSE1, digits = 3) #0.037
RMSE1

### R, Random Forests, function randomForest(), method "anova" ####
library(randomForest)
model.forest <- randomForest(Employed ~., data = train.data, method = "anova",
                             ntree = 300,
                             mtry = 2, #mtry is sqrt(6)
                             replace = F,
                             nodesize = 1,
                             importance = T)
varImpPlot(model.forest) # Look at the IncNodePurity plot. From this plot we see that important vars are `State`, `Employed` and `LabourForce`
prediction <- predict(model.forest,test.data)
RMSE3 <- sqrt(mean((log(prediction)-log(test.data$Employed))^2))
round(RMSE3, digits = 3) # 0.009


#### CONCLUSION: TO PREDICT Employment rate in Malaysia
# Multiple Linear Regression RMSE: 0.003
#Random Forest RMSE:  0.009
#Regression Tree RMSE: 0.037






