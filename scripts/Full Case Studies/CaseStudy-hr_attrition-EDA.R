# Objective: Exploratory Data Analysis for Human Resource analytics
# Script author: Ashish Dutt
# Script create date: 13/3/2019
# Script last modified date: 17/3/2019
# Email: ashishdutt@yahoo.com.my

# clean the workspace
rm(list = ls())

# load the required libraries
library(data.table) # for fread(), data.table()
library(caret) # for nearZeroVar(), findCorrelation()
library(corrplot) # for correlation matrix
library(ggplot2)
library(grid) # for grid.rect()
library(plyr) # for mapvalues(), rename()
library(Boruta) # for feature importance

# read the data
df.train<- fread("data/hr_attrition_train.csv", header = TRUE, stringsAsFactors = TRUE)

# summary statistics
summary(df.train)
# renaming the column names
df.train<- rename(df.train, c("sales"="role"))
df.train<- rename(df.train, c("time_spend_company"="exp_in_company"))
str(df.train)

# rearrange the columns such that dependent var is positioned last
df.train<- df.train[,c(1:7,9:11,8)]
# check for missing values
sum(is.na(df.train)) #0 missing values

## Exploratory Data Analysis
# see the column names
colnames(df.train)
dim(df.train)

attrition<-as.factor(df.train$left)
summary(attrition)
perc_attrition_rate<-sum(df.train$left/length(df.train$left))*100
#percentage of attrition
print(perc_attrition_rate) # 21.41% is the attrition rate

# check for correlation among continuous variables
str(df.train)
cor_vars<- df.train[,c(2:8,11)] # the index position of continuous variables
cor.vals<- cor(cor_vars, method = "pearson")
# Visualize high correlations
corrplot(cor.vals, method = "number") # none of the continuous vars have high correlation
## positive(+) correlation between projectCount, averageMonthlyHours, and evaluation. Which could mean that the employees who spent more hours and did more projects were evaluated highly.
## negative(-) relationships, attrition and satisfaction are highly correlated. I'm assuming that people tend to leave a company more when they are less satisfied.

###  Statistical Test for Correlation
# One-Sample T-Test (Measuring Satisfaction Level)
# A one-sample t-test checks whether a sample mean differs from the population mean. Let's test to see whether the average satisfaction level of employees that had a attrition differs from the entire employee population.
# 
# Hypothesis Testing: Is there significant difference in the means of satisfaction level between employees who had a attrition and the entire employee population?
# 
# Null Hypothesis: (H0: pTS = pES) The null hypothesis would be that there is no difference in satisfaction level between employees who did attrition and the entire employee population.
# 
# Alternate Hypothesis: (HA: pTS != pES) The alternative hypothesis would be that there is a difference in satisfaction level between employees who did attrition and the entire employee population.
# Let's compare the means of our employee attrition satisfaction against the employee population satisfaction
emp_population_satisfaction <-mean(df.train$satisfaction_level)
left_pop<-subset(df.train,left==1)
emp_attrition_satisfaction <-mean(left_pop$satisfaction_level)

print( c('The mean for the employee population is: ', emp_population_satisfaction) )
print( c('The mean for the employees that had a attrition is: ' ,emp_attrition_satisfaction) )

## Let's conduct a t-test at 95% confidence level and see if it correctly rejects the null hypothesis that the sample comes from the same distribution as the employee population. To conduct a one sample t-test, we can use the stats.ttest_1samp() function:
t.test(left_pop$satisfaction_level,mu=emp_population_satisfaction) # Employee Population satisfaction mean
# The test result shows the test statistic "t" is equal to 0.4398. This test statistic tells us how much the sample mean deviates from the null hypothesis. If the t-statistic lies outside the quantiles of the t-distribution corresponding to our confidence level and degrees of freedom, we reject the null hypothesis. We can check the quantiles with stats.t.ppf():

# Distribution Plots

## Salary V.S. attrition
plt.1<- table(df.train$salary, df.train$left)
plt.1.df<- as.data.frame(plt.1)
print(plt.1.df)
ggplot(plt.1.df, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()+
  ggtitle("(A) Salary vs attrition")+
  labs(x="Salary", 
       y="count", fill="Attrition")+
  theme_minimal()
# add a border
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()

## Finding
## Majority of employees who left either had low or medium salary.
## Barely any employees left with high salary
## Employees with low to average salaries tend to leave the company.

## Department V.S. attrition
plt.2<- table(df.train$role, df.train$left)
plt.2.df<- as.data.frame(plt.2)
print(plt.2.df)
ggplot(plt.2.df, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()+
  ggtitle("(B) Department vs attrition")+
  labs(x="Department", 
       y="Frequency", fill="Attrition")+
  theme_minimal()
# add a border
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()
## Findings: The sales, technical, and support department were the top 3 departments to have employee attrition
# The management department had the smallest amount of attrition

## attrition V.S. ProjectCount
plt.3<- table(df.train$number_project, df.train$left)
plt.3.df<- as.data.frame(plt.3)
print(plt.3.df)
ggplot(plt.3.df, aes(x=Var1,y=Freq,fill=Var2)) +
  geom_bar(position="dodge",stat='identity') + coord_flip()+
  ggtitle("(C) ProjectCount vs attrition")+
  labs(x="ProjectCount", 
       y="Frequency", fill="Attrition")+
  theme_minimal()

# add a border
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()

## Findings
# More than half of the employees with 2,6, and 7 projects left the company
# Majority of the employees who did not leave the company had 3,4, and 5 projects
# All of the employees with 7 projects left the company
# There is an increase in employee attrition rate as project count increases
## Points to ponder?
# Why are employees leaving at the lower/higher spectrum of project counts?
#   Does this means that employees with project counts 2 or less are not worked hard enough or are not highly valued, thus leaving the company?
#   Do employees with 6+ projects are getting overworked, thus leaving the company?

## attrition V.S. Evaluation
# Kernel Density Plot
emp_left_data<-subset(df.train,left==1)
emp_stay_data<-subset(df.train,left==0)

ggplot() + 
  geom_density(aes(x=last_evaluation), colour="red", data=emp_left_data) + 
  geom_density(aes(x=last_evaluation), colour="blue", data=emp_stay_data)+
  ggtitle("(D) Evaluation vs attrition")+
  labs(x="Evaluation", y="Frequency", fill="Attrition")+
  theme_minimal()
# add a border
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()

## attrition vs Average Monthly Hours
names(df.train)
ggplot() + geom_density(aes(x=average_montly_hours), colour="red", data=emp_left_data) + 
  geom_density(aes(x=average_montly_hours), colour="blue", data=emp_stay_data)+
  ggtitle("(D) Average monthly hours vs attrition")+
  labs(x="average monthly hours", y="Frequency", fill="Attrition")+
  theme_minimal()
# add a border
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()

## Findings
# There is a biomodal distribution for those that had a attrition.
# Employees with low performance tend to leave the company more
# Employees with high performance tend to leave the company more
# The sweet spot for employees that stayed is within 0.6-0.8 evaluation

# clear the graphic device
grid.newpage()

## Feature Importance

df.train$left<-as.factor(df.train$left)
boruta.df.train <- Boruta(left~., data = df.train, doTrace = 2)

print(boruta.df.train) # all attributes are important
plot(boruta.df.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.df.train$ImpHistory),function(i)
  boruta.df.train$ImpHistory[is.finite(boruta.df.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.df.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.df.train$ImpHistory), cex.axis = 0.7)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphic device
grid.newpage()

# Key Observations: The above graph clearly represents the factors which serve as the top reasons for attrition in a company:
#   
#   Satisfaction level: it already had a negative corellation with the outcome. People with low satisfaction were most likely to leave even when compared with evaluations(Evident cluster was formed with respect to low satisfaction)
# 
# Salary and the role they played has one of the least impact on attrition
# 
# Pressure due to the number of projects and how they were evaluated also holds key significance in determining attrition
