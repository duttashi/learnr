# clean the workspace
rm(list = ls())
# Data Source: manually created artificial dataset.
# Objective: To build a model to predict which customers do not have health insurance cover
# Dataset name: customer.tsv
# Data dictionary: 
# marital.stat= marital status; state.of.res= state of residence; num.vehicles= number of vehicles owned; recent.move= recently moved house;

# Load the required libraries
library(ggplot2)
library(hexbin)

# Load the data in R environment
custdata<- read.table('data/customer.tsv',header=TRUE,sep='\t', stringsAsFactors = TRUE)
summary(custdata)
#levels(custdata$state.of.res) 

# Invalid values and outliers
summary(custdata$income)
summary(custdata$age)
# Exploring data range
summary(custdata$income)

# Exploring data : Spotting problems using visualization
library(ggplot2)
# Univariate visualization- Histogram (Continuous data- use density plot, histogram)
ggplot(custdata) +
  geom_histogram(aes(x=age), binwidth=5, fill="gray")
# density plot
ggplot(custdata) + geom_density(aes(x=age))

ggplot(custdata) + geom_density(aes(x=income))
ggplot(custdata) + geom_density(aes(x=income))+
  scale_x_log10(breaks=c(100,1000,10000,100000))

# univariate categorical data inspection- Bar charts
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +  
  coord_flip() + 	
  theme(axis.text.y=element_text(size=rel(0.8)))  

#Producing a bar chart with sorted categories 
statesums <- table(custdata$state.of.res) 	
statef <- as.data.frame(statesums) 	 
colnames(statef)<-c("state.of.res", "count") 	
summary(statef)
statef <- transform(statef,
                    state.of.res=reorder(state.of.res, count))
summary(statef)
ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",       
                         fill="gray") +
  coord_flip() +                                
  theme(axis.text.y=element_text(size=rel(0.8)))

#	Line plots- visualizing relationship between two variables
ggplot(custdata)+ geom_line(aes(x=age, y=income))

# lets subset the data on age and income
custdata1<- subset(custdata,(custdata$age>0 & custdata$age<100) & custdata$income>0)
ggplot(custdata1)+ geom_line(aes(x=age, y=income))

# scatter plot
ggplot(custdata1)+ geom_point(aes(x=age, y=income))
ggplot(custdata1)+ geom_point(aes(x=age, y=income))+
  geom_smooth(data = custdata1, aes(x=age, y=income))

# Hexbin plot
ggplot(data = custdata1, aes(x=age, y=income))+
  geom_hex(binwidth=c(5,10000))+
  geom_smooth(color="white")

# Stacked bar chart to determine relationship between two categorical variables
ggplot(data = custdata)+
  geom_bar(aes(x= marital.stat, fill=health.ins))+
  ggtitle("Stacked Bar Chart")

# side-by-side bar chart
ggplot(data = custdata)+
  geom_bar(aes(x= marital.stat, fill=health.ins),
           position = "dodge")+
  ggtitle("Side-by-Side Bar Chart")

# filled bar chart
ggplot(data = custdata)+
  geom_bar(aes(x= marital.stat, fill=health.ins),
           position = "fill")+
  ggtitle("Filled Bar Chart")

####### MISSING DATA TREATMENT ######
str(custdata)
colSums(is.na(custdata))
table(is.na(custdata$is.employed))

# Visualizing Missing data
library(Amelia)
missmap(custdata[,1:11],
        main = "Missing values in Health Insurance Dataset",
        y.labels = NULL,
        y.at = NULL)

# Remapping NA to a level and creating a new variable
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),"missing",
                                   ifelse(custdata$is.employed==T, "employed","not employed")
                                   )
custdata$is.employed.fix<- as.factor(custdata$is.employed.fix)
# using the mice package
library(mice)
custdata$is.employed.fix<-NULL # drop the variable
custdata$housing.type.fix<-NULL# drop the variable
tempData <- mice(custdata,m=5,maxit=50,meth='pmm',seed=1234)

custdata.complete<- complete(tempData,1)

missmap(custdata.complete[,1:11],
        main = "Missing values in Health Insurance Dataset",
        y.labels = NULL,
        y.at = NULL)

summary(custdata.complete$age)
meanage<-mean(custdata.complete$age)
meanage
stdage<-sd(custdata.complete$age)
stdage
custdata.complete$age.normalized<- (custdata.complete$age-meanage)/stdage
summary(custdata.complete$age.normalized)
custdata.complete$age<-NULL # drop the age variable
str(custdata.complete)

### Log transformation for skewed distributions
summary(custdata.complete$income)
hist(custdata.complete$income)
# replacing negative income value to zero because log does not accept negative values
custdata.complete$income<- ifelse(custdata.complete$income<0, 0, custdata.complete$income)
# Log transformation for skewed distributions
custdata.complete$income.log<-log10(custdata.complete$income)
# visualizing the log transformed distribution
hist(custdata.complete$income.log)

custdata.complete$income<- NULL # dropping the skewed variable
str(custdata.complete)
