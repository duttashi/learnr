# clear the workspace
rm(list=ls())
# Data Source Info
# Labour Force and Social Statistic http://www.dosm.gov.my/v1/index.php?r=column3/accordion&menu_id=aHhRYUpWS3B4VXlYaVBOeUF0WFpWUT09
# Load the required libraries
library(ggplot2)
library(plyr)
library(tidyr) # for the gather()
library(mice)

# Load the data
df1<- read.csv("data/bptms-Employed_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2<- read.csv("data/bptms-Labour_force_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df3<- read.csv("data/bptms-Labour_Force_Participation_rate_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df4<- read.csv("data/bptms-Outside_labour_force_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5<- read.csv("data/bptms-Unemployment_Rate.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df6<- read.csv("data/bptms-Employed_less_than_30_hours.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Exploratory Data Analysis
dim(df1)
dim(df2)
dim(df3)
dim(df4)
dim(df5)
dim(df6)

str(df1) # OBSERVATION: remove the comma between the numbers in employed attribute and change data type to numeric, rename the col header to short names
str(df2) # OBSERVATION: remove the comma between the numbers in employed attribute and change data type to numeric, rename the col header to short names
str(df3) # OBSERVATION: rename the col header to short names
str(df4) # OBSERVATION: remove the comma between the numbers in employed attribute and change data type to numeric, rename the col header to short names
str(df5) # OBSERVATION: rename the col header to short names
str(df6) # OBSERVATION: rename the col header to short names

sum(is.na(df1))
sum(is.na(df2))
sum(is.na(df3)) # 29 missing values
colSums(is.na(df3)) # check which col has missing values
sum(is.na(df4))
sum(is.na(df5)) # 29 missing values
colSums(is.na(df5))
sum(is.na(df6)) #2 missing values
colSums(is.na(df6))

# Basic Data Management 

## Renaming the column name
names(df1)
df1<- rename(df1, c("State.Country" = "state"))
df1<- rename(df1, c("Employed...000." = "employed"))
names(df2)
df2<- rename(df2, c("State.Country" = "state"))
df2<- rename(df2, c("Labour.Force...000." = "labourforce"))
names(df3)
df3<- rename(df3, c("State.Country" = "state"))
df3<- rename(df3, c("Labour.Force.Participation.Rate..Percentage." = "lbrfrcperct"))
names(df4)
df4<- rename(df4, c("State.Country" = "state"))
df4<- rename(df4, c("Outside.Labour.Force...000." = "outlbrfrc"))
names(df5)
df5<- rename(df5, c("State.Country" = "state"))
df5<- rename(df5, c("Unemployment.Rate..Percentage." = "unemprateperct"))
names(df6)
df6<- rename(df6, c("State.Country" = "state"))
df6<- rename(df6, c("Working.less.than.30.hours...000." = "workless30"))

## Change data type
df1$state<- as.factor(df1$state)
df1$employed<- as.numeric(gsub(",","", df1$employed))

str(df2)
df2$state<- as.factor(df2$state)
df2$labourforce<- as.numeric(gsub(",","", df2$labourforce))

str(df3)
df3$state<- as.factor(df3$state)

str(df4)
df4$state<- as.factor(df4$state)
df4$outlbrfrc<- as.numeric(gsub(",","", df4$outlbrfrc))

str(df5)
df5$state<- as.factor(df5$state)

str(df6)
df6$state<- as.factor(df6$state)

# Missing value treatment
colSums(is.na(df3))
tempData <- mice(df3,m=5,maxit=50,meth='pmm',seed=1234)
df3<-mice::complete(tempData,1)
colSums(is.na(df3))

colSums(is.na(df5))
tempData <- mice(df5,m=5,maxit=50,meth='pmm',seed=1234)
df5<-mice::complete(tempData,1)
colSums(is.na(df5))

colSums(is.na(df6))
tempData <- mice(df6,m=5,maxit=50,meth='pmm',seed=1234)
df6<-mice::complete(tempData,1)
colSums(is.na(df6))

# Inner Join the data frames on common column
df.m1<- merge(df1,df2, by="Year")
df.m2<- merge(df3,df4, by="Year")
df.m3<- merge(df.m2, df5, by="Year")
df.master<- merge(df.m1, df.m3, by="Year")


