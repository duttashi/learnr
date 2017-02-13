# clear the workspace
rm(list=ls())
# Data Source Info
# Labour Force and Social Statistic http://www.dosm.gov.my/v1/index.php?r=column3/accordion&menu_id=aHhRYUpWS3B4VXlYaVBOeUF0WFpWUT09
# Load the required libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr) # for the gather()
library(missForest)

# Load the data
df1<- read.csv("data/bptms-Employed_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2<- read.csv("data/bptms-Labour_force_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df3<- read.csv("data/bptms-Labour_Force_Participation_rate_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df4<- read.csv("data/bptms-Outside_labour_force_by_state.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5<- read.csv("data/bptms-Unemployment_Rate.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#df6<- read.csv("data/bptms-Employed_less_than_30_hours.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

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
table(df6$state) # state with 2 levels, one is blank and other is Malaysia. Drop df6. 
rm(df6) # dropped df6

sum(is.na(df1))
sum(is.na(df2))
sum(is.na(df3)) # 29 missing values
colSums(is.na(df3)) # check which col has missing values
sum(is.na(df4))
sum(is.na(df5)) # 29 missing values
colSums(is.na(df5))

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

## Change data type
df1$state<- as.factor(df1$state)
df1$employed<- as.numeric(gsub(",","", df1$employed))
df2$state<- as.factor(df2$state)
df2$labourforce<- as.numeric(gsub(",","", df2$labourforce))
df3$state<- as.factor(df3$state)
df4$state<- as.factor(df4$state)
df4$outlbrfrc<- as.numeric(gsub(",","", df4$outlbrfrc))
df5$state<- as.factor(df5$state)

## Joining the data frames
## using the dplyr library

system.time(join1<- inner_join(df1,df2))
system.time(join2<- inner_join(df3,df4))
system.time(join3<- inner_join(join1,join2))
system.time(df.master<- inner_join(join3,df5))


# Missing value treatment
## visualization
library(VIM)
aggr_plot <- aggr(df.master, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df.master), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))
colSums(is.na(df.master))


imputdata<- missForest(df.master)
# check imputed values
imputdata$ximp
df.cmplt<- imputdata$ximp
colSums(is.na(df.cmplt))

# Describe the data
library(Hmisc)
describe(df.cmplt)

# Univariate Data Visualizations

# Histogram's: NO GOOD IN SHOWING CONTINUOUS DATA
ggplot(df.cmplt) +
  geom_histogram(aes(x=Year), fill="gray", binwidth = 20, stat = "bin", position = "stack")

ggplot(df.cmplt) +
  geom_histogram(aes(x=employed), fill="gray", binwidth = 5000, stat = "bin", position = "stack")

ggplot(df.cmplt) +
  geom_histogram(aes(x=labourforce), fill="gray", binwidth = 5000, stat = "bin", position = "stack")
ggplot(df.cmplt) +
  geom_histogram(aes(x=lbrfrcperct), fill="gray", binwidth = 100, stat = "bin", position = "stack")

# Density plots # FAR BETTER THAN HISTOGRAMS
ggplot(df.cmplt)+
  geom_density(aes(x=employed, fill="red"))
ggplot(df.cmplt)+
  geom_density(aes(x=labourforce, fill="red")) # majority of the labour force is >5000
ggplot(df.cmplt)+
  geom_density(aes(x=lbrfrcperct, fill="red")) # majority lies between 60-70%
ggplot(df.cmplt)+
  geom_density(aes(x=outlbrfrc, fill="red")) # majority of outside labour force is >2000
ggplot(df.cmplt)+
  geom_density(aes(x=unemprateperct, fill="red")) # majority lies between 2.5 and 5.0
ggplot(df.cmplt)+
  geom_density(aes(x=Year, fill="red")) # twin peaked, first peak at about 1988 and second peak at 2000-2005

# subset the data based on observations from denisty plot
subst.data.1<- subset(df.cmplt, labourforce<=1600 & lbrfrcperct <=70 & unemprateperct>=2.5
                   & unemprateperct<=5.0) 
# OBSERVATION: when labourforce <= 2000, there were outliers in the data. when labourforce<=1600 the outliers were removed


# Boxplot

library(magrittr) # for the pipe operator
library(dplyr) # for select() 
str(subst.data.1)

boxplot(subst.data.1 %>% 
          # Note that the ‘%>%’ (pipe) passes data from the command before to the one after.
          select(employed,labourforce,outlbrfrc))
boxplot(subst.data.1 %>%
          select(unemprateperct))
boxplot(subst.data.1 %>%
          select(lbrfrcperct))

