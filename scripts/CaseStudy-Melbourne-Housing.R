# Case Study- Melbourne Housing Data
# URL: https://www.kaggle.com/anthonypino/melbourne-housing-market


# Load the data
df.melb<- read.csv("data/Melbourne_housing.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

# Exploratory data analysis
dim(df.melb) #[1] 11511     9
str(df.melb)
sum(is.na(df.melb)) #2515 missing 
colSums(is.na(df.melb)) # all missing values in Price
str(df.melb$Date)

# Split date into year, month and date format
library(tidyr)
df.melb<- separate(df.melb, Date, into = c("day", "month", "year"), sep="-")
str(df.melb)
df.melb$day<- as.numeric(df.melb$day)
df.melb$month<- as.numeric(df.melb$month)
df.melb$year<- as.numeric(df.melb$year)
str(df.melb)

# Split Address into House number and Street Name
head(df.melb$Address,10)
df.melb<-extract(df.melb, "Address", c("HouseNumber","space","Streetname"), "(\\d*)(\\s)(\\w*)")
str(df.melb)
unique(df.melb$Streetname)
df.melb$space<-NULL
df.melb$Streetname<- as.factor(df.melb$Streetname)
df.melb$HouseNumber<- as.numeric(df.melb$HouseNumber)

# Missing value visualization
library(VIM)
aggr_plot <- aggr(df.melb, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df.melb), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# MISSING DATA IMPUTATION 
# PMM (Predictive Mean Matching)  – For numeric variables
library(mice)
tempData <- mice(df.melb,m=5,maxit=50,meth='pmm',seed=1234) 
df.master<- mice::complete(tempData,1) 
colSums(is.na(df.master))
