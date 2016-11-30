# Basic Data Management
## 1. Manipulating data and missing values
## 2. Understanding data type conversions
## 3. Creating and recoding variables
## 4. Sorting, merging and subsetting datasets
## 5. Selecting and dropping vars

sessionInfo()
## Creating tha dataset
# leadership dataset
manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2)
q5 <- c(5,5,2,NA,1)
leadership <- data.frame(manager,date,gender,age,q1,q2,q3,q4,q5, 
                         stringsAsFactors=FALSE)
summary(leadership)

## a. Recoding variables.
## Lets say you want to change the age of managers in leadership dataset from continuous var age
## to categorical variable agecat(Young, Middle, Aged, Elder)
leadership$age[leadership$age == 99]<- NA # recode missing value to NA
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"
## This code could have been written more compactly using the within() 
leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })
## Renaming variables
fix(leadership) # invokes interactive editor 
str(leadership)

library(reshape) # use the raname() in reshape library 
leadership<-rename(leadership, c(manager="managerID", date="testDate"))
names(leadership[2])<-"testDate" # finally, you can use the names() to rename a var. Note, you will have to provide the index. In R the index numbers begin from 1

# Sorting a dataset
newdata <- leadership[order(leadership$age),]
View(newdata)

# Subsetting Datasets

## 1. Selecting (keeping) variables
newdata <- leadership[, c(6:10)] # method #1
View(newdata)

myvars <- c("q1", "q2", "q3", "q4", "q5") # method #2
newdata <-leadership[myvars]
View(newdata)

myvars <- paste("q", 1:5, sep="") # method #3
newdata <- leadership[myvars]
View(newdata)

## 2. Dropping variables
myvars <- names(leadership) %in% c("q3", "q4") 
leadership[!myvars]

## 3. Selecting observations
newdata <- leadership[1:3,]
newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]

## 4. Using the subset() function
newdata <- subset(leadership, age >= 35 | age < 24,
                  select=c(q1, q2, q3, q4))
newdata <- subset(leadership, gender=="M" & age > 25,select = gender:q4)

## 5. Using SQL statements to manipulate data frames
library(sqldf)
newdf <- sqldf("select * from mtcars where carb=1 order by mpg",
               row.names=TRUE)
newdf
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear
      from mtcars where cyl in (4, 6) group by gear")
