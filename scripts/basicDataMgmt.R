# Basic Data Management
## 1. Manipulating data and missing values
## 2. Understanding data type conversions
## 3. Creating and recoding variables
## 4. Sorting, merging and subsetting datasets
## 5. Selecting and dropping vars

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

