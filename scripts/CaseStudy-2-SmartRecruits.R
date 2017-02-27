# clean the workspace
rm(list=ls())
# Data Source: 
# Data dictionary:
library(tidyr)
library(mice)

set.seed(1234)

train <- read.csv(file = "data/smart_recruits.csv", stringsAsFactors = F)
# check data structure
str(train)

# Note: The target variable is Business_Sourced
## lets remove the target variable from the train dataset for now
train.y<- train$Business_Sourced
train$Business_Sourced<- NULL
# Feature Engineering
## seperate date variables into day, month and year using separate() from tidyr() package
?separate

train<- separate(data = train, col = Application_Receipt_Date, 
                 into = c("Receipt_month", "Receipt_date", "Receipt_year")
                 )
str(train)

## convert new features to respective data types

train$Receipt_date<- as.numeric(train$Receipt_date)
train$Receipt_month<- as.numeric(train$Receipt_month)
train$Receipt_year<- as.numeric(train$Receipt_year)
table(train$Receipt_year)

## Separate applicant birthdate into month, day, year
## --------------------------------------------------
train<- separate(data = train, col = Applicant_BirthDate, 
                 into = c("Applicant_Birth_month", "Applicant_Birth_date", "Applicant_Birth_year")
                 )
# dropping variables
train$Applicant_Birth_date <- NULL
train$Applicant_Birth_month <- NULL
# check for missing data in the new variable Applicant_Birth_year
sum(is.na(train$Applicant_Birth_year)) #73 missing values
# check variable type
str(train$Applicant_Birth_year) # is character data type
# impute missing values 
train$Applicant_Birth_year[is.na(train$Applicant_Birth_year)] <-
  as.numeric(names(which.max((table(train$Applicant_Birth_year)))))

# convert  variable Applicant_Birth_year to numeric
train$Applicant_Birth_year<- as.numeric(train$Applicant_Birth_year)
## create applicant age variable
train$Applicant_Age<- (2008-train$Applicant_Birth_year)

str(train)
## separate Manager's DOJ into year, month and date
## ----------------------------------------------------------
train<- separate(data = train, col = Manager_DOJ, 
                 into = c("Manager_Join_month", "Manager_Join_date", "Manager_Join_year")
)
# convert to numeric
train$Manager_Join_date <- as.numeric(train$Manager_Join_date)
# impute missing values 
train$Manager_Join_date[is.na(train$Manager_Join_date)] <- 
  median(train$Manager_Join_date, na.rm = T)

train$Manager_Join_month <- as.numeric(train$Manager_Join_month)
train$Manager_Join_month[is.na(train$Manager_Join_month)] <- 
  median(train$Manager_Join_month, na.rm = T)

train$Manager_Join_year <- as.numeric(train$Manager_Join_year)
train$Manager_Join_year[is.na(train$Manager_Join_year)] <- 
  median(train$Manager_Join_year, na.rm = T)

# Separating Manager_DoB into date, month, year
# ----------------------------------------------------------
train <- separate(data = train, col = Manager_DoB, 
                  into = c("Manager_Birth_month", "Manager_Birth_date", "Manager_Birth_year"))
train$Manager_Birth_date <- NULL
train$Manager_Birth_month <- NULL
train$Manager_Birth_year <- as.numeric(train$Manager_Birth_year)
train$Manager_Birth_year[is.na(train$Manager_Birth_year)] <- 
  median(train$Manager_Birth_year, na.rm = T)

# Create Manager_Age variable
# ---------------------
train$Manager_Age <- (2008 - train$Manager_Birth_year)
train$Manager_Birth_year <- NULL

# Encoding Applicant_Gender
# ----------------------------------------------------------
train$Applicant_Gender[train$Applicant_Gender == "F"] <- 1
train$Applicant_Gender[train$Applicant_Gender == "M"] <- 2
train$Applicant_Gender[train$Applicant_Gender == ""] <- 3
train$Applicant_Gender <- as.numeric(train$Applicant_Gender)

# Encoding Applicant_Occupation
# ----------------------------------------------------------
train$Applicant_Occupation[train$Applicant_Occupation == "Salaried"] <- 1
train$Applicant_Occupation[train$Applicant_Occupation == "Business"] <- 2
train$Applicant_Occupation[train$Applicant_Occupation == "Others" |
                             train$Applicant_Occupation == ""] <- 3
train$Applicant_Occupation[train$Applicant_Occupation == "Self Employed" |
                             train$Applicant_Occupation == "Student"] <- 4
train$Applicant_Occupation <- as.numeric(train$Applicant_Occupation)

# Encoding Applicant_Qualification
# ----------------------------------------------------------
train$Applicant_Qualification[train$Applicant_Qualification == "Class XII"] <- 1
train$Applicant_Qualification[train$Applicant_Qualification == "Graduate"] <- 2
train$Applicant_Qualification[train$Applicant_Qualification == "Class X"] <- 3
train$Applicant_Qualification[train$Applicant_Qualification != 1 &
                                train$Applicant_Qualification != 2 &
                                train$Applicant_Qualification != 3] <- 4
train$Applicant_Qualification <- as.numeric(train$Applicant_Qualification)

# Encoding Manager_Joining_Designation
# ----------------------------------------------------------
# check levels
table( unique(train$Manager_Joining_Designation))

train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 1"]<-1
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 2"]<-2
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 3"]<-3
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 4"]<-4
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 5"]<-5
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 6"]<-6
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Level 7"]<-7
train$Manager_Join_Desg[train$Manager_Joining_Designation=="Other"]<-8

table(train$Manager_Join_Desg)
# drop the original variable
train$Manager_Joining_Designation<-0

# Encoding Manager_Current_Designation
# ----------------------------------------------------------
# check levels
table( unique(train$Manager_Current_Designation))

temp_current_des <- train$Manager_Current_Designation # create a copy of the variable
train$Manager_Current_Designation <- 0 #drop the original variable
train$Manager_Current_Designation[temp_current_des == "Level 1"|
                                    temp_current_des == "Other"] <- 1
train$Manager_Current_Designation[temp_current_des == "Level 2"] <- 2
train$Manager_Current_Designation[temp_current_des == "Level 3"] <- 3
train$Manager_Current_Designation[temp_current_des == "Level 4"] <- 4
train$Manager_Current_Designation[temp_current_des == "Level 5" |
                                    temp_current_des == "Level 6" |
                                    temp_current_des == "Level 7"] <- 5
rm(temp_current_des) # drop the temp variable

# Creating Manager_Progress variable
# ----------------------------------
train$Manager_Progress <- (train$Manager_Current_Designation - 
                             train$Manager_Joining_Designation)

train$Manager_Progress[train$Manager_Joining_Designation == 0] <- -2


# Encoding Manager_Status
# ----------------------------------------------------------
train$Manager_Status[train$Manager_Status == "Confirmation"] <- 1
train$Manager_Status[train$Manager_Status == "Probation"] <- 2
train$Manager_Status[train$Manager_Status == ""] <- 3
train$Manager_Status <- as.numeric(train$Manager_Status)

# Missing data treatment for the numeric variables
train$Manager_Grade[!complete.cases(train)] <- median(train$Manager_Grade, na.rm = T)
train$Manager_Num_Application[!complete.cases(train)] <- 2.00
train$Manager_Num_Coded[!complete.cases(train)] <- mean(train$Manager_Num_Coded, na.rm = T)
train$Manager_Business[!complete.cases(train)] <- mean(train$Manager_Business, na.rm = T)
train$Manager_Num_Products[!complete.cases(train)] <- mean(train$Manager_Num_Products, 
                                                           na.rm = T)
train$Manager_Business2[!complete.cases(train)] <- median(train$Manager_Business2, 
                                                          na.rm = T)
train$Manager_Num_Products2[!complete.cases(train)] <- median(train$Manager_Num_Products2, 
                                                              na.rm = T)

# Add back the Target variable, Business_Sourced
# ----------------------------------------------
train$Business_Sourced <- train.y


## BUILD MODEL WITH STRATIFIED K-FOLD CV
library(caret) # for the createFolds() function
folds <- createFolds(as.factor(train$Business_Sourced), k = 5)
fold_auc <- c()

