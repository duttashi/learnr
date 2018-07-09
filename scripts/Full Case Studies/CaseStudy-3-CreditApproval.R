# Reference: http://rstudio-pubs-static.s3.amazonaws.com/73039_9946de135c0a49daa7a0a9eda4a67a72.html

# clean the workspace
rm(list=ls())
# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data
# Load the dataset
df.data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                  sep = "," , header = FALSE)

str(df.data) # Look at the data and think of meaningful names to assign as column header 
# Add meaningful names to the different columns in the data.frame
names(df.data)<-c("Male","Age","Debt","Married","BankCustomer","EducationLevel","Ethnicity","YearsEmployed",
                  "PriorLoanDefault","EmploymentStatus","CreditScore","DriversLicense","Citizen","ZipCode","Income","CreditApproveStatus")
str(df.data)
# Exploratory Data Analysis (EDA)

# 1. Data Transformation
## Turning the ‘+’ to a ‘1’ and the ‘-’ to a ‘0’ for CreditApproveStatus

