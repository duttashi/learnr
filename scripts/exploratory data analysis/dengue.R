# clear the workspace
rm(list = ls())
# read the data
dengue.train<- read.csv("data/dengue_train.csv", header = TRUE, sep = ",")
str(dengue.train$week_start_date)
# split date 
library(tidyr) # for separate()
# separate the week_start_date into year, month and date format
dengue.train <- separate(dengue.train, week_start_date, into= c("Year", "Month", "Day"), 
                      sep="-",extra = "drop")
# convert character to numeric format
dengue.train$Year<- as.numeric(dengue.train$Year)
dengue.train$Month<- as.numeric(dengue.train$Month)
dengue.train$Day<- as.numeric(dengue.train$Day)
str(dengue.train)

# drop variable `year`
dengue.train$year<-NULL 

  