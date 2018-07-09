
# Data downloaded from: https://archive.ics.uci.edu/ml/datasets/Absenteeism+at+work
# Data description: The database was created with records of absenteeism at work from July 2007 to July 2010 at a courier company in Brazil. 

# clean the workspace
rm(list = ls())
# load the data in the environment
library(readxl)
work_absence_data <- read_excel("C:/Users/Ashoo/Downloads/Web Download/Absenteeism_at_work_AAA/Absenteeism_at_work.xls", 
                                 col_types = c("numeric", "text", "text", "text", 
                                               "text", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "text", 
                                               "numeric", "text", "text",
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric")
                                 )
# Data Management Decisions
# 1. RECODE VARIABLES: Shorten the column names
library(dplyr) # for rename()
library(magrittr)# for the pipe operator

badvarnames<- colnames(work_absence_data)
goodvarnames<- c("AbsentReason","AbsentMonth","AbsentDay",
                 "Season","TransportExpense","DistanceToWork","ServiceTime",
                 "Age","DailyAvgWorkLoad","Education","Son","HitTarget",
                 "DiscplnFailure","SocialDrinker","SocialSmoker","Pet",
                 "Weight","Height","BMI","HourlyAbsentTime")

work_absence_data<- work_absence_data %>%
  select(badvarnames) %>%
  setNames(goodvarnames)

# change values for factor variables
table(work_absence_data$AbsentDay)
