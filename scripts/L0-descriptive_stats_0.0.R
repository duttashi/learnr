# clear the workspace
rm(list=ls())

# Data: Flights data
# http://cran-logs.rstudio.com/
# Data source: http://stat-computing.org/dataexpo/2009/the-data.html
# Detailed dataset description: http://www.transtats.bts.gov/Fields.asp?Table_ID=236
# http://r-exercises.com/2016/10/19/descriptive-analytics-part-0-data-exploration/
# Objective: Explore the flights dataset of year 2008 

# Load the required libraries
library(dplyr)
# Read in the dataset
flights<-read.csv("data/flights2008.csv", header = TRUE, sep = ",")

## Data Exploration
nrow(flights) # Total row count, 7009728
ncol(flights) # Total number of cols, 29
str(flights)  # Structure of the data
sum(is.na(flights)) # Total missing values count, 28602754
colSums(is.na(flights)) # Columns with missing values
#ArrTime=151649, DayOfWeek=136246, ActualElapsedTime=154699,CRSElapsedTime=844,AirTime=151649, ArrDelay=154699, DepDelay=136246, TaxiIn=151649,TaxiOut=137058,CarrierDelay=5484993,WeatherDelay=5484993, NASDelay=5484993, SecurityDelay=5484993,LateAircraftDelay=5484993
summary(flights)
names(flights) # prints the names of the variables
print(object.size(flights), units="GB") #prints the data size= 0.98GB

help(match)
match(TRUE, is.na(flights[1]), nomatch=FALSE) # Check if first col has missing value. Return a logical value 
# Or it can be written by using the %in%
TRUE %in% is.na(flights[1])

# Print the number of variables that contain missing values using a custom function
count<-0
for (i in 1:ncol(flights[1]))
  {
  if( match(TRUE,is.na(flights[i]),nomatch = FALSE)){
    count <- count+1
  }
}
print (count)

# Or by using the %in% is given as
count <- 0
for (i in 1:ncol(flights)){
    if( TRUE %in% is.na(flights[i])){
      count <- count+1
    }
}
print (count) # 14 

# print the portion of missing values in the dataset. 
count/ncol(flights) # Approx about 48% missing values in the dataset

#print the variable names with missing values by creating a user defined function
miss_name <- c()
for (i in 1:length(flights)){
  if (TRUE %in% is.na(flights[i])){
    miss_name<-append(miss_name,names(flights[i]))
  }
}
print(miss_name)

summary(flights) # quick summary statistics for the dataset