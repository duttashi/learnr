# This script is in continuation with L0-descriptive_stats_0.0.R

# Data Formatting
# load the required libraries
library(stringr)
library(chron)
# see the help pages for str_pad, substring, paste, chron, head
help("str_pad")

# prints the first five rows of the dataset.
head(flights[miss_name]) # The date format need to be changed
# create a new object dep_time and assign the values of flights$DepTime . If the value is less than 4 elements, fill make it a 4-element value with zeros.
dep_time<- str_pad(flights$DepTime, 4, pad = "0")
# create a new object hour and assign the first two elements of the dep_time object. 
# Doing this because so as to seperate the hour, minute, seconds from the variable DepTime
hour<-substring(dep_time,1,2)
# Create a new object named minutes and assign the last two elements of the dep_time object.
minute<-substring(dep_time,3,4)
# Assign to the object dep_time the hour in format ‘HH:MM:SS’ , seconds should be ‘00’ , we make this assumption for the sake of formatting.
for(i in 1:length(dep_time)){
  dep_time[i] <- paste(c(hour[i],minute[i],'00'),collapse = ':')
}
# Change the class of dep_time from character to times.
dep_time <- chron(times=dep_time)
# Print the first 10 rows and then the 10 last rows of the dep_time. 
head(dep_time, n=10)
tail(dep_time, n=10)
#  If the formatting of the object is ‘HH:MM:SS’(as it should) then assign the dep_time to flights$DepTime
flights$DepTime<-dep_time
str(flights)
