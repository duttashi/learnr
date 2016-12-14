# Introducing the data.table()
# For more info see this https://www.r-bloggers.com/intro-to-the-data-table-package/
# data.table() works well with large data files of size in GB

# Subsetting Data The Old School Way

# Load a built in dataset for example purpose

attach(mtcars) # loading the motor trend car road tests dataset
help(mtcars)
str(mtcars)

class(mtcars) # check the data type.  

#select rows 1 & 2

mtcars[1:2,]

# select rows 3&2 and columns 4,5,6

mtcars[3:4,4:6]

# select rows 3&2 and columns 4,6,9

mtcars[3:4,c(4,6,9)]

# Find the rows where the MPG column is greater than 30

mtcars[mtcars$mpg>30,]
# select all rows where MPG > 30 and cylinder is equal to 4 and then 
# extract columns 10 and 11
mtcars[mtcars$mpg>30 & mtcars$cyl==4, 10:11]

# Get the mean MPG by Transmission

tapply(mtcars$mpg, mtcars$am, mean)
# Get the mean MPG for Transmission grouped by Cylinder

aggregate(mpg~am+cyl,data=mtcars,mean)
# Cross tabulation based on Transmission and Cylinder

table(transmission=mtcars$am, cylinder=mtcars$cyl)

# Using the data.table package

library(data.table)

dt<- data.table(mtcars) # load the mtcars data as a data.table

class(dt)
#mtcars[,mean(mpg)]

dt[,mean(mpg)]   # You can't do this with a normal data frame
# Reproducing the previous tapply example

tapply(mtcars$mpg,mtcars$am,mean)

# Here is how we would do this with the data table "dt"

dt[,mean(mpg),by=am]
# We could even extend this to group by am and cyl

dt[,mean(mpg),by=.(am,cyl)]
# If we want to more clearly label the computed average

dt[,.(avg=mean(mpg)),by=.(am,cyl)]

# Tabulation

# Within a data table the special variable .N represents the count of rows.

dt[, .N] # How many rows

dt[, .N, by=cyl]  # How many cars in each cylinder group
# For rows where the wt is > 1.5 tons count the number of cars by
# transmission type.

dt[wt > 1.5, .(count=.N), by=am] 

# Sortng in data.table
# Present the 5 cars with the best MPG

head(dt[order(-mpg)],5) 
# Since data table inherits from a data frame we could have also done

dt[order(-mpg)][1:5]

# We could sort on multiple keys. Here we find the cars with the best
# gas mileage and then sort those on increasing weight

dt[order(-mpg,wt)][1:5]



