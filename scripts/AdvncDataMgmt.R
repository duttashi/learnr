### Advanced Data Management

# Creating a Class Roster Dataset
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)
z <- scale(roster[,2:4]) 
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

y <- quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"

name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,Lastname, roster[,-1])
roster <- roster[order(Lastname,Firstname),]

roster

# Using the reshape2 package
library(reshape2)
# See this page for details: http://seananderson.ca/2013/10/19/reshape.html
names(airquality) <- tolower(names(airquality))
head(airquality)

# Wide- to long-format data: the melt function
# What happens if we run the function melt with all the default argument values?
aql<-melt(airquality)
# By default, melt has assumed that all columns with numeric values are variables with values.
# we want to know the values of ozone, solar.r, wind, and temp for each month and day. 
# We can do that with melt by telling it that we want month and day to be “ID variables”. 
# ID variables are the variables that identify individual rows of data.

aql <- melt(airquality, id.vars = c("month", "day"))
head(aql)
# What if we wanted to control the column names in our long-format data? 
# melt lets us set those too all in one step
aql <- melt(airquality, id.vars = c("month", "day"),
            variable.name = "climate_variable", 
            value.name = "climate_value")
head(aql)

# Long- to wide-format data: the cast functions
# In reshape2 there are multiple cast functions. Since you will most commonly work with 
# data.frame objects, we’ll explore the dcast function. 
# (There is also acast to return a vector, matrix, or array.)
aql <- melt(airquality, id.vars = c("month", "day"))
aqw <- dcast(aql, month + day ~ variable)
head(aqw)
