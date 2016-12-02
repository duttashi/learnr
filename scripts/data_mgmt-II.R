# Advanced data management
# Examples in this file are from the book "R in Action",Chapter 5

#clear the workspace
rm(list=ls())
# Class Roster Dataset
Student <- c("John Davis","Angela Williams","Bullwinkle Moose",
             "David Jones","Janice Markhammer",
             "Cheryl Cushing","Reuven Ytzrhak",
             "Greg Knox","Joel England","Mary Rayburn")
math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
english <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, math, science, english, 
                     stringsAsFactors=FALSE)
# Problems:
# 1. Combine the scores to determine a single performance indicator of each student
# 2. Assign a 'A' to the top 20% of students, a 'B' to the next 20%, and so on.
# 3. Sort the students alphabetically

# Solutions:
# 1. Combine the scores to determine a single performance indicator of each student
roster
# Issue A: the scores in the three exams are not comparable. They have different means and standard deviations
# solution: transform the scores into comparable units
# Issue B: We need a method to determine the student's percentile rank based on the score to assign a grade.
# Issue C: Break the names into first and last names to sort them

# Problem 1: Issue A- Solution: The scale() function standardizes the specified columns of a matrix or a data frame to a mean of 0 and a standard deviation of 1
z<- scale(roster[,2:4]) # Note: , means use all rows and 2:4 means select columns 2 to 4.
                        # In R the column numbering begins from 1
score<- apply(z,1,mean)
roster<-cbind(roster, score)
roster

# Problem 2: Issue B- Solution: Grade students
y <- quantile(score, c(.8,.6,.4,.2))
roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
roster

options(digits = 2) # limits the number of digits printed after the decimal places to 2. easier to read
roster

#Problem 3: Extract last and first names
name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,Lastname, roster[,-1])
roster <- roster[order(Lastname,Firstname),]

roster
#Problem 3-Issue C: Sort by last and first names
roster<- roster[order(Firstname,Lastname),]
roster
