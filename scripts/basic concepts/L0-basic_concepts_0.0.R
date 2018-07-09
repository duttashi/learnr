#Introduction to vectors, matrices, and data frames in R
# clear all
rm(list=ls())

# Let's start with some vectors
char_list <- character(length = 0) #empty character list
num_list <- numeric(length = 10) #length can be != 0, but 0 is default value
log_list <- logical(length = 3) #default value is FALSE

# But you can always use good ol' c() for the same purpose
log_list_2 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE) #some Ts and Fs
num_list_2 <- c(1, 4, 12, NA, 101, 999) #numb
char_list_2 <- c("abc", "qwerty", "test", "data", "science")
# Factor vectors are also part of R
fac_list <- gl(n = 4, k = 1, length = 8, ordered = T, labels = c("low", "med", "high", "very high")) 
fac_list
class(fac_list)
typeof(fac_list)

# Subsetting is regular-thing-to-do when using R
char_list_2[5] #single element can be selected
log_list_2[2:4] #or some interval
num_list_2[3:length(num_list_2)] #or even length() function

# New objects can be created when subsetting
test <- num_list_2[-c(2,4)] #or somthing like this - displays all but 2nd and 4th element
test_2 <- num_list_2 %in% test #operator %in% can be very useful
not_na <- num_list_2[!is.na(num_list_2)] #removing NAs using operator ! and is.na() function

# Vector ordering
sort(test, decreasing = T) #using sort() function
test[order(test, decreasing = T)] #or with order() function

# Vector sequences
seq(1,22,by = 2) #we already mentioned seq()
rep(1, 4) #but rep() is something new :)
rep(num_list_2, 2) #replicate num_list_2, 2 times

# Concatenation
new_num_vect <- c(num_list, num_list_2) #using 2 vectors to create new one
new_num_vect
new_combo_vect <- c(num_list_2, log_list) #combination of num and log vector
new_combo_vect #all numbers? false to zero? coercion in action

new_combo_vect_2 <- c(char_list_2, num_list_2) #works as well
new_combo_vect_2 #where are the numbers?
class(new_combo_vect_2) #all characters

# Matrices are available in R
matr <- matrix(data = c(1,3,5,7,NA,11), nrow = 2, ncol = 3) #2x3 matrix
class(matr) #yes, it's matrix
typeof(matr) #double as expected

matr[,2] #2nd column
matr[3,] #oops, out of bounds, there's no 3rd row
matr[2,3] #element in 2nd row and 3rd column

matr_2 <- matrix(data = c(1,3,5,"7",NA,11), nrow = 2, ncol = 3) #another 2x3 matrix
class(matr_2) #matrix again
typeof(matr_2) #but not double anymore, type conversion in action!
t(matr_2) #transponed matr_2

# What can we do if a matrix needs to encompass different types of data?
# Introducing data frame!

library(datasets) #there are some datasets in base R like mtcars
cars_data <- mtcars

# Some useful information about data frames
str(cars_data) #lets see what we have here
summary(cars_data) #more information about mtcars dataset
names(cars_data) #column names
?mtcars # documentation 

# statistics on data frame
mean(cars_data$mpg)
median(cars_data$cyl)
is.list(cars_data[1,])

# Lets do some data frame subsetting
cars_data
cars_data[-1,] # remove the first row
cars_data[,-1] # remove the first col
cars_data[c(1,3)] #keeping 1st and 3rd column only
cars_data[-c(1,3)] #removing 1st and 3rd column
cars_data[ ,-c(1,3)] #same as the previous line of code

cars_data[!duplicated(cars_data$mpg), ] #maybe we want to remove all cars with same mpg?
#remember it keeps only the first occurence!

# Data Subsetting
subset(cars_data, mpg < 19) #this is one way (and it can be slow!)
cars_data[cars_data$mpg < 19, ] #this is another one (faster)
cars_data[which(cars_data$mpg < 19), ] #and another one (usually even more faster)

cars_data[cars_data$mpg > 20 & cars_data$am == 1, ] #multiple conditions

cars_data[grep("Merc", row.names(cars_data), value=T), ] #filtering by pattern match

# Data frame transformations
cars_data$trans <- ifelse(cars_data$am == 0, "automatic", "manual") #we can add new colums
head(cars_data)
cars_data$trans <- NULL #or we can remove them
head(cars_data)

cars_data[c(1:3,11,4,7,5:6,8:10)] #this way we change column order

# Separation and joining of data frames
low_mpg <- cars_data[cars_data$mpg < 15, ] #new data frame with mpg < 15
high_mpg <- cars_data[cars_data$mpg >= 15, ] #new data frame with mpg >= 15
head(low_mpg)
head(high_mpg)
mpg_join <- rbind(low_mpg, high_mpg) # we can combine 2 data frames like this
head(mpg_join)
car_condition <- data.frame(sample(c("old","new"), replace = T, size = 32)) #creating random data frame
#with "old" and "new" values
names(car_condition) <- "condition" #for all kinds of objects
colnames(car_condition) <- "condition" #for "matrix-like" objects, but same effect here
rownames(car_condition) <- rownames(cars_data) #use row names of one data frame as row names of other

mpg_join <- cbind(mpg_join, car_condition) #or combine data frames like this
head(mpg_join)
