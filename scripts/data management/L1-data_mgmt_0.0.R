# Creating a Dataset
# Examples given in this script are from the book "R in Action",Chapter 4

# clear the workspace
rm(list=ls())

# Vectors: are one dimensional array that can hold just one type of data. created by the c()
a<-c(1:10)
b<-c("one","two","three","five")
# Matrices: are two dimensional array with the same mode. Created by matrix()
c<- matrix(1:10,nrow=5,ncol=2)
c
cells<- c(1,10,11,34)
rnames<-c("R1","R2")
cnames<-c("C1","C2")
mymatrix<-matrix(cells, nrow = 2,ncol = 2,byrow = TRUE, dimnames = list(rnames,cnames))
mymatrix
# Arrays: similar to matrices but with more than two dimesnions. Created using the array()
dim1<-c("A1","A2")
dim2<-c("B1","B2","B3")
dim3<-c("C1","C2","C3","C4")
z<-array(1:10, c(2,3,4), dimnames = list(dim1,dim2,dim3))
z
# Dataframe: is a generalised form of matrix that can contain different modes of data. Created by data.frame()
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(patientID, age, diabetes, status)
patientdata
table(patientdata$diabetes, patientdata$status)

# Factors: the variables can be described as nominal, ordinal or continuous. 
# The nominal variables are categorical without an implied order example, Diabetes (Type1, Type2)
# Ordinal variables imply an order but not amount. exmp: Status(poor,improved,excellent)
# The categorical (ordinal) and ordered categorical (ordinal) variables in R are called Factors.
# The function factor() stores the categorical values as a vector of integers in the range [1..k](where k is the number of unique values in the nominal variable)
diabetes<- factor(diabetes) # stores this vector as (1,2,1,1) and associates it with 1=Type1 and 2=Type2 internally. This assignment is alphabetically
str(diabetes)
status <- c("Poor", "Improved", "Excellent", "Poor")
status<-factor(status)
str(status)
status<-factor(status, ordered = TRUE) # For Factors representing ordinal variables you must add ordered=TRUE. 
                                       # This will be internally encoded as 1= Excellent, 2=Improved, 3=Poor
str(status)
# You can override the default by specifying the levels option. For example,
status<-factor(status, ordered = TRUE, levels = c("Poor", "Improved", "Excellent"))
str(status) # Now as you can see, 1=Poor, 2=Improved, 3=Excellent

# Lists: is the most complex of the R data types. It is an ordered collection of diverse datatypes
# Creating a list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)
mylist
