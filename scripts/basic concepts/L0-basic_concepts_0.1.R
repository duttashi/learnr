# Introduction to lists and functions in R
# clear all
rm(list=ls())

# It's time to speak about lists
num_vect<-c(1:10)
char_vect<-c("data","science")
data_frame<-data.frame(num_vect,char_vect)
head(data_frame)
lista<-list(data_frame,num_vect,char_vect)
lista
str(lista)
length(lista)
as.list(char_vect) # another way to create a list

#List manipulation
names(lista)<-c("number","string","xyz")
head(lista)
lista[2]
lista[3]
is.list(lista[3])
is.list(lista[[3]])


class(lista[[3]]) # also a list? not be so sure!

lista$words # we can also extract an element this way
lista[["words"]] # or even like this

length(lista$words) # 2 as expected

lista[["words"]][1] # digging even deeper

lista$new_elem <- c(TRUE, FALSE, FALSE, TRUE) # add new element

length(lista) # now list has 4 elements
lista$new_elem <- NULL # but we can remove it easily

new_vect <- unlist(lista) # creating a vector from list

# Introduction to Functions in R
# elementary: a defition of a function in R
fun <- function(x) x+10
fun(5)

# taking two arguments
fun2 <- function(x,y) x+y
fun2(3,4)

#multiple R expresions in the function body
fun<-function(x,y)
  {
    a<-sum(x)
    b<-sum(y)
    a-b
  }
