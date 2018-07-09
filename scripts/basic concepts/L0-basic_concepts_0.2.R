#Introduction to R: Control Flow + Data Structures = R Programs

# Starting with simple 'if'
num <- 2 # some value to test with
if (num > 0) 
  print("num is positive")
Sometimes 'if' has its 'else'
if (num > 0) { # test to see if it's positive
  print("num is positive") # print in case of positive number
} else {
  print("num is negative") # it's negative if not positive
}

# Multiple 'else's are also possible
if (!is.numeric(num)) { # check if it's numeric
  print("this is not a number") # print this if it isn't numeric
} else if (num > 0) { # check if it's greater than 0
  print("num is positive") # print if it's positive
} else if (num < 0) { # maybe it is negative
  print("num is negative") # print it if it's < 0
} else print("ZERO!") # who knows, maybe it is zero

# R is vectorized so there's vectorized if-else
simple_vect <- c(1, 3, 12, NA, 2, NA, 4) # just another num vector with NAs
ifelse(is.na(simple_vect), "nothing here", "some number") # nothing here if it's
# NA or it's a number

# For loop is always working same way
for (i in simple_vect) print(i) # iterate in set of values

# Be aware that loops can be slow if
vec  <-  numeric()
system.time(
  for(i in seq_len(50000-1)) 
    {
    some_calc <- sqrt(i/10)
    vec <- c(vec, some_calc) # this is what makes it slow
    }  
)

# This solution is slightly faster
iter <- 50000
vec <- numeric(length=iter) # this makes it faster...
system.time(
  for(i in seq_len(iter-1)) {
    some_calc <- sqrt(i/10)
    vec[i] <- some_calc # ...not this!
  }
)

# This solution is even more faster
iter <- 50000
vec <- numeric(length=iter) # not because of this...
system.time(
  for(i in seq_len(iter-1)) {
    vec[i] <- sqrt(i/10) # ...but this!
  }
)

# Another example how loops can be slow (loop vs vectorized function)
iter <- 50000

system.time(
  for (i in 1:iter) {
    vec[i] <- rnorm(n=1, mean=0, sd=1) # approach from previous example
  }
)

system.time(y <- rnorm(iter, 0, 1)) # but this is much much faster

# R also knows about while loop
r <- 1 # initializing some variable
while (r < 5) { # while r < 5
  print(r) # print r
  r <- r + 1 # increase r by 1
}

# Loops can be nested
for(i in 1:5) { # outer loop
  for(j in 1:5) { # inner loop
    print(paste0(i,j)) # some code
  }
}

# Loops can be altered using break and next
for(i in 1:5) {
  if (i == 4) break # jump out of loop if condition is true
  print(i)
}

for(i in 1:5) {
  if (i == 4) next # just skip current iteration if condition is true
  print(i)
}

# Nope, we didn't forget 'repeat' loop
i <- 1
repeat { # there is no condition...
  print(i)
  i <- i + 1
  if (i == 10) break # ...so we have to break it if we don't want infinite loop
}

# And there's something called 'switch' :)
switch(2, "data", "science", "serbia") # choose one option based on value

# More on switch:
switchIndicator <- "A"
switchIndicator <- "switchIndicator"
switchIndicator <- "AvAvAv"
# rare situations where you do not need to enclose strings: ' ', or " "
switch(switchIndicator,
       'A' = {print(switchIndicator)},
       'switchIndicator' = {unlist(strsplit(switchIndicator,"h"))},
       'AvAvAv' = {print(nchar(switchIndicator))}
)
# is the same as:
switch(switchIndicator,
       A = {print(switchIndicator)},
       switchIndicator = {unlist(strsplit(switchIndicator,"h"))},
       AvAvAv = {print(nchar(switchIndicator))}
)

# now:
type = 2
cc <- c("A", "B", "C")
switch(type,
       c1 = {print(cc[1])},
       c2 = {print(cc[2])},
       c3 = {print(cc[3])},
       {print("Beyond C...")} # default choice
)

# BUT if you do this, R will miss the default choice, so be careful w. switch:
type = 4
cc <- c("A", "B", "C")
switch(type,
       print(cc[1]),
       print(cc[2]),
       print(cc[3]),
       {print("Beyond C...")} # the unnamed default choice works only if previous choices are named!
)

# Switch and if-else are similar, but switch is faster (believe us!)
i <- 2
if(i == 1) {
  print("data")
} else if(i == 2) {
  print("science")
} else print("serbia")


#########################################################
### Exercise, exercise...

library(datasets)
head(iris)
is.data.frame(iris)

# suming up Sepal.Length + Sepal.Width
for (i in 1:length(iris$Sepal.Length)) {
  iris$Sepal.LW[i] <- iris$Sepal.Length[i] + iris$Sepal.Width[i]
}
iris$Sepal.LW <- NULL

# remember: this is a vector programming language:
iris$Sepal.LW <- iris$Sepal.Length + iris$Sepal.Width # avoid loops anytime when possible!
iris$Sepal.LW <- NULL

out <- length(iris$Sepal.Length)
i <- 0
repeat
{
  i <- i+1
  iris$Sepal.LW[i] <- iris$Sepal.Length[i] + iris$Sepal.Width[i]
  if(i==out) break
}
# there are many more stupid ways to do this, all in order to avoid the simple and fast:
iris$Sepal.LW <- iris$Sepal.Length + iris$Sepal.Width
iris$Sepal.LW <- NULL

# for example, by avoiding loops altogether...
iris$Sepal.LW <- apply(data.frame(iris$Sepal.Length, iris$Sepal.Width), 1, function(x) {sum(x)})
iris$Sepal.LW <- NULL

# ifelse in lapply in unlist
sepalLengthMean <- mean(iris$Sepal.Length)
iris$Sepal.LW <- unlist(lapply(iris$Sepal.Length, function(x){ifelse(x>=sepalLengthMean,T,F)}))
iris$Sepal.LW <- NULL
# however...
iris$Sepal.LW <- ifelse(iris$Sepal.Length>=mean(iris$Sepal.Length),T,F)
# now leave the iris data set alone...
iris$Sepal.LW <- NULL

#########################################################
### vectorization in R

dataSet <- USArrests
head(dataSet)

# data$Murder, data$Assault, data$Rape: columns of data

# in behavioral sciences (psychology or biomedical sciences, for example) we would call them:
# variables (or factors, even more often)
# in data science and machine learning, we usually call them: FEATURES
# in psychology and behavioral sciences, the usage of the term "feature" is usually constrained
# to theories of categorization and concept learning

# Task: classify the US states according to some global indicator of violent crime
# Two categories (simplification): more dangerous and less dangerous (F)
# We have three features: Murder, Rape, Assault, all per 100,000 inhabitants
# The idea is to combine the three available features.

# Let's assume that we arbitrarily assign the following preference order over the features:
# Murder > Rape > Assault
# in terms of the severity of the consequences of the associated criminal acts

# Let's first isolate the features from the data.frame
featureMatrix <- as.matrix(dataSet[, c(1,4,2)])
head(featureMatrix)
# Let's WEIGHT the features in accordance with the imposed preference order:
weigthsVector <- c(3,2,1) # mind the order of the columns in featureMatrix

# Essentially, we want our global indicator to be a linear combination of all three selected features
# Where each feature is weighted by the corresponding element of the weigthsVector:

featureMatrix <- cbind(featureMatrix,numeric(length(featureMatrix[,1])))
head(featureMatrix)
for (i in 1:length(featureMatrix[,1])) {
  featureMatrix[i,4] <- sum(weigthsVector*featureMatrix[i,1:3])
  # don't forget: this "*" multiplication in R is vectorized and operates element-wise
  # we have a 1x3 weightsVector and a 1x3 featureMatrix[i,1:3], Ok
  # sum() then produces the desired linear combination
}

# Classification in the simplest case, let's simply take a look at the distribution of our global indicator:
hist(featureMatrix[,4],20) # it's multimodal and not too symmetric go for median
criterion <- median(featureMatrix[,4])
# And classify:
dataSet$Dangerous <- ifelse(featureMatrix[,4]>=criterion,T,F)

# Ok. You will never do this before you have a model that has actually *learned* the
# most adequate feature weights. This is an exercise only.

# ***Important***: have you seen the for loop above? Well...
# N e v e r  d o  t h a t.
dataSet$Dangerous <- NULL

# In Data Science, you will be working with huge amounts of quantitative data.
# For loops are slow. But in vector programming languages like R...
# matrix computations are seriously fast.

# What you ***want to do*** is the following:

# Let's first isolate the features from the data.frame
featureMatrix <- as.matrix(dataSet[, c(1,4,2)])
# Let's WEIGHT the features in accordance with the imposed preference order:
weigthsVector <- c(3,2,1) # mind the order of the columns in featureMatrix

# Feature weighting:
wF <- weigthsVector %*% t(featureMatrix)
# In R, t() is for: transpose
# In R, %*% is matrix multiplication

# oh yes: R knows about row and column vectors - and you want to put this one
# as a COLUMN in your dataSet data.frame, while wF is currently a ROW vector, look:
wF
length(wF)
wF <- t(wF) 

# and classify:
dataSet$Dangerous <- ifelse(wF>=median(wF),T,F)
