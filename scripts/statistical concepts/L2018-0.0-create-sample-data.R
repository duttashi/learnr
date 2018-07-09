# Objective: Often in a data analysis task there is a requirement to prepare sample dataset. The objective if this script is to familiarise the analyst of the different methods the dataset can be prepared.

# R has several base functions that make the sampling process quite easy and fast.
# Below is an explanation of the main functions used in the current set of exercices:
# 1. set.seed() – Although R executes a random mechanism of sample creation, set.seed() function allows us to reproduce the exact sample each time we execute a random-related function.
# 
# 2. sample() – Sampling function. The arguments of the function are:
#   x – a vector of values,
# size – sample size
# replace – Either use a chosen value more than once or not
# prob – the probabilities of each value in the input vector.
# 
# 3. seq()/seq.Date() – Create a sequence of values/dates, ranging from a ‘start’ to an ‘end’ value.
# 
# 4. rep() – Repeat a value/vector n times.
# 
# 5. rev() – Revert the values within a vector.
# 
# You can get additional explanations for those functions by adding a ‘?’ prior to each function’s name.


# Exercise 1
# 1. Set seed with value 1235
# 2. Create a Bernoulli sample of 100 ‘fair coin’ flippings.
# Populate a variable called fair_coin with the sample results.

# clean the workspace
rm(list = ls())
set.seed(1235)
?sample
fair_coin<- sample(c(0,1),100,replace=TRUE)

# Exercise 2
# 1. Set seed with value 2312
# 2. Create a sample of 10 integers, based on a vector ranging from 8 thru 19.
# Allow the sample to have repeated values.
# Populate a variable called hourselect1 with the sample results
hourselect1<- sample(c(8:19), 10, replace = FALSE)

# Exercise 3
# 1. Create a vector variable called probs with the following probabilities:
#   ‘0.05,0.08,0.16,0.17,0.18,0.14,0.08,0.06,0.03,0.03,0.01,0.01’
# 2. Make sure the sum of the vector equals 1.
probs<- sample(c(0.05,0.08,0.16,0.17,0.18,0.14,0.08,0.06,0.03,0.03,0.01,0.01))
sum(probs) 

# Exercise 4
# 1. Set seed with value 1976
# 2. Create a sample of 10 integers, based on a vector ranging from 8 thru 19.
# Allow the sample to have repeated values and use the probabilities defined in the previous question.
# Populate a variable called hourselect2 with the sample results
set.seed(1976)
hourselect2<- sample(c(8:19),10, replace = TRUE, prob = probs) 
hourselect2

# Exercise 5
# Let’s prepare the variables for a biased coin:
# 1. Populate a variable called coin with 5 zeros in a row and 5 ones in a row
# 2. Populate a variable called probs having 5 times value ‘0.08’ in a row and 5 times value ‘0.12’ in a row.
# 3. Make sure the sum of probabilities on probs variable equals 1.
coin<- rep(c(0,1), each=5)
coin 
probs<- rep(c(0.08,0.12), each=5)
sum(probs)

# Exercise 6
# 1. Set seed with value 345124
# 2. Create a biased sample of length 100, having as input the coin vector, and as probabilities probs vector of probabilities.
# Populate a variable called biased_coin with the sample results.
set.seed(345124)
biased_coin<- sample(coin, 100, replace = TRUE, prob = probs)

# Exercise 7
# Compare the sum of values in fair_coin and biased_coin
sum(fair_coin) 
sum(biased_coin)

# Exercise 8
# 1. Create a ‘Date’ variable called startDate with value 9th of February 2010 and a second ‘Date’ variable called endDate with value 9th of February 2005
# 2. Create a descending sequence of dates having all 9th’s of the month between those two dates. 
# Populate a variable called seqDates with the sequence of dates.
startDate<- as.Date("2010-02-09")
endDate<- as.Date("2005-02-09")  
seqDates<- seq.Date(startDate, endDate, by = "-1 month")
seqDates

# Exercise 9
# Revert the sequence of dates created in the previous question, so they are in ascending order and place them in a variable called RevSeqDates
RevSeqDates<- rev(seqDates)
RevSeqDates 

# Exercise 10
# 1. Set seed with value 10
# 2. Create a sample of 20 unique values from the RevSeqDates vector.
set.seed(10)
sample(RevSeqDates,20, replace = FALSE)

# Exercise 11
# R has function for making distributions. What command will list the distributions that R knows how to use?
# Read the help file on Distributions.
help("Distributions")

# Now we are going to use the distribution function to generate data.
# First we are going to make this data reproducible by setting seed
# Finish this command by choosing a number for the seed.
# set.seed()
set.seed(2018)

# Next we will make an object called Lbody about the length of cat’s body.
# This will be a continuous variable measured in inches. Change the parameters to another unit of measurement
Lbody <- rnorm(100, mean = 18, sd =3)

# Also we are going to weigh the cats. Change this code to your unit of measurement.
weightslbs <- rnorm(100, mean = 10,sd =3)

# What does rnorm() do?
help(rnorm)

# I measured a very large cat to get a mean of 18 inches. Change the code get lengths of cats in centimeters based on what you think the mean and standard deviation should be.
# Also change the code to what you think the mean age of cats is.
Age_of_cat <- rnorm(100, mean =12, sd =3)

# Now go back and change the distribution of the cats age to any distribution you would like.
Age_of_cat <- rpois(100, lambda = 3)
Age_of_cat <- rexp(100,1)

# Write code to generate a sample of 100 cats with or without claws
hasClaws<- sample(c(TRUE, FALSE), size = 100 , replace = TRUE)
# Write code to generate a sample of 100 cats with or without a tail
hasTail<- sample(c(TRUE, FALSE), size=100, replace = TRUE)
# Write code using function sample.int to generate 100 cats that have five different colors of coats
furColor<- sample.int(5, size = 100, replace = TRUE)
names(furColor)<- c("white","orange", "black","brown", "multi", "black and white")

# Write code to generate bells for the cats
b<- seq(2,10, by=2)
bells<- rep.int(b, times = 20)

# The cats like toys. Write code to generate toys for cats
cattoys <- sample.int(5, size = 100, replace = TRUE)
names(cattoys) <- c("mice","wheels","feathers","ball","bones", "block")

# Write code for two more variables, an id variable and a grouping variable of small, medium and large.
catid <- 1:100
catsize <- sample(c("small", "meduim", "large"),size = 100, replace = TRUE)
# Put all variables created for Exercise 11 in a adata frame
CatData <- data.frame(catid,Lbody,catsize,Age_of_cat,weightslbs, cattoys,hasClaws,hasTail,furColor,bells)



