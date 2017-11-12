# Objective: create a function to replace missing values from a data frame.
# Input to function will be a data framme
# 

rm(list = ls())
# Generate a sample dataset
set.seed(1014)
# create a dummay data frame
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
df
names(df) <- letters[1:6]
df
# create function to replace missing values with NA
fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
fix_missing(df)
df
df<- fix_missing(df)
df
