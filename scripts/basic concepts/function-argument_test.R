# create a function to test the arguments

# clean the workspace
rm(list = ls())
testArgs<- function(data){
  
  if (!is.matrix(data) & !is.data.frame(data)) {
    stop("The data must be a matrix or a data frame.")
  }
  else{
  str(data)
  print("All ok")
  }
} 

# create some test data to test the function
# a vector
aa<- c(1:10)
testArgs(aa)

# a data frame
df<- data.frame(x=c(1:10),y=c(11:20))
testArgs(df)
