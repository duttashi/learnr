# create a function to test the arguments

# clean the workspace
rm(list = ls())
testArgs<- function(data){
  
  # if(!is.matrix(aargu) | !(is.data.frame(aargu))){ 
  #   stop("The data must be a matrix format or data frame format")
  # }
  if (!is.matrix(data) & !is.data.frame(data)) {
    stop("The data must be a matrix or a data frame.")
  }
  else{
  str(data)
  print("All ok")
  }
} 

aa<- c(1:10)
testArgs(aa)

?data.frame
df<- data.frame(x=c(1:10),y=c(11:20))
df                
testArgs(df)
