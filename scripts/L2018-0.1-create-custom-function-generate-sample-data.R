## Objective: To build a custom function to create sample data. 
# Function parameters: The function will take the nrows, ncols, the samplesize and the column names 
# Function Output: A data frame
# Script create date: 02/05/2018


# clean the workspace
rm(list=ls())
makeData<- function(nrows,ncols, varnames)
{
  if(ncols<2) stop("column number must be greater than 1")
  else{
    DF<- data.frame(matrix(nrows,ncols))
    #DF<- data.frame(matrix(nrows,ncols,dimnames=list(varnames)))
    colnames(DF)<-varnames
    #DF[nrows, ncols]<- sample(c(nrows:ncols))
    #print(colnames(DF))
  } 
  return(DF)
} 

makeData(10,2,c("a","b"))
makeData(2,3,c("a"))
