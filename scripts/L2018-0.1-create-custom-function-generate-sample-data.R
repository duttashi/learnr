## Objective: To build a custom function to create sample data. 
# Function parameters: The function will take the nrows, ncols, the samplesize and the column names 
# Function Output: A data frame
# Script create date: 02/05/2018


# clean the workspace
rm(list=ls())
makeData<- function(ncols, nrows, samplesize, varnames)
{
  
  if(ncols<2) stop("column number must be greater than 1")
  else{
    set.seed(2018)
    DF<-data.frame(sample(c(nrows:ncols, samplesize, replace=TRUE)))
  } # end else
  
  # assign column names
  #if (!is.null(varnames)) 
  colnames(DF) <- varnames
  print(colnames(DF))
  return(DF)
} # end function

makeData(2,2,2)
makeData(2, 4, 10)
makeData(3,4,10,c("a"))
