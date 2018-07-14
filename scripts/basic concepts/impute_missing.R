# write a function ImputeMissing() to do the following tasks
# 1. read data
# 2. check if its in df form or not,if not a dataframe then convert to df
# else continue
# 3. Loop through the columns, impute median for continuous missing values or impute mode for categorical missing values.
# 4. return the df
# The ImputeMissing function will not work for factor levels (keep this in a to-do list for future update)

# mode imputation function
Mode <- function (x, na.rm) {
  # reference: https://stackoverflow.com/questions/7731192/replace-mean-or-mode-for-missing-values-in-r?rq=1
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) x <- xmode
  return(x)
}

ImputeMissing<- function(data=data){
  # check if data frame
  if(!(is.data.frame(data))){
    data<- as.data.frame(data)
  }
  
  # Loop through the columns of the dataframe
  for(i in 1:ncol(data))
    {
    
    if(class(data[,i]) %in% c("numeric","integer")){
      # missing continuous data to be replaced by median 
        data[is.na(data[,i]),i] <- median(data[,i],na.rm = TRUE)
      } # end inner-if
    
    else if(class(data[,i]) %in% c("character","factor")){
      # missing continuous data to be replaced by mode 
      data[is.na(data[,i]),i] <- Mode(data[,i], na.rm = TRUE)
      } # end else if
    } # end for
  return(data)
  } # end function
  


# create data frame with missing values
df <- data.frame(col1 = c(1:3, NA),
                 col2 = c("this",NA ,NA, "text"), 
                 col3 = c(2.5, 4.2, 3.2, NA),
                 stringsAsFactors = FALSE)
# test
str(df)
ImputeMissing(df)

#fake array:
age<- c(5,8,10,12,NA)
a <- factor(c("aa", "bb", NA, "cc", "cc"))
b <- c("banana", "apple", "pear", "grape", NA)
df_test <- data.frame(age=age, a=a, b=b)
df_test$b <- as.character(df_test$b)

# another test
str(df_test)
# does not work for factor levels
ImputeMissing(df_test)
