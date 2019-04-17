# Objective: How to filter multiple rows/columns containing negative values from a data frame having mixed data? 
# script create date: 17/4/2019
# Solution

# create dummy data
fac1<- c("1","2","3","4","1")
fac2<- c("1","3","2","3","3")
num1<- c(0.3,0.4,-1.23,-3.21,0.23)
num2<- c(-1.23,0.3,-3.21,0.21,0.12)

df<- data.frame(fac1,fac2, num1,num2)
df

# load library
library(dplyr)
str(mtcars)
# extract factor (or categorical) vars
fact_names<- names(df[,c(1:2)])
# Do the filtering
df%>%
  filter_at(vars(-fact_names), all_vars(.>=0)) 

df%>%
  filter_at(vars(-fact_names), any_vars(.>=0))

# count the frequency of categorical vars
plyr::count(df, fact_names)
plyr::count(df, 'fac1')
plyr::count(df, 'fac2')
plyr::count(mtcars, 'gear')
