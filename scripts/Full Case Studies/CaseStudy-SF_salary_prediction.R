# San Franscisco Salaries data from Kaggle
# reference: https://www.kaggle.com/kaggle/sf-salaries/

# clear environment
rm(list = ls())
# load the libraries
library(readr) # for read_csv()
d1<- read_csv("data/Salaries.csv", na=c("Not given"))
dim(d1)
colSums(is.na(d1)) # BasePay has 609 missing values
glimpse(d1)

# So - Notes contains no information; Agency is all the same; and Status is blank for most users.
# dropping notes and agency
d1$Notes<- NULL
d1$Agency<-NULL
str(d1)
# check variable Benefits
head(d1$Benefits) # blank values
# so how about unique values
head(unique(d1$Benefits))
# When does the first non-empty value of benefits occur?
which.min(d1$Benefits=="") # 36160 blank rows

d1<- d1 %>%
  mutate(Benefits= as.numeric(Benefits))
# Update the list of non-numeric vars
non_numeric_vars <- names(d1)[!sapply(d1, is.numeric)]


# non-numeric variables
# How many unique values are there in non numeric variables?
non_numeric_vars<- names(d1)[!sapply(d1, is.numeric)]
d1 %>%
  select(one_of(non_numeric_vars))%>%
  summarise_each(funs(unique_vars=length(unique(.)))) # Employeename has the most unique values

# Job Title
# lets look at the most frequent job title
d1%>%
  group_by(JobTitle)%>%
  summarise(Frequency=n())%>%
  arrange(desc(Frequency))%>%
  head()