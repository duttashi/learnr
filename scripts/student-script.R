# Data Source: https://archive.ics.uci.edu/ml/datasets/Student+Performance#
# 

d1=read.table("data/student-mat.csv",sep=";",header=TRUE)
d2=read.table("data/student-por.csv",sep=";",header=TRUE)
names(student.mat)
names(student.por)

d3=merge(student.mat,student.por,by=c("school","sex","age","address", "famsize", 
                                      "Pstatus","Medu",  "Fedu",  "Mjob",  "Fjob",  
                                      "reason","guardian","traveltime", "studytime", 
                                      "failures","schoolsup","famsup","paid",
                                      "activities","nursery", "higher","internet",
                                      "romantic","famrel","freetime","goout",
                                      "Dalc","Walc","health","absences","G1","G2","G3"))

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu",
                    "Fedu","Mjob","Fjob","reason","nursery","internet"))
d4=rbind(d1,d2)
print(nrow(d4)) # 382 students

# Descriptive stats
# load the library
library(caret)
# check for near zero variance predictors
nzv<- nearZeroVar(d4)
names(d4[nzv]) 

# check for high correlated predictors
str(d4)
d4.cont<-d4[c(3,7:8,13:15,24:33)]
str(d4.cont)
d4.cont.cor<-cor(d4.cont)
d4.cont.highCor<-findCorrelation(d4.cont.cor, cutoff = 0.80)
names(d4[d4.cont.highCor]) # failures and study time are highly correlated

d4.clean<-d4[,-c(14:15)]
str(d4.clean)
