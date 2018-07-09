df<-data.frame(time1=rbinom(100,1,0.3),
               time2=rbinom(100,1,0.4),
               time3=rbinom(100,1,0.5),
               time4=rbinom(100,1,0.6))

insert_nas <- function(x) {
  len <- length(x)
  n <- sample(1:floor(0.2*len), 1)
  i <- sample(1:len, n)
  x[i] <- NA 
  x
}

df2 <- sapply(df, insert_nas)
df2

sum(is.na(df2))
colSums(is.na(df2))

# missing value imputation
library(mice)
md.pattern(df2) # The output tells us that 61 samples are complete, 17 sample is missing the time1, 7 samples is missing in time2,3 samples is missing time3, and so on
tempData <- mice(df2,m=5,maxit=50,meth='pmm',seed=500)
df2.complete<- complete(df2,1)
# inspecting the distribution of original and imputed data
library(lattice)
xyplot(tempData, time3~time2+time4+time1, pch=18, cex=1)
# What we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed). The matching shape tells us that the imputed values are indeed “plausible values”.
# Another helpful plot is the strip plot:
stripplot(tempData, pch = 20, cex = 1.2)
