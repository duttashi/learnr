# Question: How to cut date vector with self-defined breaks?
# Reference: stackoverflow (https://stackoverflow.com/questions/56843548/cut-date-vector-with-self-defined-breaks)

# data
set.seed(42)
n <- 50
sample <- data.frame(date=seq(as.Date("2019/1/1"), by="day", length.out=n),
                     matrix(rnorm(4*n, 100, 50), ncol=4, 
                            dimnames=list(NULL, paste0("X", 1:4))))
breaks <- c(0, 7, 15, 30, 50)
head(sample)

library(dplyr)
sample %>%
  group_by(mygrp=cut(as.numeric(factor(date)), breaks=breaks)) %>%
  summarise(m1=mean(X1), m2=mean(X2))
