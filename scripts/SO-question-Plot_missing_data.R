# clean the workspace
rm(list = ls())
# script objective: To plot missing data using geom_line()
# create some dummy data
df <- data.frame(A = 1:10, B = 11:20, c = 21:30)
# Now, introduce some missing values
set.seed(4)
df<- as.data.frame(lapply(df,function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
head(df)

library(ggplot2)
ggplot(data = df, aes(x=A, y=B))+
  geom_point()+ 
  geom_line()


ggplot(data = df, aes(x=A, y=B))+
  geom_point(data = na.omit(df))+
  geom_line(data = na.omit(df))
