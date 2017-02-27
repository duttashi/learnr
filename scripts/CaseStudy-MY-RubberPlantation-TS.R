# Script name: 
# Data Source: http://www.dosm.gov.my/v1/index.php?r=column/ctimeseries&menu_id=NHJlaGc2Rlg4ZXlGTjh1SU1kaWY5UT09

# clear the working space
rm(list = ls())
# Load the data
df.rubber<- read.csv("data/rubberestate-my-ts.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
str(df.rubber)
summary(df.rubber)

class(df.rubber) # The class is data.frame, this needs to be changed to ts object
library(zoo) # Load the zoo package for conversion to ts object
df.rubber.ts<- as.ts(df.rubber, start=c(1965),end=c(2014),frequency=1)
is.ts(df.rubber.ts) # check if time series object or not

# EDA
start(df.rubber.ts)
end(df.rubber.ts)
sum(is.na(df.rubber.ts)) # 8 missing values

frequency(df.rubber.ts)
summary(df.rubber.ts)

# Initial plots
plot(df.rubber.ts)

layout(1:2)
plot(df.rubber.ts, ylab="Simple TS")
plot(aggregate(df.rubber.ts), ylab="Aggregrated TS")
dev.off() # invoking Rstudio to initiate a new graphing device
# https://support.rstudio.com/hc/en-us/articles/200488548-Problem-with-Plots-or-Graphics-Device
boxplot(df.rubber.ts~cycle(df.rubber.ts))

library(forecast)
plot(decompose(df.rubber.ts, type = "mult"))
