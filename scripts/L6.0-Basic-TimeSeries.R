# Time series: Basic Introduction
# This lecture is adapted from the book, "Introductory Time Series in R"

# Time Series R code
# Ashish Dutt
data("AirPassengers")
AP<-AirPassengers
class(AP)
start(AP)
end(AP)
frequency(AP) 
summary(AP)
# creating time plot
plot(AP, ylab="Passengers (1000s)")
# it is apparent that the number of passengers travelling on the airline is increasing with time. In general, a systematic change in a time series that does not appear to be periodic is known as a trend.
# A repeating pattern within each year is known as seasonal variation, although the term is applied more generally to repeating patterns within any fixed period, such as restaurant bookings on different days of the week. There is clear seasonal variation in the air passenger time series.
# However, many time series exhibit trends, which might, for example, be part of a longer cycle or be random and subject to unpredictable change. Random, or stochastic, trends are common in economic and financial time series. A regression model would not be appropriate for a stochastic trend.
# A time series plot not only emphasises patterns and features of the data but can also expose outliers and erroneous values. One cause of the latter is that missing data are sometimes coded using a negative value.  
# To get a clearer view of the trend, the seasonal effect can be removed by aggregating the data to the annual level, which can be achieved in R using the aggregate function. A summary of the values for each season can be viewed using a boxplot, with the cycle function being used to extract the seasons for each item of data.
# The plots can be put in a single graphics window using the layout function, which takes as input a vector (or matrix) for the location of each plot in the display window. 
layout(1:2)
plot(aggregate(AP))
boxplot(AP~cycle(AP))
# You can see an increasing trend in the annual series and the seasonal effects in the boxplot. More people travelled during the summer months of June to September

# Decomposition in R
# In R, the function decompose estimates trends and seasonal effects using a moving average method. Nesting the function within plot (e.g., using                                                            plot(stl())) produces a single figure showing the original series xt and the decomposed series mt, st, and zt. 
plot(decompose(AP))
AP.decom <- decompose(AP, type = "mult")
plot(AP.decom)
Trend<- AP.decom$trend
Seasonal<- AP.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2) # Electricity production data: trend with superimposed multiplicative seasonal effects.




