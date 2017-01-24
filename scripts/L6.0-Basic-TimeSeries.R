# Time series: Basic Introduction
# Load the built in Air Passengers Time Series dataset
AP<- ts(AirPassengers)
# Step 1: Initial data exploration
View(AP)
class(AP)
start(AP)
end(AP)
frequency(AP) 
summary(AP)
sum(is.na(AP)) # check for missing values

# Step 2: Data exploration
# a. Outlier detection
library(tsoutliers) 
AP.outliers<- tso(AP)
AP.outliers
# b. Initial Data Visualization
plot(AP, ylab="Passengers (1000s)")
# it is apparent that the number of passengers travelling on the airline is increasing with time. In general, a systematic change in a time series that does not appear to be periodic is known as a trend.
# A repeating pattern within each year is known as seasonal variation, although the term is applied more generally to repeating patterns within any fixed period, such as restaurant bookings on different days of the week. There is clear seasonal variation in the air passenger time series.
# However, many time series exhibit trends, which might, for example, be part of a longer cycle or be random and subject to unpredictable change. Random, or stochastic, trends are common in economic and financial time series. A regression model would not be appropriate for a stochastic trend.
# A time series plot not only emphasises patterns and features of the data but can also expose outliers and erroneous values. One cause of the latter is that missing data are sometimes coded using a negative value.  
# To get a clearer view of the trend, the seasonal effect can be removed by aggregating the data to the annual level, which can be achieved in R using the aggregate function. A summary of the values for each season can be viewed using a boxplot, with the cycle function being used to extract the seasons for each item of data.
# The plots can be put in a single graphics window using the layout function, which takes as input a vector (or matrix) for the location of each plot in the display window. 
layout(1:2)
plot(AP, ylab="Simple TS")
plot(aggregate(AP), ylab="Aggregrated TS ")
boxplot(AP~cycle(AP))

# You can see an increasing trend in the annual series and the seasonal effects in the boxplot. More people travelled during the summer months of June to September

# Step 3: Decomposition in R
# In R, the function decompose estimates trends and seasonal effects using a moving average method. Nesting the function within plot (e.g., using                                                            plot(stl())) produces a single figure showing the original series xt and the decomposed series mt, st, and zt. 
plot(decompose(AP))
AP.decom <- decompose(AP, type = "mult") # # use type = "additive" for additive components
plot(AP.decom)
plot(decompose(AP, type = "add"))

Trend<- AP.decom$trend
Seasonal<- AP.decom$seasonal
Error<- AP.decom$x
plot(Error)
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2) # Electricity production data: trend with superimposed multiplicative seasonal effects.

# How to test if a time series is stationary?
library(tseries)
adf.test(AP) # p-value < 0.05 indicates the TS is stationary
kpss.test(AP)

#Simple Moving Average
#Simple moving average can be calculated using ma() from forecast

plot(AP) # first plot the ts object
sm <- ma(AP, order=12) # 12 month moving average
lines(sm, col="red") # plot the moving average line on the previous plot

# Exponential smoothing
library(forecast)

# Simple exponential smoothing: Level Only
model <- hw(AP, initial = "optimal", h=(12), beta=NULL, gamma=NULL) # h is the no. periods to forecast

# Double Exponential smoothing: Level and Trend components
model <- hw(AP, initial = "optimal", h=(12), gamma=NULL)

# Holt Winters: Level, Trend and Seasonality
model <- hw(AP, initial = "optimal", h=(12))
plot(model)
accuracy(model) # calculate accuracy measures

# De-seasonlize a time series so as to see he seasonal pattern in the time series and helps to model the data without the seasonal effects.
library(forecast)
plot(decompose(AP, type = "additive"))
plot(decompose(AP, type = "mult"))
AP.decomp<- stl(AP, "periodic")
AP.deseason<- seasadj(AP.decomp)
plot(AirPassengers, type="l")  # original series
plot(AP.deseason, type="l") # seasonal adjusted
seasonplot(AP.deseason, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot: Airpassengers") # seasonal frequency set as 12 for monthly data.

# Step 4: Find optimal parameters
acf(log(AP))
pacf(diff(log(AirPassengers)))

# Step 5: ARIMA model and prediction
fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

