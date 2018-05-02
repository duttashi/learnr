# Outlier detection and removal

# We start by visualising the cars dataset. This data consist of two columns namely car speed and distance it traveled. 
# Type, help(cars) in the R console to look at the data
plot(cars$speed, cars$dist, xlim=c(0, 28), ylim=c(0, 230), main="Without Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars), col="blue", lwd=3, lty=2)

# Inject outliers into data.
cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

# Plot of data with outliers.
par(mfrow=c(1, 2))
plot(cars2$speed, cars2$dist, xlim=c(0, 28), ylim=c(0, 230), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars2), col="blue", lwd=3, lty=2)

# Plot of original data without outliers. Note the change in slope (angle) of best fit line.
plot(cars1$speed, cars1$dist, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars1), col="blue", lwd=3, lty=2)

# Once the outliers are identified, you can treat them by (a) Imputation- using mean, median or mode (b)values that lie outside the 1.5 * IQR limits, we could cap it by replacing those observations outside the lower limit with the value of 5th %ile and those that lie above the upper limit, with the value of 95th %ile. Below is a sample code that achieves this.
cars_outliers_removed <- cars2 [cars2$dist > quantile(cars2$dist, .25)- 
                                      1.5*IQR(cars2$dist) &
                                cars2$dist < quantile(cars2$dist, .75)+
                                      1.5*IQR(cars2$dist),]
par(mfrow=c(1, 1))
# plot the outliers removed data
plot(cars_outliers_removed$speed, cars_outliers_removed$dist, xlim=c(0, 28), ylim=c(0, 230), main="Outliers removed \n A much better fit!", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(dist ~ speed, data=cars_outliers_removed), col="blue", lwd=3, lty=2)

