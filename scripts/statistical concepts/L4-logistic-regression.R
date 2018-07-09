# load the data
dress.data<-read.csv("data/dress_data.csv",header = T, sep = ",")
str(dress.data)
logisticModel <- glm (Recommended ~ .-ID, data = dress.data, family = binomial)
summary(model)
#We can examine the error rates of our model with the single line:
table(dress.data$Recommended,predict(logisticModel,type='response')>=0.5)
# We can also look at this graphically:
library(lattice)
densityplot(predict(logisticModel,type='link'),groups=dress.data$Season!='winter',auto.key=T)

table(dress.data$Size)
