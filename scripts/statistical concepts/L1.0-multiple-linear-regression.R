# MULTIPLE LINEAR REGRESSION

# Load the data
enroll.data<- read.csv("data/data_simple_regression.csv", header = T, sep = ",")

# check data structure
dim(enroll.data)
str(enroll.data)
names(enroll.data)

lm.model.2<- lm(ROLL~UNEM+HGRAD, data = enroll.data)
# display the linear model
lm.model.2
-8255.8 + 698.2 * 9 + 0.9 * 100000

lm.model.3<- lm(ROLL~UNEM+HGRAD+INC, data = enroll.data)
lm.model.3
-9153.3 + 450.1 * 9 + 0.4 * 100000 + 4.3 * 30000
summary(lm.model.3)