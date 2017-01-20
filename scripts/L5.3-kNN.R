# Case Study: Predicting Breast Cancer
# Libraries used: class, gmodels
# Algorithm: kNN classifier
# Code file name: L5.3-kNN.R

# Step 1. Read in the data
wbcd<- read.csv("data/wisc_bc_data.csv", sep = ",", stringsAsFactors = FALSE)

# Step 2. Exploratory Data analysis
## A peak at the data dimensions and structure
dim(wbcd)
#names(wbcd)
str(wbcd)

# Step 3: Data Management
# drop the id feature
wbcd<-wbcd[,-1]
# the variable diagnosis is our response variable that we have to predict. Let's explore it
table(wbcd$diagnosis) # 357 benign tumours and 212 malignant
str(wbcd$diagnosis) # is a character variable. Lets recode it as factor
# rename the levels
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
# Let's calculate the proportion
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
# The remaining 30 features are all numeric. Let us see a quick summary of 3 variables
summary(wbcd[c("radius_worst","texture_worst","perimeter_worst")])
# notice, the variance in the mean's of radius_worst and perimeter_worst. 
# Remember,  kNN is heavily dependent upon the measurement scale of the input features.

# Step 4. Data Transformation
# Normalizing the numeric data
# We create a normalize function that takes a vector x of numeric values, and for each value in x, subtract the minimum value in x and divide by the range of values in x.
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
  }
# Now., let's test the function with some vector values
normalize(c(10,20,30,40,50))
# Rather than normalizing each of the 30 numeric variables individually, we use the  lapply() function of R takes a list and applies a function to each element of the list.
# lapply() will return a list, so we need to convert this list to a data frame
wbcd.n<- as.data.frame( lapply(wbcd[,2:31], normalize))
# let's look at the summary statistic of a transformed variable
summary(wbcd.n$radius_mean)

# Step 5. Data preparation. Creating training and testing dataset

set.seed(51) # for results reproducibility
# Since, we do not have any additional test dataset, let's partition the existing dataset such that 70% is train data and 30% is test data. There are many methods to do this. I will use the most easiest one
wbcd.n.train<- wbcd.n[1:469,]
wbcd.n.test<-wbcd.n[470:569,]

# Note: When we constructed our training and test data, we excluded the target variable,
#diagnosis. For training the kNN model, we will need to store these class labels in
#factor vectors, divided to the training and test datasets
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# Step 6: Model training 
# load the class package for kNN implementation
library(class)

# knn() syntax from the class package
# p<-knn(train,test,class,k)
# where, train is a data frame containing numeric data; test is a data frame containing numeric data;
# class is a factor vector with the class for each row in the training data
# k is an integer indicating the number of nearest neighbours

wbcd.pred<- knn(train = wbcd.n.train, test=wbcd.n.test, cl=wbcd_train_labels, k=21)
# We chose, k=21 because square root of 469 is 21

# Step 7: Model Evalaution
# The next step of the process is to evaluate how well the predicted classes in the
# wbcd_pred vector match up with the known values in the wbcd_test_labels vector. To do this, we can use the CrossTable() function in the gmodels package
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd.pred, prop.chisq = FALSE)
# The top-left cell is True Negative (labelled TN) These 77 of 100 values indicate cases where the mass was benign, and the kNN algorithm correctly identified it as such.
# The bottom-right cell (labeled TP), indicates the true positive results, where the classifier and the clinically determined label agree that the mass is malignant. A total of 21 of 100 predictions were true positives.
# The cells falling on the other diagonal contain counts of examples where the kNN approach disagreed with the true label. The 2 examples in the lower-left FN cell are false negative results; in this case, the predicted value was benign but the tumor was actually malignant. Errors in this direction could be extremely costly, as they might lead a patient to believe that she is cancer-free, when in reality the disease
# may continue to spread. The cell labeled FP would contain the false positive results, if there were any. These values occur when the model classifies a mass as malignant when in reality it was benign.  Although such errors are less dangerous than a false negative result, they should also be avoided.
# A total of 2 percent, that is, 2 out of 100 masses were incorrectly classified by the kNN approach. 

# Step 8: Improving the model performance
# Can you improve this model further? I leave this as an exercise for you.
