# kNN 

# reference: https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/


# load the data
prc<-read.csv("data/Prostate_Cancer.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
# set seed 
set.seed(51)
# EDA
# check first five rows of data
head(prc,5)
# check last five rows of data
tail(prc,5)
# check data structure
str(prc)
prc <- prc[-1]  #removes the first variable(id) from the data set.
table(prc$diagnosis_result)  # it helps us to get the numbers of patients
prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(prc$diagnosis)) * 100, digits = 1)  # it gives the result in the percentage form rounded of to 1 decimal place( and so it’s digits = 1)

# Data Preprocessing
## Normalize continuous data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}
prc_n <- as.data.frame(lapply(prc[2:9], normalize))
## The first variable in our data set (after removal of id) is ‘diagnosis_result’ which is not numeric in nature. So, we start from 2nd variable. The function lapply() applies normalize() to each feature in the data frame. The final result is stored to prc_n data frame using as.data.frame() function
summary(prc_n$radius)

# Model building
## Creating training and test data set
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]
## Note: Our target variable is ‘diagnosis_result’ which we have not included in our training and test data sets
prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]   #This code takes the diagnosis factor in column 1 of the prc data frame and on turn creates prc_train_labels and prc_test_labels data frame.

# Model training
install.packages("class")
library(class) # to use the knn(). if you dont have this library, then install it
prc_test_pred <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)
## Note: The value for k is generally chosen as the square root of the number of observations.

# Model performance evaluation
## We have built the model but we also need to check the accuracy of the predicted values in prc_test_pred as to whether they match up with the known values in prc_test_labels. To ensure this, we need to use the CrossTable() function available in the package ‘gmodels’.
library(gmodels)
CrossTable(x=prc_test_labels, y = prc_test_pred, prop.chisq = FALSE)
