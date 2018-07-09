
# Naive Bayes classification

# load the library
library(e1071)
# load the dataset
data("Titanic")
titanic_df<- as.data.frame(Titanic)
#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(titanic_df)), titanic_df$Freq) #This will repeat each combination equal to the frequency of each combination
#Create the dataset by row repetition created
Titanic_dataset=titanic_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
Titanic_dataset$Freq=NULL

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Survived ~., data=Titanic_dataset)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,Titanic_dataset)
#Confusion matrix to check accuracy
table(NB_Predictions,Titanic_dataset$Survived)

