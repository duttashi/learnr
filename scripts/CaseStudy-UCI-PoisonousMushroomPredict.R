# Case Study: Predicting poisonous mushroom
# research question: Determine, which features are most indicative of a poisonous mushroom?
# data source: http://archive.ics.uci.edu/ml/datasets/Mushroom

## clear screen
rm(list = ls())

# acquire the data from the web resource
theURL<- "http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushroom.data<- read.csv(file = theURL, header = FALSE, sep = ",",strip.white = TRUE,
                         stringsAsFactors = TRUE, 
                         col.names = c("class","cap-shape","cap-surface","cap-color","bruises",
                                       "odor","gill-attachment","gill-spacing","gill-size",
                                       "gill-color","stalk-shape","stalk-root","stalk-surface-above-ring",
                                       "stalk-surface-below-ring","stalk-color-above-ring","stalk-color-below-ring",
                                       "veil-type","veil-color","ring-number","ring-type","spore-print-color",
                                       "population","habitat"))

# summarise the data
summary(mushroom.data) # stalk.root has 2480 missing values coded as ?, change ? to NA
# veil.type variable has just 1 value

