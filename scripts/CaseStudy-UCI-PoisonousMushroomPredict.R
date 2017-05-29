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
# The variables, `gill.attachment` has just 1 value, so dropping it
# The variables, `gill.size`,`stalk.shape`,`stalk.root`, `stalk.surface.above.ring`,`stalk.surface.below.ring`,` stalk.color.above.ring`,`veil.type`,` veil.color`,` ring.number`,` ring.type`,` spore.print.color`,` population` has a missing level

# Rename the levels
str(mushroom.data)
table(mushroom.data$class) # Before renaming the levels, e=4208 p=3916
levels(mushroom.data$class)<- c("edible","poisonous")
table(mushroom.data$class) # After renaming the levels, edible=4208 poisonous=3916
levels(mushroom.data$cap.shape)<-c("bell","conical","flat","knobbed","sunken","convex") 
table(mushroom.data$cap.surface)
levels(mushroom.data$cap.surface)<- c("fibrous","grooves","smooth","scaly")
table(mushroom.data$cap.color)
levels(mushroom.data$cap.color)<- c("buff","cinnamon","red","gray","brown","pink","green",
                                    "purple","white","yellow")
table(mushroom.data$bruises)
levels(mushroom.data$bruises)<- c("bruisesno","bruisesyes")
table(mushroom.data$odor)
levels(mushroom.data$odor)<-c("almond","creosote","foul","anise","musty","nosmell","pungent",
                              "spicy","fishy")
table(mushroom.data$gill.attachment)
levels(mushroom.data$gill.attachment)<- c("attached","free")
table(mushroom.data$gill.spacing)
levels(mushroom.data$gill.spacing)<- c("close","crowded")
table(mushroom.data$gill.size)
levels(mushroom.data$gill.size)<-c("broad","narrow")
table(mushroom.data$gill.color)
levels(mushroom.data$gill.color)<- c("buff","red","gray","chocolate","black","brown","orange",
                                     "pink","green","purple","white","yellow")
table(mushroom.data$stalk.shape)
levels(mushroom.data$stalk.shape)<- c("enlarging","tapering")
table(mushroom.data$stalk.root) # has a missing level coded as ?
levels(mushroom.data$stalk.root)<- c("missing","bulbous","club","equal","rooted")
table(mushroom.data$stalk.surface.above.ring)
levels(mushroom.data$stalk.surface.above.ring)<-c("fibrous","silky","smooth","scaly")
table(mushroom.data$stalk.surface.below.ring)
levels(mushroom.data$stalk.surface.below.ring)<-c("fibrous","silky","smooth","scaly")
table(mushroom.data$stalk.color.above.ring)
levels(mushroom.data$stalk.color.above.ring)<- c("buff","cinnamon","red","gray","brown",
                                                 "orange","pink","white","yellow")
table(mushroom.data$stalk.color.below.ring)
levels(mushroom.data$stalk.color.below.ring)<- c("buff","cinnamon","red","gray","brown",
                                                 "orange","pink","white","yellow")
table(mushroom.data$veil.type)
levels(mushroom.data$veil.type)<-c("partial")
table(mushroom.data$veil.color)
levels(mushroom.data$veil.color)<- c("brown","orange","white","yellow")
table(mushroom.data$ring.number)
levels(mushroom.data$ring.number)<-c("none","one","two")
table(mushroom.data$ring.type)
levels(mushroom.data$ring.type)<- c("evanescent","flaring","large","none","pendant")
table(mushroom.data$spore.print.color)
levels(mushroom.data$spore.print.color)<- c("buff","chocolate","black","brown","orange","green",
                                            "purple","white","yellow")
table(mushroom.data$population)
levels(mushroom.data$population)<- c("abundant","clustered","numerous","scattered","several","solitary")
table(mushroom.data$habitat)
levels(mushroom.data$habitat)<-c("woods","grasses","leaves","meadows","paths","urban","waste")

# Calculate number of levels for each variable
mushroom.data.levels<-cbind.data.frame(Variable=names(mushroom.data), Total_Levels=sapply(mushroom.data,function(x){as.numeric(length(levels(x)))}))
print(mushroom.data.levels)
levels(mushroom.data$gill.attachment)

# dropping variable with constant variance
mushroom.data$gill.attachment<- NULL

set.seed(56)
sum(is.na(mushroom.data)) # check for missing values. No missing values found

# Initial data visualization
library(ggplot2)
str(mushroom.data)


# check for high correlation
library(corrplot) # for cor() and corrplot()
library(caret) # for findCorrelation()
correl<- cor(mush.data[c(2:22)]) # skip the `class` variable
highCorel<- colnames(mush.data[findCorrelation(correl,cutoff = 0.7, verbose = TRUE)])
highCorel # [1] "cap.color"   "ring.number" "veil.type"  are highly correlated
corrplot(cor(mush.data[,-1]))


# Predictive data analytics
# A. split the dataset into training and testing sets
library(caTools) # for the sample.split()
set.seed(56) 
sample = sample.split(mush.data$class, SplitRatio = .7)
x_train = subset(mush.data, sample == TRUE)
x_test = subset(mush.data, sample == FALSE)

y_train<-x_train$class
y_test <- x_test$class

x_train$class<-NULL
x_test$class<-NULL
