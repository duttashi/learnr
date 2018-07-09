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
# The variables, `gill.size`,`stalk.shape`,`stalk.root`, `stalk.surface.above.ring`,`stalk.surface.below.ring`,` stalk.color.above.ring`,`veil.type`,` veil.color`,` ring.number`,` ring.type`,` spore.print.color`,` population` has a missing level

# Rename the levels
str(mushroom.data)
levels(mushroom.data$class)<- c("edible","poisonous")
levels(mushroom.data$cap.shape)<-c("bell","conical","flat","knobbed","sunken","convex") 
levels(mushroom.data$cap.surface)<- c("fibrous","grooves","smooth","scaly")
levels(mushroom.data$cap.color)<- c("buff","cinnamon","red","gray","brown","pink","green",
                                    "purple","white","yellow")
levels(mushroom.data$bruises)<- c("bruisesno","bruisesyes")
levels(mushroom.data$odor)<-c("almond","creosote","foul","anise","musty","nosmell","pungent",
                              "spicy","fishy")
levels(mushroom.data$gill.attachment)<- c("attached","free")
levels(mushroom.data$gill.spacing)<- c("close","crowded")
levels(mushroom.data$gill.size)<-c("broad","narrow")
levels(mushroom.data$gill.color)<- c("buff","red","gray","chocolate","black","brown","orange",
                                     "pink","green","purple","white","yellow")
levels(mushroom.data$stalk.shape)<- c("enlarging","tapering")
levels(mushroom.data$stalk.root)<- c("missing","bulbous","club","equal","rooted")
levels(mushroom.data$stalk.surface.above.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushroom.data$stalk.surface.below.ring)<-c("fibrous","silky","smooth","scaly")
levels(mushroom.data$stalk.color.above.ring)<- c("buff","cinnamon","red","gray","brown",
                                                 "orange","pink","white","yellow")
levels(mushroom.data$stalk.color.below.ring)<- c("buff","cinnamon","red","gray","brown",
                                                 "orange","pink","white","yellow")
levels(mushroom.data$veil.type)<-c("partial")
levels(mushroom.data$veil.color)<- c("brown","orange","white","yellow")
levels(mushroom.data$ring.number)<-c("none","one","two")
levels(mushroom.data$ring.type)<- c("evanescent","flaring","large","none","pendant")
levels(mushroom.data$spore.print.color)<- c("buff","chocolate","black","brown","orange","green",
                                            "purple","white","yellow")
levels(mushroom.data$population)<- c("abundant","clustered","numerous","scattered","several","solitary")
levels(mushroom.data$habitat)<-c("woods","grasses","leaves","meadows","paths","urban","waste")

# Calculate number of levels for each variable
mushroom.data.levels<-cbind.data.frame(Variable=names(mushroom.data), Total_Levels=sapply(mushroom.data,function(x){as.numeric(length(levels(x)))}))
print(mushroom.data.levels)
levels(mushroom.data$veil.type)

# dropping veil type as it has only one level.
mushroom.data$veil.type<-NULL

sum(is.na(mushroom.data)) # check for missing values. No missing values found

# Initial data visualization
library(ggplot2)
str(mushroom.data)

## a. Univariate categorical data visualization
p<- ggplot(data = mushroom.data)

p+geom_bar(mapping = aes(x = cap.shape, fill=class), position = position_dodge())+
  theme(legend.position = "top")
table(mushroom.data$cap.shape, mushroom.data$class)

# b. Multivariate categorical data visualization

# Mosaic plot requires contingency table or a two way frequency table. Lets pick two predictors and create a two way table
library(vcd)

#mosaic(~ bruises+class, data = mushroom.data, legend=FALSE, gp=shading_Friendly,split_vertical=TRUE)

#mosaic(~ habitat+class, data = mushroom.data, shade=TRUE, legend=TRUE, direction="v",  main = "Relationship between mushroom habitat and class")
table(mushroom.data$habitat, mushroom.data$class)
mosaicplot(~ habitat+class, data = mushroom.data,cex.axis = 0.9, shade = TRUE, 
           main="Bivariate data visualization",
           sub = "Relationship between mushroom habitat and class",
           las=2, off=10,border="chocolate",xlab="habitat", ylab="class" )

table(mushroom.data$population, mushroom.data$class)
mosaicplot(~ population+class, data = mushroom.data, 
           cex.axis = 0.9, shade = TRUE, 
           main="Bivariate data visualization",
           sub = "Relationship between mushroom population and class",
           las=2, off=10,border="chocolate",xlab="population", ylab="class")

## Significance Test: Chisquared test

chisq.test(mushroom.data$cap.shape, mushroom.data$cap.surface, correct = FALSE)
chisq.test(mushroom.data$population, mushroom.data$habitat, correct = FALSE)
chi2<-chisq.test(mushroom.data$habitat, mushroom.data$odor, correct = FALSE)
chisq.test(mushroom.data$odor, mushroom.data$class, correct = FALSE)

## Strength of association
## As we know in this dataframe, there are 22 categorical variables with varied number of levels. So, first these categorical levels need to be converted to corresponding numeric values
## By looking at the levels of these variables, I will say they are unordered in nature. Such categorical variables are called "nominal". So, the measure of association test for categorical unordered variable is "Goodman Kruskal tau test", see Agresti

## See this vignette: https://cran.r-project.org/web/packages/GoodmanKruskal/vignettes/GoodmanKruskal.html
## See the package details at https://cran.r-project.org/web/packages/GoodmanKruskal/GoodmanKruskal.pdf

library(GoodmanKruskal)

varset1<- c("cap.shape","cap.surface","population","habitat","odor")
mushroomFrame1<- subset(mushroom.data, select = varset1)
GKmatrix1<- GKtauDataframe(mushroomFrame1)
plot(GKmatrix1)

varset2<- c("cap.shape","cap.surface","habitat","odor","class")
mushroomFrame2<- subset(mushroom.data, select = varset2)
GKmatrix2<- GKtauDataframe(mushroomFrame2)
plot(GKmatrix2, corrColors = "blue")
table(mushroom.data$class, mushroom.data$odor)

# Predictive data analytics

set.seed(56)
ratio = sample(1:nrow(mushroom.data), size = 0.25*nrow(mushroom.data))
test.data = mushroom.data[ratio,] #Test dataset 25% of total
train.data = mushroom.data[-ratio,] #Train dataset 75% of total

library(randomForest)
#Fit Random Forest Model
rf = randomForest(class ~ .,  ntree = 100,data = train.data)
plot(rf)
print(rf)

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")
