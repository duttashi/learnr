# Data analysis for Palm Oil Yield
# Objective: A team of plantation planners are concerned about the yield of oil palm trees, which seems to
# fluctuate. They have collected a set of data and needed help in analysing on how external
# factors influence fresh fruit bunch (FFB) yield. 
# Script author: Ashish Dutt
# Script create date: 28/2/2019
# Email: ashishdutt@yahoo.com.my

# clean the workspace
rm(list = ls())


# required libraries
library(caret) # for nearZeroVar(), findCorrelation()
library(corrplot) # for correlation matrix, corrplot()
library(ggpubr) # for annotate_figure(), theme_set()
library(grid) # for grid.rect()
library(gridExtra) # for grid.arrange()

# Read the data
palmoil.data<- read.csv("data/palm_ffb.csv", sep = ",", stringsAsFactors = FALSE)

# PART A: Basic EDA 
dim(palmoil.data) # 130 observations in 9 columns
colnames(palmoil.data)
sum(is.na(palmoil.data)) # Zero missing values
str(palmoil.data) # all numeric columns except for Date
# check for near zero variance in variables
badCols<- nearZeroVar(palmoil.data) # no variable with near zero variance property

# check for correlation among continuous variables
df.data<- palmoil.data[,c(2:9)] # drop the date var
cor.vals<- cor(df.data)
cor.vals.p<- cor(df.data, method = "pearson")
cor.vals.k<- cor(df.data, method = "kendall")
cor.vals.s<- cor(df.data, method = "spearman")

# Visualize high correlations
corrplot(cor.vals, method = "circle")
corrplot(cor.vals.p, method = "number")
corrplot(cor.vals.s, method = "number")
corrplot(cor.vals.k, method = "number")

# Correlation Treatment
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(cor.vals.p, cutoff=0.75)
# remove the correlated variables
df.data.new<- df.data[,-highlyCorrelated]
colnames(df.data) # original data frame
colnames(df.data.new)
dim(df.data.new) # 130 7

#### PART B - Distribution Study
theme_set(theme_pubclean())
plt<- ggplot(data = df.data.new, aes(x=SoilMoisture))
a<-plt + geom_density() +
  geom_vline(aes(xintercept = median(SoilMoisture)),
             linetype = "dashed", size = 0.6)

plt<- ggplot(data = df.data.new, aes(x=Min_Temp))
b<-plt + geom_density() +
  geom_vline(aes(xintercept = median(Min_Temp)),
             linetype = "dashed", size = 0.6)

plt<- ggplot(data = df.data.new, aes(x=Max_Temp))
c<-plt + geom_density() +
  geom_vline(aes(xintercept = median(Max_Temp)),
             linetype = "dashed", size = 0.6)

plt<- ggplot(data = df.data.new, aes(x=Working_days))
d<-plt + geom_density() +
  geom_vline(aes(xintercept = median(Working_days)),
             linetype = "dashed", size = 0.6)

plt<- ggplot(data = df.data.new, aes(x=HA_Harvested))
e<-plt + geom_density() +
  geom_vline(aes(xintercept = median(HA_Harvested)),
             linetype = "dashed", size = 0.6)

plt<- ggplot(data = df.data.new, aes(x=FFB_Yield))
f<-plt + geom_density() +
  geom_vline(aes(xintercept = median(FFB_Yield)),
             linetype = "dashed", size = 0.6)

# Arrange the distribution plots into a grid matrix of 2 rows 4 cols
# add the above plots to a list
pList<- list(a,b,c,d,e,f)
fig1<-grid.arrange(grobs = pList, ncol = 2) ## display plot
annotate_figure(fig1
                ,top = text_grob("Density plots of Palm Oil Yield variables", color = "black", face = "bold", size = 14)
                ,bottom = text_grob("Data source: Some survey data\n", color = "brown",
                                    hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
grid.newpage()


#### PART C - (Supervised) Feature Selection or Variable Importance

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(FFB_Yield~., data=df.data.new, method="rf", 
               preProcess="scale", trControl=control,
               importance=T)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot results
plot(importance)
plot(importance, type=c("g", "o"))

# extract the important variables for further analysis
colnames(df.data.new)
df.impvars<- df.data.new[,c(6,4,1,7)]
colnames(df.impvars)

#### PART D - Model Building
df.master<- df.impvars
# Baseline model on partially clean data- the data where the missing values are imputed. And the continuous vars are scaled
smp_size <- floor(0.70 * nrow(df.master))
# set the seed to make your partition reproducible
set.seed(2019)
train_ind <- sample(seq_len(nrow(df.master)), size = smp_size)
df.train <- df.master[train_ind, ]
df.test <- df.master[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.train$X<- NULL
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Rsquared"

# Build models
# CART
colnames(df.master)
set.seed(2019)
# rf
fit.rf <- train(FFB_Yield~., data=df.train, method="rf", metric=metric, trControl=control,preProcess=c("center", "scale"))
# kNN
fit.knn <- train(FFB_Yield~., data=df.train, method="knn", metric=metric, trControl=control,preProcess=c("center", "scale"))
# SVM
fit.svm <- train(FFB_Yield~., data=df.train, method="svmRadial", metric=metric, trControl=control,preProcess=c("center", "scale"))

# summarize accuracy of models
results <- resamples(list(rf=fit.rf, knn=fit.knn, svm=fit.svm))
# Table comparison
summary(results)
# compare accuracy of models
# boxplot comparison
bwplot(results)
# dotplot comparison
dotplot(results)

# Make Predictions using the best model 
predictions <- predict(fit.rf, df.test, na.action = na.pass)
#confusionMatrix(predictions, df.test$FFB_Yield)
table(predictions, df.test$FFB_Yield)

###### END OF SCRIPT