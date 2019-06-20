# credit card default anomalies prediction
# Dataset source: https://www.kaggle.com/uciml/default-of-credit-card-clients-dataset
# Script author: Ashish Dutt
# Email: ashishdutt@yahoo.com.my
# Script create date: 20/6/2019
# Script last modified date: 


# clean the workspace
rm(list = ls())

# Required libraries
library(reshape2) # for melt()
library(caret) # for nearZeroVar(), findCorrelation()
library(corrplot) # for correlation matrix corrplot()
library(ggplot2)
library(plyr) # for mapvalues()
library(gridExtra) # for grid.arrange()
library(ggpubr) # for annotate_figure()
library(grid) # for grid.rect()


# Read the data
df <- read.csv("data/UCI_Credit_Card.csv", header = TRUE, stringsAsFactors = TRUE)

# summary statistics
summary(df)

#### EXPLORATORY DATA ANALYSIS

# the categorical vars are sex, education, marital status and Pay_0 to Pay_6
table(df$PAY_0)
# PAY_0 (perimissible values are 1 to 9). (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, ... 8=payment delay for eight months, 9=payment delay for nine months and above)
# Code any other values in variables PAY_0 to PAY_6 as 0
# Rename the dependent variable `default.payment.next.month` to `credit_default`

# Data Management 

# 1. rename variables
colnames(df)[colnames(df)=='default.payment.next.month']<- 'credit_default'
str(df)
table(df$credit_default) # Note: 1-yes, 0-no

# 2. Coerce to factor the categorical vars
# determine the column indexes that are categorical 
cols<- c(3:5,7:12,25) 
# convert numeric to categorical in one line
df[,cols] <- lapply(df[,cols],as.factor)

# 3. Collapse levels in variable PAY_0 to PAY_6 that are anot in data dictionary
table(df$PAY_0)
levels(df$PAY_0)<- list("1"=c("1"),"2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
table(df$PAY_2)
levels(df$PAY_2)<- list("1"=c("1"),"2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
table(df$PAY_3)
levels(df$PAY_3)<- list("1"=c("1"),"2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
table(df$PAY_4)
levels(df$PAY_4)<- list("1"=c("1"),"2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
table(df$PAY_5)
levels(df$PAY_5)<- list("2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
table(df$PAY_6)
levels(df$PAY_6)<- list("2"=c("2"),"3"=c("3"),
                        "4"=c("4"),"5"=c("5"),"6"=c("6"),
                        "7"=c("7"),"8"=c("8"),
                        "NA"=c("0","-1","-2")
                        )
str(df)

# Rearrange the columns such that categorical are first, followed by continuous
df<- df[,c(3:5,7:12,2,6,13:24,1,25)]
str(df)
names(df) # 1:9 are categorical rest are continuous

# Exploratory Data Analysis

# check for correlation among continuous variables
# There is no evidence of high correlation among the continuos variables
melt_corr<- melt(cor(df[,c(10:23)]))
ggplot(data = melt_corr, aes(x=Var1, y=Var2))+
  geom_tile()+
  scale_fill_gradient(low = "grey", high = "darkred")+
  labs(title="Correlation Matrix", x="Numeric variable's", 
       y="Numeric variable's", fill="Coefficient Range")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

## Alternative method for correlation detection
cor_vars<- df[,c(10:23)] # the index position of continuous variables
cor.vals<- cor(cor_vars, method = "pearson")
# Visualize high correlations
corrplot(cor.vals, method = "number") # Positive high correlation between var BILL_AMT1 to BILL_AMT6

# Check for near zero variance again in both df.cat and df.cont data frames
badCols<- nearZeroVar(df) # no near zero variance 
# check for missing values
sum(is.na(df)) # no missing values


# Distribution Plots
names(df)
# save df to a new dataframe for visuals purpose only
df.1<- df

# Rename the categorical var levels for plotting purpose 
df.1$credit_default<- mapvalues(df.1$credit_default, from = levels(df.1$credit_default), to=c("no","yes"))
df.1$SEX<- mapvalues(df.1$SEX, from = levels(df.1$SEX), to=c("male","female"))
df.1$EDUCATION<- mapvalues(df.1$EDUCATION, from = levels(df.1$EDUCATION), to=c("unknown","graduate","university","high school","others","unknown","unknown"))
df.1$MARRIAGE<- mapvalues(df.1$MARRIAGE, from = levels(df.1$MARRIAGE), to=c("unknown","married","single","others"))

# Boxplots amount of credit limit by education 
p <- ggplot(df.1, aes(x=EDUCATION,y=LIMIT_BAL,fill=credit_default)) 
plt1<-p + geom_boxplot()+
  coord_flip()+
  ggtitle("(A) Education vs Limit Balance")+
  labs(x="Education", 
       y="count", fill="credit_default")+
  theme_light()
plt1
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphics device
grid.newpage()

# Boxplots amount of credit limit by Marriage
p <- ggplot(df.1, aes(x=MARRIAGE,y=LIMIT_BAL,fill=credit_default)) 
plt2<-p + geom_boxplot()+
  coord_flip()+
  ggtitle("(B) Marriage vs Limit Balance")+
  labs(x="Marriage", 
       y="count", fill="credit_default")+
  theme_light()
plt2
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphics device
grid.newpage()

# Boxplots amount of credit limit by Person's Sex
p <- ggplot(df.1, aes(x=SEX,y=LIMIT_BAL,fill=credit_default)) 
plt3<-p + geom_boxplot()+
  #coord_flip()+
  ggtitle("(C) Sex vs Limit Balance")+
  labs(x="Marriage", 
       y="count", fill="credit_default")+
  theme_light()
plt3
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphics device
grid.newpage()  

# fig1<- grid.arrange(arrangeGrob(plt1), 
#                     arrangeGrob(plt3, ncol=1), ncol=2, widths=c(2,1)) 
# annotate_figure(plt1
#                 ,top = text_grob("The effect of Education on Credit Default", color = "black", face = "bold", size = 14)
#                 ,bottom = text_grob("Kaggle data", color = "brown",
#                                     hjust = 1, x = 1, face = "italic", size = 10)
# )
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
# clear the graphics device
grid.newpage()


## Feature importance
# Cautionary Note: boruta takes a long time to run. 
# boruta.df <- Boruta(credit_default~., data = df, doTrace = 2, maxRuns=11)
# 
# print(boruta.df) # all attributes are important
# plot(boruta.df, xlab = "", xaxt = "n")
# 
# lz<-lapply(1:ncol(boruta.df$ImpHistory),function(i)
#   boruta.df$ImpHistory[is.finite(boruta.df$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.df$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.df.train$ImpHistory), cex.axis = 0.7)
# # Add a black border around the 2x2 grid plot
# grid.rect(width = 1.00, height = 0.99, 
#           gp = gpar(lwd = 2, col = "black", fill=NA))
# # clear the graphic device
# grid.newpage()

# Data Modelling

# ## Save the id column in a separate variable
# df.id<- df$ID
# ### Drop the id columns
# df$ID<- NULL

## Data splitting for initial model building
set.seed(2019)
smp_size <- floor(0.75 * nrow(df))
# set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
df.train <- df[train_ind, ]
df.test <- df[-train_ind, ]
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# CART
fit.cart <- train(credit_default~., data=df.train, method="rpart", metric=metric, trControl=control)
# Logistic Regression
fit.logreg <- train(credit_default~., data=df.train, method="glm", metric=metric, trControl=control)

# kNN (note: takes some time to complete)
#fit.knn <- train(credit_default~., data=df.train, method="knn", metric=metric, trControl=control)
# SVM (note: takes some time to complete)
#fit.svm <- train(credit_default~., data=df.train, method="svmRadial", metric=metric, trControl=control)
# XG Boost
# fit.xgboost <- train(credit_default~., data=df.train, method="xgbDART", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(cart=fit.cart, logreg=fit.logreg))
summary(results)
# compare accuracy of models
dotplot(results) # best models are knn and cart

# Make Predictions using the best model 
predictions.1 <- predict(fit.logreg, df.test) # #accuracy of 82%
confusionMatrix(predictions.1, df.test$credit_default)

predictions.2 <- predict(fit.cart, df.test) #accuracy of 81%
confusionMatrix(predictions.2, df.test$credit_default)  

final_model<- fit.logreg

# Add the id column from the df.test dataframe that was saved earlier to the df.test dataframe
#df.test$id<- test.id

## Modeling on the supplied test data
model.final<- train(credit_default~., data= df.train, method="glm", metric=metric, trControl=control)
preds<- predict(model.final, newdata =  df.test, type = "raw")
# write the predicted values to file
pred_vals<- data.frame(cbind(df.test$ID, pred=preds))
write.csv(pred_vals, file="data/credit_default_predictions.csv", row.names=FALSE)
table(pred_vals$pred) # Note: 1 means YES, 2 means NO
