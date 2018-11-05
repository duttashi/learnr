# Data analysis for Good Health Survey Business Unit (SBU)


# clean the workspace
rm(list = ls())

# load the required libraries
library(plyr) # for mapvalues()
library(caret) # for nearZeroVar()
library(mice) # for missing data imputation
library(factoextra) # for fviz_cluster(), get_eigenvalue(), eclust()
library(FactoMineR) # for PCA() and MCA()
library(gridExtra) # for grid.arrange()
library(ggpubr) # for annotate_figure()
library(grid) # for grid.rect()

# load the data
# change the blanks to NA
surveydata<-read.csv("data/SBU_example_Surveydata_2014.csv", sep = ",",stringsAsFactors = TRUE,
                     na.strings=c("","NA"))
# Separate continuous and categorical variables
df.data<- surveydata
df.cat<- df.data[,sapply(df.data, is.factor)]
dim(df.cat) # 104 variables
df.cont<- df.data[,!sapply(df.data, is.factor)]
dim(df.cont) # 16 variables
sum(is.na(df.data))
sum(is.na(df.cat))
sum(is.na(df.cont))

#  recode categorical vars
df.cat$FMONTH<- mapvalues(df.cat$FMONTH, from = levels(df.cat$FMONTH), to=c(1:12))
df.cat$IMONTH<- mapvalues(df.cat$IMONTH, from = levels(df.cat$IMONTH), to=c(1:12))
df.cat$PVTRESD1<- mapvalues(df.cat$PVTRESD1, from = levels(df.cat$PVTRESD1), to=c(1,2,NA))
df.cat$CELLFON3<- mapvalues(df.cat$CELLFON3, from = levels(df.cat$CELLFON3), to=c(1,NA))
df.cat$GENHLTH<- mapvalues(df.cat$GENHLTH, from = levels(df.cat$GENHLTH), to=c(1:7))
df.cat$PHYSHLTH<- mapvalues(df.cat$PHYSHLTH, from = levels(df.cat$PHYSHLTH), to=c(1:4))
df.cat$MENTHLTH<- mapvalues(df.cat$MENTHLTH, from = levels(df.cat$MENTHLTH), to=c(1:4))
df.cat$POORHLTH<- mapvalues(df.cat$POORHLTH, from = levels(df.cat$POORHLTH), to=c(1:5))
df.cat$HLTHPLN1<- mapvalues(df.cat$HLTHPLN1, from = levels(df.cat$HLTHPLN1), to=c(1:4))
df.cat$EXERANY2<- mapvalues(df.cat$EXERANY2, from = levels(df.cat$EXERANY2), to=c(1:4))
df.cat$CVDCRHD4<- mapvalues(df.cat$CVDCRHD4, from = levels(df.cat$CVDCRHD4), to=c(1:4))
df.cat$ASTHMA3<- mapvalues(df.cat$ASTHMA3, from = levels(df.cat$ASTHMA3), to=c(1:4))
df.cat$ASTHNOW<- mapvalues(df.cat$ASTHNOW, from = levels(df.cat$ASTHNOW), to=c(1:5))
df.cat$CHCSCNCR<- mapvalues(df.cat$CHCSCNCR, from = levels(df.cat$CHCSCNCR), to=c(1:4))
df.cat$CHCOCNCR<- mapvalues(df.cat$CHCOCNCR, from = levels(df.cat$CHCOCNCR), to=c(1:4))
df.cat$CHCCOPD<- mapvalues(df.cat$CHCCOPD, from = levels(df.cat$CHCCOPD), to=c(1:4))
df.cat$HAVARTH3<- mapvalues(df.cat$HAVARTH3, from = levels(df.cat$HAVARTH3), to=c(1:4))
df.cat$ADDEPEV2<- mapvalues(df.cat$ADDEPEV2, from = levels(df.cat$ADDEPEV2), to=c(1:4))
df.cat$DIABETE3<- mapvalues(df.cat$DIABETE3, from = levels(df.cat$DIABETE3), to=c(1:6))
df.cat$AGE<- mapvalues(df.cat$AGE, from = levels(df.cat$AGE), to=c(1:8))
df.cat$MARITAL<- mapvalues(df.cat$MARITAL, from = levels(df.cat$MARITAL), to=c(1:8))
df.cat$CHILDREN<- mapvalues(df.cat$CHILDREN, from = levels(df.cat$CHILDREN), to=c(1:4))
df.cat$EDUCA<- mapvalues(df.cat$EDUCA, from = levels(df.cat$EDUCA), to=c(1:8))
df.cat$EMPLOY1<- mapvalues(df.cat$EMPLOY1, from = levels(df.cat$EMPLOY1), to=c(1:10))
df.cat$INCOME2<- mapvalues(df.cat$INCOME2, from = levels(df.cat$INCOME2), to=c(1:11))
df.cat$SEX<- mapvalues(df.cat$SEX, from = levels(df.cat$SEX), to=c(1:2))
df.cat$PREGNANT<- mapvalues(df.cat$PREGNANT, from = levels(df.cat$PREGNANT), to=c(1:5))
df.cat$QLACTLM2<- mapvalues(df.cat$QLACTLM2, from = levels(df.cat$QLACTLM2), to=c(1:5))
df.cat$USEEQUIP<- mapvalues(df.cat$USEEQUIP, from = levels(df.cat$USEEQUIP), to=c(1:5))
df.cat$DECIDE<- mapvalues(df.cat$DECIDE, from = levels(df.cat$DECIDE), to=c(1:5))
df.cat$DIFFWALK<- mapvalues(df.cat$DIFFWALK, from = levels(df.cat$DIFFWALK), to=c(1:5))
df.cat$DIFFALON<- mapvalues(df.cat$DIFFALON, from = levels(df.cat$DIFFALON), to=c(1:5))
df.cat$DRNKANY5<- mapvalues(df.cat$DRNKANY5, from = levels(df.cat$DRNKANY5), to=c(1:4))
df.cat$DRNKDRI2<- mapvalues(df.cat$DRNKDRI2, from = levels(df.cat$DRNKDRI2), to=c(1:5))
df.cat$DRNKDY4<- mapvalues(df.cat$DRNKDY4, from = levels(df.cat$DRNKDY4), to=c(1:3))
df.cat$DRNKMO4<- mapvalues(df.cat$DRNKMO4, from = levels(df.cat$DRNKMO4), to=c(1:3))
df.cat$DROCDY3<- mapvalues(df.cat$DROCDY3, from = levels(df.cat$DROCDY3), to=c(1:3))
df.cat$FLSHOT6<- mapvalues(df.cat$FLSHOT6, from = levels(df.cat$FLSHOT6), to=c(1:4))
df.cat$FLSHTMY2<- mapvalues(df.cat$FLSHTMY2, from = levels(df.cat$FLSHTMY2), to=c(1:6))
df.cat$FOBTFS<- mapvalues(df.cat$FOBTFS, from = levels(df.cat$FOBTFS), to=c(1:3))
df.cat$SEX<- mapvalues(df.cat$SEX, from = levels(df.cat$SEX), to=c(1:2))
df.cat$PREGNANT<- mapvalues(df.cat$PREGNANT, from = levels(df.cat$PREGNANT), to=c(1:5))
df.cat$QLACTLM2<- mapvalues(df.cat$QLACTLM2, from = levels(df.cat$QLACTLM2), to=c(1:5))
df.cat$USEEQUIP<- mapvalues(df.cat$USEEQUIP, from = levels(df.cat$USEEQUIP), to=c(1:5))
df.cat$DECIDE<- mapvalues(df.cat$DECIDE, from = levels(df.cat$DECIDE), to=c(1:5))
df.cat$BLDSTOOL<- mapvalues(df.cat$BLDSTOOL, from = levels(df.cat$BLDSTOOL), to=c(1:5))
df.cat$BLIND<- mapvalues(df.cat$BLIND, from = levels(df.cat$BLIND), to=c(1:5))
df.cat$BMI5<- mapvalues(df.cat$BMI5, from = levels(df.cat$BMI5), to=c(1:2))
df.cat$BMI5CAT<- mapvalues(df.cat$BMI5CAT, from = levels(df.cat$BMI5CAT), to=c(1:5))
df.cat$CDCPAID<- mapvalues(df.cat$CDCPAID, from = levels(df.cat$CDCPAID), to=c(1:5))
df.cat$CHLDCNT<- mapvalues(df.cat$CHLDCNT, from = levels(df.cat$CHLDCNT), to=c(1:7))
df.cat$CNCNOINS<- mapvalues(df.cat$CNCNOINS, from = levels(df.cat$CNCNOINS), to=c(1:4))
df.cat$HCVHEAR<- mapvalues(df.cat$HCVHEAR, from = levels(df.cat$HCVHEAR), to=c(1:5))
df.cat$HCVINPTE<- mapvalues(df.cat$HCVINPTE, from = levels(df.cat$HCVINPTE), to=c(1:4))
df.cat$HCVPRIME<- mapvalues(df.cat$HCVPRIME, from = levels(df.cat$HCVPRIME), to=c(1:5))
df.cat$HCVTEST<- mapvalues(df.cat$HCVTEST, from = levels(df.cat$HCVTEST), to=c(1:5))
df.cat$HEALTHCL1<- mapvalues(df.cat$HEALTHCL1, from = levels(df.cat$HEALTHCL1), to=c(1:5))
df.cat$HFOB3YR<- mapvalues(df.cat$HFOB3YR, from = levels(df.cat$HFOB3YR), to=c(1:3))
df.cat$DIFFDRES<- mapvalues(df.cat$DIFFDRES, from = levels(df.cat$DIFFDRES), to=c(1:5))
df.cat$USENOW3<- mapvalues(df.cat$USENOW3, from = levels(df.cat$USENOW3), to=c(1:6))
df.cat$ALCDAY5<- mapvalues(df.cat$ALCDAY5, from = levels(df.cat$ALCDAY5), to=c(1:6))
df.cat$RFSEAT2<- mapvalues(df.cat$RFSEAT2, from = levels(df.cat$RFSEAT2), to=c(1:3))
df.cat$RFSEAT3<- mapvalues(df.cat$RFSEAT3, from = levels(df.cat$RFSEAT3), to=c(1:3))
df.cat$SHINGLE2<- mapvalues(df.cat$SHINGLE2, from = levels(df.cat$SHINGLE2), to=c(1:5))
df.cat$SEATBELT<- mapvalues(df.cat$SEATBELT, from = levels(df.cat$SEATBELT), to=c(1:9))
df.cat$PDIABTST<- mapvalues(df.cat$PDIABTST, from = levels(df.cat$PDIABTST), to=c(1:5))
df.cat$PREDIAB1<- mapvalues(df.cat$PREDIAB1, from = levels(df.cat$PREDIAB1), to=c(1:6))
df.cat$ASBIRDUC<- mapvalues(df.cat$ASBIRDUC, from = levels(df.cat$ASBIRDUC), to=c(1:5))
df.cat$RFBMI5<- mapvalues(df.cat$RFBMI5, from = levels(df.cat$RFBMI5), to=c(1:3))
df.cat$SMOKER3<- mapvalues(df.cat$SMOKER3, from = levels(df.cat$SMOKER3), to=c(1:5))
df.cat$RFSMOK3<- mapvalues(df.cat$RFSMOK3, from = levels(df.cat$RFSMOK3), to=c(1:3))
df.cat$RFBING5<- mapvalues(df.cat$RFBING5, from = levels(df.cat$RFBING5), to=c(1:3))
df.cat$RFDRHV4<- mapvalues(df.cat$RFDRHV4, from = levels(df.cat$RFDRHV4), to=c(1:3))
df.cat$RFDRMN4<- mapvalues(df.cat$RFDRMN4, from = levels(df.cat$RFDRMN4), to=c(1:4))
df.cat$RFDRWM4<- mapvalues(df.cat$RFDRWM4, from = levels(df.cat$RFDRWM4), to=c(1:4))
df.cat$PNEUMO2<- mapvalues(df.cat$PNEUMO2, from = levels(df.cat$PNEUMO2), to=c(1:4))
df.cat$RFBLDS2<- mapvalues(df.cat$RFBLDS2, from = levels(df.cat$RFBLDS2), to=c(1:4))
df.cat$RFBLDS3<- mapvalues(df.cat$RFBLDS3, from = levels(df.cat$RFBLDS3), to=c(1:3))
df.cat$SSBSUGAR<- mapvalues(df.cat$SSBSUGAR, from = levels(df.cat$SSBSUGAR), to=c(1:7))
df.cat$SSBFRUT2<- mapvalues(df.cat$SSBFRUT2, from = levels(df.cat$SSBFRUT2), to=c(1:7))
df.cat$LIFECHG<- mapvalues(df.cat$LIFECHG, from = levels(df.cat$LIFECHG), to=c(1:5))
df.cat$PFCHLDFT<- mapvalues(df.cat$PFCHLDFT, from = levels(df.cat$PFCHLDFT), to=c(1:2))
df.cat$REGION<- mapvalues(df.cat$REGION, from = levels(df.cat$REGION), to=c(1:2))
df.cat$PNEUVAC3<- mapvalues(df.cat$PNEUVAC3, from = levels(df.cat$PNEUVAC3), to=c(1:5))
df.cat$LASTHCVT<- mapvalues(df.cat$LASTHCVT, from = levels(df.cat$LASTHCVT), to=c(1:4))
df.cat$BRTHCNTL3<- mapvalues(df.cat$BRTHCNTL3, from = levels(df.cat$BRTHCNTL3), to=c(1:5))
df.cat$TYPCNTRL2<- mapvalues(df.cat$TYPCNTRL2, from = levels(df.cat$TYPCNTRL2), to=c(1:17))
df.cat$NOBCUSE2<- mapvalues(df.cat$NOBCUSE2, from = levels(df.cat$NOBCUSE2), to=c(1:16))
df.cat$PRNTLVIT<- mapvalues(df.cat$PRNTLVIT, from = levels(df.cat$PRNTLVIT), to=c(1:6))

# Check for near zero variance again in both df.cat and df.cont data frames
badCols<- nearZeroVar(df.cat)
dim(df.cat[,badCols]) # there are 22 variables with near zero variance property 
colnames(df.cat[,badCols]) 
# drop the near zero variance columns and save to new data frame
df.cat<- df.cat[,-badCols]
dim(df.cat) # [1] 6865   82
badCols<- nearZeroVar(df.cont)
dim(df.cont[,badCols]) # there are No variables with near zero variance property in continuous vars
df.cont<- df.cont[,-badCols]
dim(df.cont) # [1] 6865   13

# check for missing values in df.cat and df.cont
sum(is.na(df.cat)) # 32325 values 
sum(is.na(df.cont)) # 9030 values
colSums(is.na(df.cat)) # LASTHCVT BRTHCNTL3 TYPCNTRL2 NOBCUSE2 PRNTLVIT
colSums(is.na(df.cont)) # STATERES   LADULT NUMADULT CTYCODE1

# Missing value treatment
dim(df.cat)
miceMod.cat<- mice(df.cat, m=2, maxit = 3, method = "cart", seed = 2018)
df.cat.cmplt<- complete(miceMod.cat, 2)
sum(is.na(df.cat.cmplt)) # [1] 0 missing values now

miceMod.cont<- mice(df.cont, m=2, maxit = 5, method = "cart", seed = 2018)
df.cont.cmplt<- complete(miceMod.cont, 2)
sum(is.na(df.cont.cmplt))

# JOIN THE DATAFRAMES with no missing values
dim(df.cat.cmplt) # [1] 6865   82
dim(df.cont.cmplt) # [1] 6865   13
## since both dataframes have equal number of rows, we can bind the columns together 
df.master<- cbind(df.cat.cmplt, df.cont.cmplt) # note: categorical vars are from 1:82 and continuous are from 83:92
dim(df.master) # [1] 6865   95
sum(is.na(df.master)) # 0

# Baseline model on partially clean data- the data where the missing values are imputed. And the continuous vars are scaled
smp_size <- floor(0.70 * nrow(df.master))
# set the seed to make your partition reproducible
set.seed(2018)
train_ind <- sample(seq_len(nrow(df.master)), size = smp_size)
df.train <- df.master[train_ind, ]
df.test <- df.master[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.train$X<- NULL
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Build models
# CART
set.seed(2018)
fit.cart <- train(DIABETE3~., data=df.train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(2018)
fit.knn <- train(DIABETE3~., data=df.train, method="knn", metric=metric, trControl=control)
# SVM
set.seed(2018)
fit.svm <- train(DIABETE3~., data=df.train, method="svmRadial", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm))
summary(results)
# compare accuracy of models
dotplot(results)

# Make Predictions using the best model 
predictions <- predict(fit.cart, df.test)
confusionMatrix(predictions, df.test$DIABETE3)  

# CLUSTERING TO DETERMINE RELEVANT VARIABLES
str(df.master)
names(df.master) # 1:82 are categorical vars, 83:95 are continuous vars
windowsFonts(Times=windowsFont("TT Times New Roman"))
extrafont::loadfonts(device="win")

# Conduct PCA for continuous variables
df.master.pca<-PCA(df.master[,c(83:95)], graph = FALSE)
# Scree plot to visualize the PCA's in continuous vars
screeplot<-fviz_screeplot(df.master.pca, addlabels = TRUE,
                          barfill = "#b4a8d1", barcolor = "black",
                          ylim = c(0, 50), xlab = "Clustering tendency for continuous features", ylab = "Percentage of explained variance",
                          main = "(A) Scree plot for continuous features"
)


# Determine Variable contributions to the principal axes
# Contributions of variables to PC1
pc1<-fviz_contrib(df.master.pca, choice = "var", 
                  axes = 1, top = 10, sort.val = c("desc"),
                  fill = "#b4a8d1")+
  labs(title="(B) continuous feature")

# Contributions of variables to PC2
pc2<-fviz_contrib(df.master.pca, choice = "var", axes = 2, top = 10,
                  sort.val = c("desc"),
                  fill = "#b4a8d1")+
  labs(title="(C) continuous feature")

fig1<- grid.arrange(arrangeGrob(screeplot), 
                    arrangeGrob(pc1,pc2, ncol=1), ncol=2, widths=c(2,1)) 
annotate_figure(fig1
                ,top = text_grob("Clustering tendency of continuous features", color = "black", face = "bold", size = 14)
                ,bottom = text_grob("Some survey data", color = "brown",
                                    hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))

# Extract all the PC and store in a new data frame
names(df.cont.cmplt)
df.cont.cmplt.impca<-as.data.frame(df.cont.cmplt[,c(1:2,4:5,7:9,11)])
# clear the graphic device
grid.newpage()

# STEP : MULTIPLE CORRESPONDENCE ANALYSIS (MCA) FOR CATEGORICAL DATA
dim(df.cat.cmplt) #[1] 6865   82 categorical variables
# Conduct the MCA test
res.mca<- MCA(df.cat.cmplt, ncp = 5, graph = TRUE)
# visualize the percentages of inertia explained by each MCA dimensions, use the function fviz_eig() or fviz_screeplot() [factoextra package]
screeplot<-fviz_screeplot(res.mca, addlabels = TRUE,
                          barfill = "#b4a8d1", barcolor = "black",
                          ylim = c(0, 50), xlab = "Clustering tendency of categorical features", ylab = "Percentage of explained variance",
                          main = "(A) Scree plot for categorical variables"
)
# Contributions of variables to PC1
pc1<-fviz_contrib(res.mca, choice = "var", 
                  axes = 1, top = 10, sort.val = c("desc"),
                  fill = "#b4a8d1")+
  rotate_x_text(angle = 80)+
  labs(title="(B) categorical feature")

# Contributions of variables to PC2
pc2<-fviz_contrib(res.mca, choice = "var", axes = 2, top = 10,
                  sort.val = c("desc"),
                  fill = "#b4a8d1")+
  labs(title="(C) categorical feature")
fig2<- grid.arrange(arrangeGrob(screeplot), 
                    arrangeGrob(pc1,pc2, ncol=1), ncol=2, widths=c(2,1)) 
annotate_figure(fig2
                ,top = text_grob("Clustering tendency of categorical features", color = "black", face = "bold", size = 14)
                ,bottom = text_grob("Data source: Some survey data\n", color = "brown",
                                    hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))

# Extract all the PC and store in a new data frame
names(df.cat.cmplt)
# so even though DIABETE3 is not found as an imp variable, we still add it to the dataframe as its a classification problem
df.cat.cmplt.impcs<-as.data.frame(df.cat[,c(17,28:34,50,48:49,37,62,40,
                                            30,18,27,15,59,25,22)]
)
# clear the graphic device
grid.newpage()

dim(df.cat.cmplt.impcs) # [1] 6865   21
dim(df.cont.cmplt.impca) # [1] 6865    8

# STEP : ADD THE PCA AND MCA VARIABLES TOGETHER TO A SINGLE DATAFRAME
df.master.final<- cbind(df.cat.cmplt.impcs,df.cont.cmplt.impca)
names(df.master.final) # 1:21 are categorical, 22:29 are continuous vars
# variable reduction
dim(surveydata) # [1] 6865  120
dim(df.master) # [1] 6865   95
dim(df.master.final)# [1] 6865   29
# We have reduced the original 120 variables to 29 variables.
write.csv(df.master.final, file = "data\\clean_survey_data_impvars.csv")


# Machine Learning models on the reduced dataset
smp_size <- floor(0.70 * nrow(df.master.final))
set.seed(2018)
train_ind <- sample(seq_len(nrow(df.master.final)), size = smp_size)
df.train <- df.master.final[train_ind, ]
df.test <- df.master.final[-train_ind, ]
# drop the variable X
df.train$X<- NULL
df.train$X<- NULL
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# Build models
# CART
set.seed(2018)
fit.cart <- train(DIABETE3~., data=df.train, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(2018)
fit.knn <- train(DIABETE3~., data=df.train, method="knn", metric=metric, trControl=control)
# SVM
set.seed(2018)
fit.svm <- train(DIABETE3~., data=df.train, method="svmRadial", metric=metric, trControl=control)

# summarize accuracy of models
results <- resamples(list(cart=fit.cart, knn=fit.knn, svm=fit.svm))
summary(results)
# compare accuracy of models
dotplot(results)

# Make Predictions using the best model
predictions <- predict(fit.cart, df.test)
confusionMatrix(predictions, df.test$DIABETE3)  

# END OF SCRIPT
