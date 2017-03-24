# source: https://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii/
# Business Q:  identify the customers segments, those are eligible for loan amount 
# Evaluation metric: Accuracy i.e. percentage of loan approval you correctly predict.
## clear screen
rm(list = ls())

# Import the data from a url
theUrl1<-"https://datahack-prod.s3.ap-south-1.amazonaws.com/train_file/train_u6lujuX_CVtuZ9i.csv"
dreamhouse.dat<- read.table(file = theUrl1, header = FALSE, sep = ",", 
                        strip.white = TRUE, stringsAsFactors = TRUE)
save.data<- dreamhouse.dat
dreamhouse.dat<-save.data
str(dreamhouse.dat)

# add column header
colnames(dreamhouse.dat)<- c("Loan_ID","Gender","Married","Dependents","Education","Self_Employed",
                             "ApplicantIncome","CoapplicantIncome","LoanAmount","Loan_Amount_Term",
                             "Credit_History","Property_Area","Loan_Status")
# recode the levels
dreamhouse.dat$Loan_ID<- as.numeric(dreamhouse.dat$Loan_ID)
table(dreamhouse.dat$Gender)
levels(dreamhouse.dat$Gender)<-list(Female=c("Female"),Male=c("Male"),mislvl=c("Gender",""))
table(dreamhouse.dat$Married)
levels(dreamhouse.dat$Married)<-list(Married=c("Married","Yes"),NotMarried=c("No"),mislvl=c(""))
table(dreamhouse.dat$Dependents)
levels(dreamhouse.dat$Dependents)<- list(NoDep=c("0"),OneDep=c("1"),TwoDep=c("2"),ThreenAbove=c("3+"),mislvl=c("","Dependents"))
table(dreamhouse.dat$Education)
levels(dreamhouse.dat$Education)<-list(Gradate=c("Graduate"), NotGraduate=c("Not Graduate"))
table(dreamhouse.dat$Self_Employed)
levels(dreamhouse.dat$Self_Employed)<- list(SelfEmplyd=c("Self_Employed","Yes"), NotSelfEmplyd=c("No"), mislvl=c(""))
dreamhouse.dat$ApplicantIncome<- as.numeric(dreamhouse.dat$ApplicantIncome)
summary(dreamhouse.dat$ApplicantIncome)
dreamhouse.dat$CoapplicantIncome<- as.numeric(dreamhouse.dat$CoapplicantIncome)
summary(dreamhouse.dat$CoapplicantIncome)
dreamhouse.dat$LoanAmount<- as.numeric(dreamhouse.dat$LoanAmount)
summary(dreamhouse.dat$LoanAmount)
levels(dreamhouse.dat$Loan_Amount_Term)
levels(dreamhouse.dat$Loan_Amount_Term)<-list(OneYr=c("12"),ThreeYr=c("36"),FiveYr=c("60"),SevenYr=c("84"),
                                              TenYr=c("120"),FifteenYr=c("180"),
                                              TwentyYr=c("240"),TwentyFiveYr=c("300"),ThirtyYr=c("360"),
                                              FortyYr=c("480"),mislvl=c("Loan_Amount_Term"))

levels(dreamhouse.dat$Loan_Amount_Term)<-list(Yr10=c("OneYr","ThreeYr","FiveYr","SevenYr","TenYr"),
                                              Yr20=c("FifteenYr","TwentyYr"), Yr30=c("TwentyFiveYr"),
                                              Yr40=c("ThirtyYr","FortyYr"), mislvl=c("mislvl"))
table(dreamhouse.dat$Loan_Amount_Term)

table(dreamhouse.dat$Credit_History) # credit history meets guidelines[Yes=1, No=0]
levels(dreamhouse.dat$Credit_History)<- list(mislvl=c(""),No=c("0"),Yes=c("1","Credit_History"))
table(dreamhouse.dat$Property_Area)
levels(dreamhouse.dat$Property_Area)<-list(Rural=c("Rural"), SemiUrban=c("Semiurban"), Urban=c("Urban","Property_Area"))
str(dreamhouse.dat)
table(dreamhouse.dat$Loan_Status)
levels(dreamhouse.dat$Loan_Status)<- list(Y=c("Y"),N=c("N","Loan_Status"))

# check for missing data
# Missing data visualization
library(VIM)
aggr_plot <- aggr(dreamhouse.dat, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(dreamhouse.dat), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## Missing data imputation
str(dreamhouse.dat)
library(missForest)
imputdata<- missForest(dreamhouse.dat) 
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
dream.cmplt<- imputdata$ximp
aggr_plot <- aggr(dream.cmplt, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(dream.cmplt), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


dreamhouse.clean<- dream.cmplt
## divide the data into train and test set
ratio = sample(1:nrow(dreamhouse.clean), size = 0.25*nrow(dreamhouse.clean))
test.data = dreamhouse.clean[ratio,] #Test dataset 25% of total
train.data = dreamhouse.clean[-ratio,] #Train dataset 75% of total
dim(train.data) # 462,13
dim(test.data) #153,13


library(randomForest)
library(caret) # for VarImp()
#fit the randomforest model
model.rf <- randomForest(Loan_Status~., 
                         data = train.data, 
                         importance=TRUE,
                         keep.forest=TRUE
)
print(model.rf)
#what are the important variables (via permutation)
varImpPlot(model.rf, type=1)
#predict the outcome of the testing data
predict<- predict(model.rf, test.data)
# Calculate prediction accuracy and error rates
actuals_preds <- data.frame(cbind(actuals=test.data$income, predicteds=predict)) # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy # 100%

# Logistic Regression as reposnse variable has two levels only
str(dreamhouse.clean)
glm.fit<- glm(Loan_Status~., data = train.data, family = "binomial") # binomial for logistic regression
summary(glm.fit) # The smallest p value is associated with notmarried and has a positive coeefecient indicating a an association between loan status and not married
glm.probs<- predict(glm.fit, type="response") # type=response gives the probabilities
glm.probs[1:10]
contrasts(train.data$Loan_Status) # gives the dummy coding done by R
model.anova<- anova(glm.fit, test = "Chisq")
model.anova 
# While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
library(pscl)
pR2(glm.fit)
# Lets see how the glm.fit model performs on the test.data
fit.result<- predict(glm.fit, newdata=test.data, type="response")
fit.result<- ifelse(fit.result > 0.5,1,0)
misClasificError<- mean(fit.result != test.data$Loan_Status)
misClasificError
print(paste('Accuracy',1-misClasificError))
library(ROCR)
p<- predict(glm.fit, newdata=test.data, type="response")
pr <- prediction(p, test.data$Loan_Status)
prf<- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc<- performance(pr, measure = "auc")
auc<- auc@y.values[[1]]
auc
