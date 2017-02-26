# Case Study: Income prediction
# research question: Predict if the income exceeds >$50K/year based on census data. Also known as "Census Income" dataset.
# data source: http://archive.ics.uci.edu/ml/datasets/Adult

## clear screen
rm(list = ls())
# Import the data from a url
theUrl<-"http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult.data<- read.table(file = theUrl, header = FALSE, sep = ",", 
                        strip.white = TRUE, stringsAsFactors = TRUE)
adult<- adult.data # make a copy of the data 

str(adult.data)

# Exploratory Data Analysis
## a. Structure 
dim(adult.data)
str(adult.data) # Observations: add column headers; 
sum(is.na(adult.data)) # No missing values
colSums(is.na(adult.data))
head(adult.data)
# add column header
colnames(adult.data)<- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation",
               "relationship","race","sex","capital.gain","capital.loss","hoursperweek","native.country",
               "income")
str(adult.data)

# 
# collapse the factor levels and recode level with no name (coded as ?in original data) to missing

levels(adult.data$workclass)<- c("missing","federalgov","localgov","neverworked","private","selfempNotInc",
                                 "selfempInc","stategov","withoutpay")

levels(adult.data$education)<- list(presch=c("Preschool"), primary=c("1st-4th","5th-6th"),
                                    upperprim=c("7th-8th"), highsch=c("9th","Assoc-acdm","Assoc-voc","10th"),
                                    secndrysch=c("11th","12th"), graduate=c("Bachelors","Some-college"),
                                    master=c("Masters"), phd=c("Doctorate"))

levels(adult.data$marital.status)<- list(divorce=c("Divorced","Separated"), 
                                           married=c("Married-AF-spouse","Married-civ-spouse","Married-spouse-absent"),
                                           notmarried=c("Never-married"), widowed=c("Widowed"))

levels(adult.data$occupation) # missing level name coded as `?`
levels(adult.data$occupation)<- list(missing=c("?"), clerical=c("Adm-clerical"), 
                                     lowskillabr=c("Craft-repair","Handlers-cleaners","Machine-op-inspct",
                                                  "Other-service","Priv-house-serv","Prof-specialty",
                                                  "Protective-serv"),
                                     highskillabr=c("Sales","Tech-support","Transport-moving","Armed-Forces"),
                                     agricultr=c("Farming-fishing")
                                     )
table(adult.data$occupation)
table(adult.data$relationship)

levels(adult.data$relationship)<- list(husband=c("Husband"), wife=c("Wife"), outofamily=c("Not-in-family"),
                                       unmarried=c("Unmarried"), relative=c("Other-relative"), 
                                       ownchild=c("Own-child"))

levels(adult.data$race)
levels(adult.data$sex)

levels(adult.data$native.country)<- list(missing=c("?","South"),SEAsia=c("Vietnam","Laos","Cambodia","Thailand"),
                                           Asia=c("China","India","HongKong","Iran","Philippines","Taiwan"),
                                           NorthAmerica=c("Canada","Cuba","Dominican-Republic","Guatemala","Haiti",
                                                          "Honduras","Jamaica","Mexico","Nicaragua","Puerto-Rico",
                                                          "El-Salvador","United-States"),
                                           SouthAmerica=c("Ecuador","Peru","Columbia","Trinadad&Tobago"),
                                           Europe=c("France","Germany","Greece","Holand-Netherlands","Italy",
                                                    "Hungary","Ireland","Poland","Portugal","Scotland","England",
                                                    "Yugoslavia"),
                                           PacificIslands=c("Japan","France"),
                                           Oceania=c("Outlying-US(Guam-USVI-etc)")
                                           )

# check for missing values
colSums(is.na(adult.data)) # missing values in, education(11077) occupation(4066) and native.country(20)
str(adult.data)
# Missing data visualization
library(VIM)
aggr_plot <- aggr(adult.data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

## Missing data imputation
library(missForest)

str(adult.data) # generally it is advisable not to impute the categorical missing values, if they are less than they should be removed

imputdata<- missForest(adult.data) 
# check imputed values
imputdata$ximp
# assign imputed values to a data frame
adult.cmplt<- imputdata$ximp
aggr_plot <- aggr(adult.cmplt, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(adult.data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))

# Test of independence for the categorical variable
library(vcd) # for xtabs() and assocstats()
mytable<- xtabs(~native.country+workclass, data= adult.cmplt)
chisq.test(mytable) # native.country and workclass are related as p value is less than 0.005
#############################################################
library(ggplot2)

p = ggplot(adult.cmplt,aes(x=education, y=age, color=race))
p + geom_jitter(alpha=0.3) 