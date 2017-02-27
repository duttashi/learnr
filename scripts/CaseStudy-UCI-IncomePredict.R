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

# Exploratory Data Analysis
## a. Structure 
dim(adult.data)
str(adult.data) # Observations: add column headers; 

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

# Get the data summary
summary(adult.cmplt) 
# avg age is 38 years, maximum workforce is in private class, graduates are maximum followed by secondaryschool; majority are married; 


table(adult.data$occupation)

# Data Visualization
library(ggplot2)

# Univariate data visualization. Density plots for continuous predictors and bar plots for categorical predictors
str(adult.cmplt)
ggplot(adult.cmplt)+
  geom_density(aes(x=age, fill="red"))

ggplot(adult.cmplt)+
  geom_density(aes(x=capital.gain, fill="red")) # majority data between 0 to <20000
ggplot(adult.cmplt)+
  geom_density(aes(x=capital.loss, fill="red")) # majority data between 0 to 20 and 1000 to 2000
ggplot(adult.cmplt)+
  geom_density(aes(x=hoursperweek, fill="red")) # majority data between 25 to 60
ggplot(adult.cmplt)+
  geom_density(aes(x=education.num , fill="red")) # majority data between 8 to 15

# subset the data based on observations from denisty plot
adult.cmplt.sub1<- subset(adult.cmplt, age<=75 & hoursperweek<=65)

# Boxplot for subset data and to check for outliers
library(magrittr) # for the pipe operator
library(dplyr) # for select() 
str(subst.data.1)

boxplot(adult.cmplt.sub1 %>% 
          # Note that the ‘%>%’ (pipe) passes data from the command before to the one after.
          select(hoursperweek)) # outliers between 0 to 35 and 55 and above

# subset the data again based on boxplot outlier above
adult.cmplt.sub1.1<- subset(adult.cmplt, hoursperweek==40 & age<=75)
boxplot(adult.cmplt.sub1.1 %>% 
          select(hoursperweek, age)) # Outliers removed for hoursperweek and age. This means that good data lies in age <=75 and hoursperweek=40

boxplot(adult.cmplt %>% 
          select(capital.gain))
adult.cmplt.sub1.2<- subset(adult.cmplt, capital.gain<=10)
boxplot(adult.cmplt.sub1.2 %>% 
          select(capital.gain))
boxplot(adult.cmplt %>% 
          select(capital.loss))
boxplot(adult.cmplt %>% 
          select(fnlwgt))
adult.cmplt.sub1.3<- subset(adult.cmplt, fnlwgt<=300000)
boxplot(adult.cmplt.sub1.3 %>% 
          select(fnlwgt)) # no outliers so fnlwght<=300000 is ok

boxplot(adult.cmplt %>% 
          select(education.num))
adult.cmplt.sub1.4<- subset(adult.cmplt, education.num>=5)
boxplot(adult.cmplt.sub1.4 %>% 
          select(education.num))# no outliers so education.num>=5 is ok

## Final Subset
adult.cmplt.subset<- subset(adult.cmplt,hoursperweek==40 & age<=75 & fnlwgt<=300000 & 
                             education.num>=5)


# Note: The predictor capital.gain and capital.loss has maximum 0 values. Dropping these predictors
adult.cmplt$capital.gain<-NULL
adult.cmplt$capital.loss<- NULL

# Now lets look at the density plots on this subset data
ggplot(adult.cmplt.sub1.1)+
  geom_density(aes(x=age , fill="red")) # looks better now. majority of the people are aged 20-65 years

# remove the subsets not required anymore
rm(adult.cmplt.sub1)
rm(adult.cmplt.sub1.2)
rm(adult.cmplt.sub1.4)
rm(adult.cmplt.sub1.3)
rm(adult.cmplt.sub1.1)

# Outlier treatment completed. For subsequent analysis, use the data frame `adult.cmplt.subset`
str(adult.cmplt.subset)

