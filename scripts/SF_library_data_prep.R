# clear the working space
rm(list = ls())
# check working directory
getwd()
# Load the data
sf.lib.data<- read.csv("data/Library_Usage.csv", header = TRUE, sep = ",")
## make a copy of the original data frame. All experiments will be conducted for now on the copy
temp.data<-sf.lib.data
# check data structure
str(sf.lib.data)
# convert character datypes to factor
temp.data$Patron.Type.Definition<- as.factor(temp.data$Patron.Type.Definition)
temp.data$Home.Library.Code<- as.factor(temp.data$Home.Library.Code)
temp.data$Home.Library.Definition<- as.factor(temp.data$Home.Library.Definition)
temp.data$Circulation.Active.Month<- as.factor(temp.data$Circulation.Active.Month)
temp.data$Circulation.Active.Year<-as.factor(temp.data$Circulation.Active.Year)
temp.data$Notice.Preference.Code<- as.factor(temp.data$Notice.Preference.Code)
temp.data$Notice.Preference.Definition<-as.factor(temp.data$Notice.Preference.Definition)
temp.data$Provided.Email.Address<- as.factor(temp.data$Provided.Email.Address)
temp.data$Outside.of.County<- as.factor(temp.data$Outside.of.County)

str(temp.data)
table(temp.data$Patron.Type.Definition)


#sf.lib.data$Patron.Type.Definition<- as.factor(sf.lib.data$Patron.Type.Definition)

table(sf.lib.data$Age.Range) # as you can see one of the column has missing header
names(sf.lib.data)
temp.df<- sf.lib.data
sf.lib.newdata<-lapply(temp.df, gsub, pattern = "", replacement = "missing", fixed = TRUE)
table(sf.lib.newdata$Age.Range)

sf.lib.data$Age.Range<- as.numeric(sf.lib.data$Age.Range)
sf.lib.newdata<-lapply(sf.lib.data, gsub, pattern = "<=9 years", replacement = "<=9 years", fixed = TRUE)
table(sf.lib.newdata$Age.Range)
