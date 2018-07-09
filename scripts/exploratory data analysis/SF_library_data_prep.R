# clear the working space
rm(list = ls())
# check working directory
getwd()
# Load the data
sf.lib.data<- read.csv("data/Library_Usage.csv", header = TRUE, sep = ",")
## make a copy of the original data frame. All experiments will be conducted for now on the copy
temp.data<-sf.lib.data
# check data structure
str(temp.data)
# replace the "-" with "miss"
temp.data<-as.data.frame(lapply(temp.data, gsub, pattern = "-", replacement = "miss", fixed = TRUE))
# Now check globally for missing values
colSums(is.na(temp.data)) # supervisor.district has 110310 missing values
# replace the blanks in predictor 'supervisor.district' with 0 to make it numeric
temp.data[is.na(temp.data)]<-0
## lets check the range for this predictor 
range(temp.data$Supervisor.District)

## check the str
str(temp.data)

# convert character datypes to factor
temp.data$Patron.Type.Code<-as.integer(temp.data$Patron.Type.Code)
temp.data$Supervisor.District<-as.integer(temp.data$Supervisor.District)
temp.data$Year.Patron.Registered<-as.integer(temp.data$Year.Patron.Registered)
temp.data$Total.Checkouts<-as.integer(temp.data$Total.Checkouts)
temp.data$Total.Renewals<-as.integer(temp.data$Total.Renewals)

temp.data$Patron.Type.Definition<- as.factor(temp.data$Patron.Type.Definition)
temp.data$Age.Range<-as.factor(temp.data$Age.Range)
temp.data$Home.Library.Code<- as.factor(temp.data$Home.Library.Code)
temp.data$Home.Library.Definition<- as.factor(temp.data$Home.Library.Definition)
temp.data$Circulation.Active.Month<- as.factor(temp.data$Circulation.Active.Month)
temp.data$Circulation.Active.Year<-as.factor(temp.data$Circulation.Active.Year)
temp.data$Notice.Preference.Code<- as.factor(temp.data$Notice.Preference.Code)
temp.data$Notice.Preference.Definition<-as.factor(temp.data$Notice.Preference.Definition)
temp.data$Provided.Email.Address<- as.factor(temp.data$Provided.Email.Address)
temp.data$Outside.of.County<- as.factor(temp.data$Outside.of.County)
str(temp.data)
# check predictor levels and collapse them wherever possible

levels(temp.data$Patron.Type.Definition)
# lets keep 4 levels for patron type definition namely; Adult (ADULT,AT USER ADULT,YOUNG ADULT), Child(=juvenile,AT USER JUVENILE), teen, senior, staff (RETIRED STAFF,STAFF,AT USER SENIOR) and Others (AT USER WELCOME,BOOKS BY MAIL,DIGITAL ACCESS CARD,FRIENDS FOR LIFE,SPECIAL,TEACHER CARD,VISITOR,WELCOME )
levels(temp.data$Patron.Type.Definition)<-list(Adult=c("ADULT","AT USER ADULT","YOUNG ADULT"),
                                               Child=c("AT USER JUVENILE","JUVENILE"),
                                               Teen=c("AT USER TEEN"),
                                               Senior=c("AT USER SENIOR","SENIOR"),
                                               Staff=c("RETIRED STAFF","STAFF","TEACHER CARD"),
                                               Others=c("AT USER WELCOME","BOOKS BY MAIL","DIGITAL ACCESS CARD","FRIENDS FOR LIFE","VISITOR","WELCOME","SPECIAL")
                                               )
levels(temp.data$Patron.Type.Definition)
table(temp.data$Patron.Type.Definition)

levels(temp.data$Age.Range)
# change age range to match patron type definition levels
# 0-9 years= child, 10-19 years= teen, 20-44 years=adult, >=45 years=senior
levels(temp.data$Age.Range)<- list(Child=c("0 to 9 years"),
                                   Teen=c("10 to 19 years"),
                                   Adult=c("20 to 24 years","25 to 34 years","35 to 44 years"),
                                   Senior=c("45 to 54 years","55 to 59 years","60 to 64 years","65 to 74 years","75 years and over"),
                                   Misng=c("")
                                   )
# check the structure again
str(temp.data)
# check again for missing values
colSums(is.na(temp.data)) # none found
# Replace sf.lib.data with temp.data
str(sf.lib.data)
sf.lib.data<-temp.data
# check the data summary now
summary(sf.lib.data)
## from the summary, we can see that home.library.code and home.library.definition matches
## also notice preference code and notice preference definition matches


# remove the temp.data 
rm(temp.data)



