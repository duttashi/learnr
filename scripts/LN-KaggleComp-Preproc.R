# Data Source information
# Data Source: https://www.kaggle.com/aljarah/xAPI-Edu-Data
# clear the workspace
rm(list = ls())
# Load the data
stud.data <- read.table("C:/Users/Ashoo/Downloads/references/New folder/xAPI-Edu-Data.csv",
                        sep = ",", header = TRUE, stringsAsFactors = FALSE)
# Data Preprocessing

# create train.data frame
train.data<-stud.data
# Dummy coding for factor variables
str(train.data)
table(train.data$gender)

# create temporary variables to store the continuous variable values after converting them from categorical variables

train.data$Gender[train.data$gender %in% c("F")]<-1
train.data$Gender[train.data$gender %in% c("M")]<-2
str(train.data)

table(train.data$NationalITy)

train.data$National<-ifelse(train.data$NationalITy=="Egypt",1,
                            ifelse(train.data$NationalITy=="Iran",2,
                                   ifelse(train.data$NationalITy=="Iraq",3,
                                          ifelse(train.data$NationalITy=="Jordan",4,
                                                 ifelse(train.data$NationalITy=="Kw",5,
                                                        ifelse(train.data$NationalITy=="lebanon",6,
                                                               ifelse(train.data$NationalITy=="Lybia",7,
                                                                      ifelse(train.data$NationalITy=="Morocco",8,
                                                                             ifelse(train.data$NationalITy=="Palestine",9,
                                                                                    ifelse(train.data$NationalITy=="SaudiArabia",10,
                                                                                           ifelse(train.data$NationalITy=="Syria",11,
                                                                                                  ifelse(train.data$NationalITy=="Tunis",12,
                                                                                                         ifelse(train.data$NationalITy=="USA",13,14)
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
)
table(train.data$National)

table(train.data$PlaceofBirth)
train.data$BirthPlace<-ifelse(train.data$PlaceofBirth=="Egypt",1,
                            ifelse(train.data$PlaceofBirth=="Iran",2,
                                   ifelse(train.data$PlaceofBirth=="Iraq",3,
                                          ifelse(train.data$PlaceofBirth=="Jordan",4,
                                                 ifelse(train.data$PlaceofBirth=="KuwaIT",5,
                                                        ifelse(train.data$PlaceofBirth=="lebanon",6,
                                                               ifelse(train.data$PlaceofBirth=="Lybia",7,
                                                                      ifelse(train.data$PlaceofBirth=="Morocco",8,
                                                                             ifelse(train.data$PlaceofBirth=="Palestine",9,
                                                                                    ifelse(train.data$PlaceofBirth=="SaudiArabia",10,
                                                                                           ifelse(train.data$PlaceofBirth=="Syria",11,
                                                                                                  ifelse(train.data$PlaceofBirth=="Tunis",12,
                                                                                                         ifelse(train.data$PlaceofBirth=="USA",13,14)
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
                                          )
                                   )
                            )
)
table(train.data$BirthPlace)
str(train.data)
table(train.data$StageID)
train.data$SchLevel<-ifelse(train.data$StageID=="lowerlevel",0,
                           ifelse(train.data$StageID=="MiddleSchool",1,2
                                  )
)
# lowerlevel=0, MiddleSchool=1, HighSchool=2
table(train.data$SchLevel)
table(train.data$GradeID)
train.data$GradeLevel<-ifelse(train.data$GradeID=="G-02",2,
                            ifelse(train.data$GradeID=="G-04",4,
                                   ifelse(train.data$GradeID=="G-05",5,
                                          ifelse(train.data$GradeID=="G-06",6,
                                                 ifelse(train.data$GradeID=="G-07",7,
                                                        ifelse(train.data$GradeID=="G-08",8,
                                                               ifelse(train.data$GradeID=="G-09",9,
                                                                      ifelse(train.data$GradeID=="G-10",10,
                                                                             ifelse(train.data$GradeID=="G-11",11,12)
                                                                                                  )
                                                                                           )
                                                                                    )
                                                                             )
                                                                      )
                                                               )
                                                        )
                                                 )
table(train.data$GradeLevel)
table(train.data$SectionID)
train.data$Section<-ifelse(train.data$SectionID=="A",1,
                           ifelse(train.data$SectionID=="B",2,3)
                           )
table(train.data$Section)
table(train.data$Topic)

train.data$Subject<-ifelse(train.data$Topic=="Arabic",1,
                              ifelse(train.data$Topic=="Biology",2,
                                     ifelse(train.data$Topic=="Chemistry",3,
                                            ifelse(train.data$Topic=="English",4,
                                                   ifelse(train.data$Topic=="French",5,
                                                          ifelse(train.data$Topic=="Geology",6,
                                                                 ifelse(train.data$Topic=="History",7,
                                                                        ifelse(train.data$Topic=="IT",8,
                                                                               ifelse(train.data$Topic=="Math",9,
                                                                                      ifelse(train.data$Topic=="Quran",10,
                                                                                             ifelse(train.data$Topic=="Science",11,12
                                                                                                    )
                                                                                                    )
                                                                                             )
                                                                                      )
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              
table(train.data$Subject)
str(train.data)
table(train.data$Semester)
# Semester="F" means "First", "S" means "Second"
train.data$SchSemester<-ifelse(train.data$Semester=="F",1,2)
table(train.data$SchSemester)

table(train.data$Relation)
train.data$Parent<-ifelse(train.data$Relation=="Father",1,2)
table(train.data$Parent)

table(train.data$ParentAnsweringSurvey)
train.data$ParentSurveyd<-ifelse(train.data$ParentAnsweringSurvey=="Yes",1,0)
table(train.data$ParentSurveyd)

table(train.data$ParentschoolSatisfaction)
train.data$ParentSchSatisfy<-ifelse(train.data$ParentschoolSatisfaction=="Good",1,0)
table(train.data$ParentSchSatisfy)

table(train.data$StudentAbsenceDays)
train.data$StudAbsntDays<-ifelse(train.data$StudentAbsenceDays=="Under-7",0,1)
table(train.data$StudAbsntDays)

table(train.data$Class) # students are classified into three numerical intervals based on their total grade/mark
# Low-Level: interval includes values from 0 to 69,
# Middle-Level: interval includes values from 70 to 89,
# High-Level: interval includes values from 90-100.
train.data$TotMarksLevel<-ifelse(train.data$Class=="L",0,
                                 ifelse(train.data$Class=="M",1,2))
table(train.data$TotMarksLevel)

# Drop the character variables
str(train.data)
train.data$gender<-NULL
train.data$NationalITy<-NULL
train.data$PlaceofBirth<-NULL
train.data$StageID<-NULL
train.data$GradeID<-NULL
train.data$SectionID<-NULL
train.data$Topic<-NULL
train.data$Semester<-NULL
train.data$Relation<-NULL
train.data$ParentAnsweringSurvey<-NULL
train.data$ParentschoolSatisfaction<-NULL
train.data$StudentAbsenceDays<-NULL
train.data$Class<-NULL

# Visualization for highly correlated predictors
library(corrplot)
library(caret)
correl<-cor(train.data,use = "complete.obs", method = "kendall")
corrplot(correl, method="ellipse", type="lower",  sig.level = 0.01, insig = "blank")
# Detect and remove highly correlated predictors
highCorr<- findCorrelation(correl, cutoff = 0.80)
names(highCorr) # No variable is highly correlated
# check for skewness
library(moments)
skewness(train.data)
kurtosis(train.data)

# Check for outliers
## The best tool for outlier identification is the boxplot.  It visualizes the median and the spread of the data.
## https://www.r-bloggers.com/use-box-plots-to-assess-the-distribution-and-to-identify-the-outliers-in-your-dataset/
boxplot(train.data$raisedhands, main="Boxplot", ylab="Raised Hands")
