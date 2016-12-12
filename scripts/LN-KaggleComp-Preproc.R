# Data Source information
# The data is derived from https://www.kaggle.com/aljarah/xAPI-Edu-Data
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
