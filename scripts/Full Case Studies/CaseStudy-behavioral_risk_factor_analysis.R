# SCRIPT NAME: behavioral_risk_factor_analysis.R
# SCRIPT CREATE DATE: 02-Nov-2018
# SCRIPT LAST MODIFIED DATE: 02-Nov-2018

# OBJECTIVE: How to accurately predict DIABETE3 from the given data? 

# load the required libraries
library(ggplot2)
library(caret) # for nearZeroVar()
library(mice) # for missing data imputation
library(dplyr) # for data management tasks
library(magrittr) # for pipe operator
library(naniar) # for replace_with_na()

getwd()

# load the data
surveydata<-read.csv("data/SBU_example_Surveydata_2014.csv", sep = ",", 
                     stringsAsFactors = TRUE)

# Preliminary observations ####
# data dimension
dim(surveydata) # 6,865 observations in 120 variables
str(surveydata)
sum(is.na(surveydata)) # 18064 missing values
colSums(is.na(surveydata)) # max missing values in variables STATERES, LADULT, NUMADULT, CTYCODE1
# many factor variables with only 1 level
levels(surveydata$WEIGHT2)
levels(surveydata$WTKG3)
levels(surveydata$STATE) 
levels(surveydata$ZIPCODE)
table(surveydata$IYEAR) # so we have data for year 2014 and year 2015. There are 6837 observations for 2014 and 28 observations for 2015
sum(is.na(surveydata$CTYCODE1)) # complete empty. drop it

# Variables with value like "Missing". will be recoded as NA
# Variables with value like "Not asked or Missing",  will be recoded as NA
# Variables with value like "Don’t know/Refused/Missing", will be recoded as NA
# Variables with value like "Don’t know/Not Sure Or Refused/Missing", will be recoded as NA
# Variables with value like "Don’t know/Not sure" will be recoded as NA

levels(surveydata$CELLFON3) # value as "Missing"
levels(surveydata$RFDRWM4) # value as "Don’t know/Refused/Missing"
levels(surveydata$RFBLDS2) # value as Missing or Age less than 50
levels(surveydata$SSBSUGAR) # Not asked or Missing
levels(surveydata$DROCDY3) # Don’t know/Not Sure Or Refused/Missing

replace_vals<- list('Missing','Not asked or Missing','Don’t know/Refused/Missing',
                 'Don’t know/Not Sure Or Refused/Missing','Don’t know/Not sure',
                 'Not asked or Missing')

sum(is.na(surveydata)) # 18064 missing values
df.data<- surveydata %>%
  mutate_all(funs(type.convert(as.character(replace(., .== replace_vals, NA)))))

sum(is.na(df.data)) # 68291 missing values

?replace
levels(df.data$PVTRESD1)

# CHECK FOR NEAR ZERO VARIANCE variables
badCols<- nearZeroVar(df.data)
dim(df.data[,badCols]) # there are 27 variables with near zero variance property
colnames(df.data[,badCols])
# drop the near zero variance columns and save to new data frame
df.data.1<- df.data[,-badCols]

##### Separate continuous and categorical variables
df.cat<- df.data.1[,sapply(df.data.1, is.factor)]
dim(df.cat) # 80 variables
str(df.cat)
df.cont<- df.data.1[,!sapply(df.data.1, is.factor)]
dim(df.cont) # 13 variables
str(df.cont)

# # PART B: DATA VISUALIZATION ####
# 
# # STEP 3.0: CREATE CUSTOM THEME ACCORDING TO JOURNAL GUIDELINES
# # CREATING A MANUAL COLOR PALETTE
# mycolors = c(brewer.pal(name="Set2", n = 8), 
#              brewer.pal(name="Set1", n = 6))
# # CREATE A CUSTOM THEME
# mytheme<- function(base_size = 11, base_family = ""){
#   theme_bw(base_size = base_size, base_family = base_family) %+replace%
#     theme(plot.title = element_text(family="Arial", size = 11,
#                                     # where t=top margin, r=right margin, b=bottom margin, l=left margin
#                                     margin = margin(t = 00, r = 0, b = 10, l = 0)
#     ),
#     axis.text = element_text(family="Times", size = 10),
#     axis.title.x = element_text(family="Times", size = 10,
#                                 margin = margin(t = 10, r = 0, b = 0, l = 0)
#     ),
#     axis.title.y = element_text(family="Times", size = 11,
#                                 angle = 90,
#                                 margin = margin(t = 0, r = 10, b = 0, l = 0)
#     ),
#     legend.title = element_text(family="Times", size = 11),
#     legend.text = element_text(family="Times", size = 11),
#     panel.border=element_rect(fill=NA, size = 0.2),
#     legend.background = element_rect(fill="gray90", size=.5, 
#                                      linetype="dotted"))
# }
# 
# # load this library before plotting otherwise when changing the font type will give a warning message, "ggplot2 font family not found in windows font database"
# windowsFonts(Times=windowsFont("TT Times New Roman"))
# extrafont::loadfonts(device="win")
# 
# # missing value imputation
# tempData <- mice(df.data.1,m=5,maxit=10,meth="rf",seed=500)
# df1.complete<- complete(df2,1)
