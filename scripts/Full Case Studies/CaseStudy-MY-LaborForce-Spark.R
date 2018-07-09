# Motivation: Apply sparklyr to `predicting employment related factors in malaysia case study`
# check this post: https://shiring.github.io/machine_learning/2017/02/19/food_spark
# Script name: employment_spark.R

# clear the workspace
rm(list=ls())

# Data Source Info: Labor Statistics Data from 1982-2014
# Labour Force and Social Statistic http://www.dosm.gov.my/v1/index.php?r=column3/accordion&menu_id=aHhRYUpWS3B4VXlYaVBOeUF0WFpWUT09
# Load the required libraries
library(ggplot2)
library(plyr) # for the rename ()

#library(tidyr) # for the gather()
library(missForest) # for missForest()


# Load the data
df1<- read.csv("data/misc/bptms-Employed_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df2<- read.csv("data/misc/bptms-Labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df3<- read.csv("data/misc/bptms-Labour_Force_Participation_rate_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df4<- read.csv("data/misc/bptms-Outside_labour_force_by_State.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
df5<- read.csv("data/misc/bptms-Unemployment_Rate.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

sum(is.na(df3)) # 29 missing values
colSums(is.na(df3)) # check which col has missing values
sum(is.na(df5)) # 29 missing values
colSums(is.na(df5))

# Basic Data Management 

## Renaming the column name. I'm following the CamelCase attribute naming convention
names(df1)
df1<- rename(df1, c("State.Country" = "State"))
df1<- rename(df1, c("Employed...000." = "Employed"))
names(df2)
df2<- rename(df2, c("State.Country" = "State"))
df2<- rename(df2, c("Labour.Force...000." = "LabrFrc"))
names(df3)
df3<- rename(df3, c("State.Country" = "State"))
df3<- rename(df3, c("Labour.Force.Participation.Rate..Percentage." = "LabrFrcPerct"))
names(df4)
df4<- rename(df4, c("State.Country" = "State"))
df4<- rename(df4, c("Outside.Labour.Force...000." = "OutLabrFrc"))
names(df5)
df5<- rename(df5, c("State.Country" = "State"))
df5<- rename(df5, c("Unemployment.Rate..Percentage." = "UnempRatePerct"))

## Change data type
df1$State<- as.factor(df1$State)
df1$Employed<- as.numeric(gsub(",","", df1$Employed))
df2$State<- as.factor(df2$State)
df2$LabrFrc<- as.numeric(gsub(",","", df2$LabrFrc))
df3$State<- as.factor(df3$State)
df4$State<- as.factor(df4$State)
df4$OutLabrFrc<- as.numeric(gsub(",","", df4$OutLabrFrc))
df5$State<- as.factor(df5$State)

## Joining the data frames
## using the dplyr library
library(dplyr)
system.time(join1<- inner_join(df1,df2))
system.time(join2<- inner_join(df3,df4))
system.time(join3<- inner_join(join1,join2))
system.time(df.master<- inner_join(join3,df5))

## MISSING DATA IMPUTATION 
imputdata<- missForest(df.master)
# assign imputed values to a data frame
df.cmplt<- imputdata$ximp
## FACTOR TO NUMERIC CONVERSION ##
df.cmplt$State<-as.factor(gsub("W.P.Putrajaya","Putrajaya", df.cmplt$State,ignore.case=T))
df.cmplt$State<-as.factor(gsub("W.P. Kuala Lumpur","Kuala Lumpur", df.cmplt$State,ignore.case=T))
df.cmplt$State<-as.factor(gsub("W.P Labuan","Labuan", df.cmplt$State,ignore.case=T))
df.cmplt$State<- as.numeric(df.cmplt$State)

# check for missing values in the new data frame
colSums(is.na(df.cmplt))

# create subset
subst.data.2<- subset(df.cmplt, 
                      (LabrFrcPerct>=60 & LabrFrcPerct <=70) & 
                        (UnempRatePerct>=2.5 & UnempRatePerct<=5.0)
)

## CUSTOM THEME
library(extrafont)
library(gridExtra)

my_theme <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "lightgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "black"),
      legend.position = "bottom",
      legend.justification = "top", 
      legend.box = "horizontal",
      legend.box.background = element_rect(colour = "grey50"),
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}

## VISUALLY DETECTING OUTLIER using BOXPLOTS

# When reviewing a boxplot, an outlier is defined as a data point that is located outside the fences (“whiskers”) of the boxplot (e.g: outside 1.5 times the interquartile range above the upper quartile and bellow the lower quartile).
# remember, ggplot2 requires both an x and y variable of a boxplot. Here is how to make a single boxplot

p1<-ggplot(data= subst.data.2, aes(x="", y=Employed))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p2<-ggplot(data= subst.data.2, aes(x="", y=LabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")
p3<-ggplot(data= subst.data.2, aes(x="", y=OutLabrFrc))+
  geom_boxplot(outlier.size=2,outlier.colour="red")

p1+ ggtitle("Employed in Malaysia (1982-2014)")+
  xlab("")+ylab("Employed")
p2+ ggtitle("Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Labour Force")
p3+ ggtitle("Outside Labour Force in Malaysia (1982-2014)")+
  xlab("")+ylab("Outside Labour Force")

# reset the par for graphics
par(mfrow=c(1,1))