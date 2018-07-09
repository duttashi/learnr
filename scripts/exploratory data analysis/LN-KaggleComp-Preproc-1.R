# Data information
# https://www.kaggle.com/fedagntmulder/serialhomicideoffenderdata
# clear the workspace
rm(list = ls())

# Load the data
df <- read.csv("C:/Users/Ashoo/Downloads/references/New folder/serialhomicideoffenderdata/Serial Killers Data.csv", stringsAsFactors=TRUE)

# EDA
dim(df)
str(df)
table(str(df))
names(df)

# Data Preprocessing
colSums(is.na(df))

## 1. Replace missing values to 99
df[is.na(df)]<- NA
colSums(is.na(df))
table(df$Race)
table(df$SexAbuse)
table(df$DadStable)
table(df$KillMethod)

## 2. Collapse the factor levels
str(df)

table(df$TypeofKiller)
df$TypeofKiller<-factor(df$TypeofKiller)
levels(df$TypeofKiller)<-list(doublemurder=c("Doublemurder+3rdattempt","Doublemurderer"),
            serial=c("Serial","Serial-accused","Serial-Accused","Serial-Orderedothers",
                      "Serial-Organizational-Accused","Serial-Organizational-Accused",
                      "Serial-Organizational-CriminalEnterprise","Serial-Organizational-Cult",
                      "Serial-Organizational-DrugEnterprise","Serial-Organizational-DrugEnterprise-Suspected",
                      "Serial-Organizational-Gang","Serial-Organizational-Government","Serial-Organizational-OrganizedCrime",
                      "Serial-Organizational-State","Serial-Perhapsmythical","Serial-Self-Proclaimed",
                      "Serial-selfproclaimed","Serial-Statusindoubt","Serial-suspected","Serial-Suspected",
                      "Serial-suspected-Acquittedofonemurder","Serial-suspected(5killed,7moreattempted)",
                      "Serial-Team","Serial-Team-Accomplice","Serial-Team-Accused","Serial-Team-ChargesDropped","Serial-Team-suspected","Serial-Team-Suspected","Serial-Team-Twoevents","Serial-Team-TwoEvents","Serial-Team-Twomurders","Serial-Team-TwoMurders","Serial-Team-Twomurders-suspected","Serial-Team-Twomurders+oneattempt","Serial-Team-Twomurders+otherattempts","Serial-Two-murders-suspected","Serial-Twoevents","Serial-TwoEvents","Serial-Twoevents-Accused","Serial-Twoevents-suspected","Serial-Twoevents-Suspected","Serial-Twoevents+attempts","Serial-Twoeventsplus2attempts","Serial-Twomurders","Serial-TwoMurders","Serial-Twomurders-Accomplice","Serial-Twomurders-accused","Serial-Twomurders-Accused","Serial-twomurders-selfproclaimed","Serial-Twomurders-selfproclaimed","Serial-twomurders-suspected","Serial-Twomurders-suspected","Serial-Twomurders-Suspected","Serial-Twomurders+2attempts","Serial-Twomurders+9attempts","Serial-Twomurders+multipleattempts","Serial-Twomurders+oneattempt","Serial-Twomurders+otherattempts","Serial-Twomurdersplusoneattempt"),
            soldpoison=c("Soldpoison"),
            spree=c("Spree","Spree-Accused","Spree-suspected","Spree-Team","Spree-Team-Twomurders","Spree-Twomurders"),
            others=c("Awaitingconfirmation"))

df$Race<-as.factor(df$Race) # convert to factor

library(ggplot2)
plt<- qplot(TypeofKiller, data = df, Race, alpha=I(.5))
plt
#scatterplot
plt1<- qplot(NumVics, AgeGroup, data = df, size=I(3))
plt1
