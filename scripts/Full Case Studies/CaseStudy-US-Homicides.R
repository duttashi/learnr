# Case Study: Homicides
# Data Source: https://www.kaggle.com/murderaccountability/homicide-reports
# Reference url: http://www.murderdata.org/p/how-to.html
# Project start date: 14/3/2017
# Preliminary Objectives
## 1. EDA
## 2. Detecting Relationships

## clear screen
rm(list = ls())
# Load the data
homi.data<- read.csv("data/US_murders.csv", header = T, stringsAsFactors = T, sep = ",")

# Load the libraries
library(magrittr) # for the %>% 
library(dplyr) # for the filter()
library(ggplot2) # for visualization
# Explore the data
dim(homi.data)
str(homi.data)
sum(is.na(homi.data)) # 1 missing value
colSums(is.na(homi.data)) # Perpetrator.Age 
levels(homi.data$Agency.Type)
levels(homi.data$Weapon) # collapse the level for different type of guns and merge it with Firearm
table(homi.data$Weapon)
# collapse the factor levels for weapon
levels(homi.data$Weapon)<- list(Firearm=c("Firearm","Gun","Handgun","Rifle","Shotgun"),
                                BluntObj=c("Blunt Object"),Drown=c("Drowning"),Drugs=c("Drugs"),
                                Explosive=c("Explosives"),Fall=c("Fall"),Fire=c("Fire"),
                                SharpObj=c("Knife"),Stangled=c("Strangulation","Suffocation"),
                                Poison=c("Poison"), Unknown=c("Unknown"))

table(homi.data$Year,homi.data$Weapon)
# convert the colnames to lower
names(homi.data)<- tolower(names(homi.data))
names(homi.data)

# Outliers in victim and perpetrators age
table(homi.data$victim.age) #8,444 victims age is 0; 9281 victims age is 99; 974 victims age is 998. Clearly these are outliers
boxplot(homi.data$victim.age)
table(homi.data$perpetrator.age) #21,6,327 perpetrator age is 0; 90 perpetrators age is 99... clearly these are outliers
table(homi.data$victim.count) # There are 586059 instances of Zero victims.. If no victims then no crime!
table(homi.data$perpetrator.count) # There are 558838 instances of Zero perpetrators. If no perpetrator than no crime!

# subset the homi.data on age such that victims age & perpetrator age is between 1 to 98   
homi.data.clean<- homi.data %>% filter((victim.age>0 & victim.age<=65) & 
                                         (perpetrator.age>0 & perpetrator.age<=58) &
                                         (victim.count>0) & (perpetrator.count>0))

## Quick Plots
qplot(perpetrator.age, data = homi.data.clean, color=victim.sex, binwidth = 20)
qplot(victim.age, data = homi.data.clean, color=victim.sex)
qplot(x=victim.age,y=perpetrator.age, data = homi.data.clean, 
      color=victim.sex, geom = "boxplot") # distinct outliers in both victim & perpterator age. Also victim sex unknown can be labelled as NA
levels(homi.data.clean$victim.sex)<- list(Male=c("Male"),Female=c("Female"))
qplot(x=victim.age,y=victim.count, data = homi.data.clean, 
      color=victim.sex, geom = "boxplot")

# Underage criminals
homi.below18.crimnl<- homi.data.clean %>% filter(perpetrator.age>14 & perpetrator.age<18)


boxplot(homi.below18.crimnl$perpetrator.age) # The median age is 16
boxplot (perpetrator.age ~ victim.count, data = homi.below18.crimnl, 
         main = "Under age criminals",
         xlab = "Total count of victims", ylab = "Perpetrator's Age", col = "salmon")
# Density plots # FAR BETTER THAN HISTOGRAMS
ggplot(homi.below18.crimnl)+
  geom_density(aes(x=perpetrator.age, fill="red")) # majority of the perperators are in teens 16-17 years
ggplot(homi.below18.crimnl)+
  geom_density(aes(x=victim.age, fill="red")) # majority of victims between 20-40 age group
ggplot(homi.below18.crimnl)+
  geom_density(aes(x=perpetrator.count, fill="red")) # majority of the perpetarors are alone. They strike alone followed by pairs.


#	Line plots- visualizing relationship between two variables
ggplot(homi.below18.crimnl)+ geom_line(aes(x=victim.age, y=incident, color="red"))
## can't see any relationship
## Checking for High correlation
cor(homi.below18.crimnl$perpetrator.age, homi.below18.crimnl$victim.age) # low positive correlation
cor(homi.below18.crimnl$perpetrator.age, homi.below18.crimnl$victim.count)# low positive correlation
cor(homi.below18.crimnl$perpetrator.age, homi.below18.crimnl$incident) #low positive correlation
cor(homi.below18.crimnl$perpetrator.age, homi.below18.crimnl$perpetrator.count)#low positive correlation

# Bar plot for categorical data
qplot(perpetrator.age, data = homi.below18.crimnl, fill = perpetrator.sex) + 
  facet_grid (. ~ victim.race)

## reorder the columns in df.cmplt data frame
df.homi.below18<- homi.below18.crimnl[c(1,7,9,13,17,22:23,2:6,8,10:12,14:16,18:21,24)] # arrange all ints together and factors together
summary(df.homi.below18)

# collapse the factor levels
levels(df.homi.below18$relationship)<- list(Family_blood=c("Brother","Daughter","Father","Husband","Mother","Sister","Son","Wife"),
                                            Family_distant=c("Ex-Husband","Ex-Wife","In-Law","Stepdaughter","Stepfather","Stepmother","Stepson"),
                                            Family_casual=c("Boyfriend","Girlfriend"),
                                            Work=c("Employee","Employer"),
                                            Neighbor=c("Neighbor","Acquaintance"),
                                            Stranger=c("Unknown","Stranger")
                                            )
str(df.homi.below18)
  
table(df.homi.below18$relationship, df.homi.below18$crime.type)

# Clustering
library(klaR)
# Categorical data use kmodes()
cluster.results <-kmodes(df.homi.below18[,13:24], 3, iter.max = 10, weighted = FALSE )
