# Basic statistics                                                    #
# requires packages npmc, ggm, gmodels, vcd, Hmisc,                   #
#                   pastecs, psych, doBy to be installed              #
# install.packages(c("ggm", "gmodels", "vcd", "Hmisc",                #
#                    "pastecs", "psych", "doBy"))                     #
# Examples given in this script are from the book "R in Action",Chapter 7

mt <- mtcars[c("mpg", "hp", "wt", "am")]
head(mt)

# DESCRIPTIVE STATISTICS FOR CONTINUOUS VARIABLES
# Descriptive stats via summary
summary(mt)
# Descriptive stats via describe (Hmisc)
library(Hmisc)
myvars <- c("mpg", "hp", "wt")
describe(mtcars[myvars])
# Descriptive stats via stat.desc (pastecs)
library(pastecs)
stat.desc(mtcars[myvars])
#Descriptive stats via describe (psych)
library(psych)
describe(mtcars[myvars])
#Descriptive stats by group with aggregate
aggregate(mtcars[myvars], by=list(am=mtcars$am), mean)
aggregate(mtcars[myvars], by=list(am=mtcars$am), sd)

# Descriptive stats by group via describe.by (psych)
library(psych)
myvars <- c("mpg", "hp", "wt")
describeBy(mtcars[myvars], list(am=mtcars$am))

# DESCRIPTIVE STATISTICS FOR CATEGORICAL VARIABLES
# frequency tables
library(vcd)
head(Arthritis)

# one way table can be generated using the table() function
mytable <- with(Arthritis, table(Improved))
mytable  # frequencies
prop.table(mytable) # proportions
prop.table(mytable)*100 # percentages

# two way table where the xtabs() allows you to create a contigency table using formula style input
mytable <- xtabs(~ Treatment+Improved, data=Arthritis)
mytable # frequencies
margin.table(mytable,1) #row sums
margin.table(mytable, 2) # column sums
prop.table(mytable) # cell proportions
prop.table(mytable, 1) # row proportions
prop.table(mytable, 2) # column proportions
addmargins(mytable) # add row and column sums to table

# Two way table using CrossTable
library(gmodels)
CrossTable(Arthritis$Treatment, Arthritis$Improved)

# Tests of independence
# R provides several methods for testing the independence of categorical variables
# Chi-square test of independence
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable) # there appears to be a relationship between treatment received and level of improvement (p<0.1)
# because the propbability (p-value is small 0.01, we can reject the hypothesis that 
#treatment type and outcome are independent)

mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable) # there does'nt appear to be a relationship between patient sex and improvement as p>0.5
# because the propbability (p-value is greater than 0.05, 
#we can accept the hypothesis that patient sex and improvement are independent)

# Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)

# Chochran-Mantel-Haenszel test
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)
# the results suggest that treatment received and improvement reported are not independent
# within each level of sex 

# Measures of association for a two-way table
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

# Covariances and correlations
# correlation coeffecients are used to describe relationships among quantitative variables.
# The (+-) sign indicate the direction of the relationship (positive or inverse) and the
# magnitude indicates the strength of the relationship (ranging from 0 for no relationship
# to 1 for a perfect relationship)

# Load the state.x77 dataset. Use help(state.x77) to learn more about the dataset
states<- state.x77[,1:6]
cov(states)
cor(states) 
cor(states, method="spearman") # we can see a strong positive correlation exist between
# income and high school graduation rates and a strong negative correlation exist between
# illiteracy rates and life expectancy

x <- states[,c("Population", "Income", "Illiteracy", "HS Grad")]
y <- states[,c("Life Exp", "Murder")]
cor(x,y)

# partial correlations
library(ggm)
# partial correlation of population and murder rate, controlling
# for income, illiteracy rate, and HS graduation rate
pcor(c(1,5,2,3,6), cov(states))
# Testing a correlation coefficient for significance
cor.test(states[,3], states[,5])

# Testing correlations for significance
# Correlation matrix and tests of significance via corr.test
library(psych)
corr.test(states, use="complete")

# t test
library(MASS)
t.test(Prob ~ So, data=UScrime)


# dependent t test
sapply(UScrime[c("U1","U2")], function(x)(c(mean=mean(x),sd=sd(x))))
with(UScrime, t.test(U1, U2, paired=TRUE))


# Wilcoxon two group comparison
with(UScrime, by(Prob, So, median))
wilcox.test(Prob ~ So, data=UScrime)

sapply(UScrime[c("U1", "U2")], median)
with(UScrime, wilcox.test(U1, U2, paired=TRUE))


# Kruskal Wallis test
states <- data.frame(state.region, state.x77)
kruskal.test(Illiteracy ~ state.region, data=states)

