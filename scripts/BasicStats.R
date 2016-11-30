# Basic statistics                                                    #
# requires packages npmc, ggm, gmodels, vcd, Hmisc,                   #
#                   pastecs, psych, doBy to be installed              #
# install.packages(c("ggm", "gmodels", "vcd", "Hmisc",                #
#                    "pastecs", "psych", "doBy"))                     #

mt <- mtcars[c("mpg", "hp", "wt", "am")]
head(mt)

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

# frequency tables
library(vcd)
head(Arthritis)
# one way table
mytable <- with(Arthritis, table(Improved))
mytable  # frequencies
prop.table(mytable) # proportions
prop.table(mytable)*100 # percentages

# two way table
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

# Chi-square test of independence
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
chisq.test(mytable)
mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)

# Fisher's exact test
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
fisher.test(mytable)


# Chochran-Mantel-Haenszel test
mytable <- xtabs(~Treatment+Improved+Sex, data=Arthritis)
mantelhaen.test(mytable)

# Measures of association for a two-way table
library(vcd)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable)

# Covariances and correlations
states<- state.x77[,1:6]
cov(states)
cor(states)
cor(states, method="spearman")

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

