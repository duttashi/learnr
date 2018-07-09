# Case Study(CS) #1: Linear Regression
# Objective: Using the data for the top 1,000 websites on the internet for the year 2011, the objective is to use regression to predict the amount of page views
# Data used: top_1000_websites.tsv
# Packages used: 

# Read in the data
top.1000.sites<- read.csv("data/top_1000_websites.tsv", sep = "\t", stringsAsFactors = FALSE)

# Exploratory Data analysis
## A peak at the data dimensions and structure
dim(top.1000.sites) # 1000 rows in 9 cols
str(top.1000.sites)
names(top.1000.sites) # see the attribute names

## Since the objective is to predict the page views therefore, the attaribute PageViews will be our response/outcome variable
## Now, let's plot PageViews with UniqueVistors. Since both are numeric predictors/independent variable we will use a scatterplot
library(ggplot2)
ggplot(data = top.1000.sites, aes(x=PageViews, y=UniqueVisitors))+geom_point() # A terrible plot as all the values are bunched together on the x-axis and only a few jump out. This is a common problem when data is not normally distributed. because using a scale that’s large enough to show the full range of values tends to place the majority of data points so close to each other that they can’t be separated visually. To confirm that the shape of the data is the problem with this plot, we can look at the distribution of PageViews by itself;
ggplot(data = top.1000.sites, aes(x=PageViews))+geom_density() # This plot is no good either.

## HAT TIP: if plotting continuous data turns out to be visually incoherent, then its best to use log transformed values as shown next;
ggplot(data = top.1000.sites, aes(x=log(PageViews)))+geom_density() # Ahha!, now this plot makes more sense. For this reason, we’ll start using the log-transformed PageViews and UniqueVisitors from now on.
ggplot(data = top.1000.sites, aes(x=log(PageViews), y=log(UniqueVisitors)))+geom_point() # Nice, now the relationship between PageViews and UniqueVistors is evident.

# Now, lets use geom_smooth() with the method = 'lm' argument to see what the regression line will look like
ggplot(data = top.1000.sites, aes(x=log(PageViews), y=log(UniqueVisitors)))+geom_point()+
  geom_smooth(method = "lm", se=FALSE)
# This resulting line looks promising, so lets find the values that define its slope and intercept by calling lm;
lm.fit<- lm(log(PageViews)~ log(UniqueVisitors), data = top.1000.sites)
# Now that we’ve fit the line, we’d like to get a quick summary of it. We could look at
# the coefficients using coef, or we could look at the RMSE by using residuals. 
coef(lm.fit)
residuals(lm.fit)
# But we'll, introduce another function that produces a much more complex summary that we can
# walk through step by step. That function is called summary:
summary(lm.fit)
# The first thing the summary tells you the formula we have used. This is helpful for larger models
# The second step shows the residuals
lm.fit <- lm(log(PageViews) ~ HasAdvertising,
             data = top.1000.sites)
summary(lm.fit)$r.squared
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors),
             data = top.1000.sites)
summary(lm.fit)$r.squared
lm.fit <- lm(log(PageViews) ~ InEnglish,
             data = top.1000.sites)
summary(lm.fit)$r.squared
