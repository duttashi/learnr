# Exploratory data analysis using the xda package
## Package installation notes
library(devtools)
install_github("ujjwalkarn/xda")
## See https://github.com/ujjwalkarn/xda for package implementation usage
## load the package into the current session

library(xda)
## to view a comprehensive summary for all numeric columns in the iris dataset

numSummary(iris)

## n = total number of rows for that variable
## nunique = number of unique values
## nzeroes = number of zeroes
## iqr = interquartile range
## noutlier = number of outliers
## miss = number of rows with missing value
## miss% = percentage of total rows with missing values ((miss/n)*100)
## 5% = 5th percentile value of that variable (value below which 5 percent of the observations may be found)
## the percentile values are helpful in detecting outliers

## to view a comprehensive summary for all character columns in the warpbreaks dataset

charSummary(warpbreaks)

## n = total number of rows for that variable
## miss = number of rows with missing value
## miss% = percentage of total rows with missing values ((n/miss)*100)
## unique = number of unique levels of that variable
## top5levels:count = top 5 levels (unique values) in each column sorted by count
## for example, wool has 2 unique levels 'A' and 'B' each with count of 27 
## to perform bivariate analysis between 'Species' and 'Sepal.Length' in the iris dataset

bivariate(iris,'Species','Sepal.Length')

## bin_Sepal.Length = 'Sepal.Length' variable has been binned into 4 equal intervals (original range is [4.3,7.9])
## for each interval of 'Sepal.Length', the number of samples from each category of 'Species' is shown 
## i.e. 39 of the 50 samples of Setosa have Sepal.Length is in the range (4.3,5.2], and so on. 
## the number of intervals (4 in this case) can be customized (see documentation)

## to plot all other variables against the 'Petal.Length' variable in the iris dataset

Plot(iris,'Petal.Length')

## some interesting patterns can be seen in the plots below and these insights can be used for predictive modeling
Plot(iris,'Sepal.Length')
sessionInfo()
