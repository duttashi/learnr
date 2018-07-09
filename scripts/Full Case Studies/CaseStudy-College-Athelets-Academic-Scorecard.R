# Case Study: 
# research question inspiration: Which sport or school has the highest academic score? Which schools' scores have increased or decreased significantly in the past decade? Are men's or women's team academic performance better? What about public and private colleges?
# data source: https://www.kaggle.com/ncaa/academic-scores

## clear screen
rm(list = ls())
atheleteData<- read.csv("data/kaggle_db-1.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(atheleteData)

# make a copy of the original data and perform all experiments on the copy
exp.data<- atheleteData

# Calculate the number of levels for each variable
totLevel<- cbind.data.frame(Var=names(exp.data), 
                            Total_Levels=sapply(exp.data,function(x){as.numeric(length(levels(x)))}
                            )
)
print(totLevel) # 38 sports

sum(is.na(exp.data)) # no missing values
colSums(is.na(exp.data))

# Data Visualizations
library(ggplot2)
qplot(NCAA_CONFERENCE, data = exp.data, fill = SPORT_NAME) + facet_grid (. ~ SCHOOL_TYPE) 
