# clean the workspace
rm(list = ls())
# Data Source: https://www.kaggle.com/c/house-prices-advanced-regression-techniques
# reference: https://www.kaggle.com/bisaria/house-prices-advanced-regression-techniques/handling-missing-data

getwd()
df.data<- read.csv("data/houseprice.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
str(df.data)
sum(is.na(df.data))

# Visualizing Missing data
library(Amelia)

missmap(df.data[,1:80],
        main = "Missing values in Housing Prices Dataset",
        y.labels = NULL,
        y.at = NULL)

colSums(is.na(df.data))

#Following features have missing values:
#PoolQC: Pool quality
#MiscFeature: Miscellaneous feature not covered in other categories
#Alley: Type of alley access to property
#Fence: Fence qualityFireplaceQu: Fireplace quality
#FireplaceQu: Fireplace quality
#LotFrontage: Linear feet of street connected to property
#GarageCond: Garage condition
#GarageQual: Garage quality
#GarageFinish: Interior finish of the garage
#GarageYrBlt: Year garage was built
#GarageType: Garage location
#BsmtExposure: Refers to walkout or garden level walls
#BsmtCond: Evaluates the general condition of the basement
#BsmtQual: Evaluates the height of the basement
#BsmtFinType2: Rating of basement finished area (if multiple types)
#BsmtFinType1: Rating of basement finished area
#BsmtFinSF1: Type 1 finished square feet
#BsmtFinSF2: Type 2 finished square feet
#BsmtUnfSF: Unfinished square feet of basement area
#BsmtFullBath: Basement full bathrooms
#BsmtHalfBath: Basement half bathrooms
#MasVnrType: Masonry veneer type
#MasVnrArea: Masonry veneer area in square feet
#MSZoning: Identifies the general zoning classification of the sale
#Utilities: Type of utilities available
#Functional: Home functionality
#SaleType: Type of sale
#KitchenQual: Kitchen quality
#Electrical: Electrical system
#Exterior1st
#Exterior2nd

# Lets look at them one by one

#PoolQC: Pool quality
#Likely levels:
#Ex Excellent
#Gd Good
#TA Average/Typical
#Fa Fair
#NA No Pool


