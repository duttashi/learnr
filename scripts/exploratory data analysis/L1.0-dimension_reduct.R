# Introduction
# PCA and Coresspondence analysis (CA) are both central to multivariate data analysis. 
# PCA and CA are usually applied in high dimensional datasets with the principal objective being to reduce the dimensions of the data. 
# It works by creating new uncorrelated variables Y_i (named as PCA) through linear combinations of original variables X_k (in general correlated).
# These PCA collect all information than original variables, and the goal is to select some PCA by preserving as much data variance as possible.
# Load the built in Fisher's iris dataset
#  is applied in categorical data (without calculate linear combinations) as a procedure to analyze contingency tables. CA allows us to describe the relation between two nominal variables as well as the relation between the levels of themselves in a Cartesian axis.
# The extension of correspondence analysis to many categorical variables is called multiple correspondence analysis.

data("iris")
str(iris)
summary(iris)
pairs(iris[1:4],main="Iris Data", pch=19, col=as.numeric(iris$Species)+1)
mtext("Type of iris species: red-> setosa; green-> versicolor; blue-> virginica", 1, line=3.7,cex=.8)

# I will use “prcomp” R function to carry out the analysis,
# In the prcomp function we need indicate if the principal components are calculated through correlation matrix (with standardized data) or covariance matrix (with raw data).
#  In our example all variables are measured in centimetres but we will use the correlation matrix for simplicity’s sake.
#To examine variability of all numeric variables
sapply(iris[1:4],var)
range(sapply(iris[1:4],var))

sapply(iris[1:4],sd) # the sd is different
# maybe this range of variability is big in this context.
#Thus, we will use the correlation matrix
#For this, we must standardize our variables with scale() function:
# Data Standardisation
iris.stand <- as.data.frame(scale(iris[,1:4]))
sapply(iris.stand,sd) #now, standard deviations are 1
# Now I will use the prcomp() function to calculate the principal components:

#If we use prcomp() function, we indicate 'scale=TRUE' to use correlation matrix
pca <- prcomp(iris.stand,scale=T)
#it is just the same that: prcomp(iris[,1:4],scale=T) and prcomp(iris.stand)
#similar with princomp(): princomp(iris.stand, cor=T)
pca
summary(pca)
#This gives us the standard deviation of each component, and the proportion of variance explained by each component.
#The standard deviation is stored in (see 'str(pca)'):
pca$sdev

#In order to decide how many principal components should be retained, it is common to summarise the results of a principal components analysis by making a scree plot, which we can do in R using the “screeplot()” function
#plot of variance of each PCA.
#It will be useful to decide how many principal components should be retained.
screeplot(pca, type="lines",col=3)

#From this plot and from the values of the ‘Cumulative Proportion of Variance’ (in summary(pca)) we can conclude that retaining 2 components would give us enough information, as we can see that the first two principal components account for over 95% of the variation in the original data.
#The loadings for the principal components are stored in:
pca$rotation # with princomp(): pca$loadings

# All weights on the second principal component are negative. Thus the PC2 might seem considered as an overall size measurement. When the iris has larger sepal and petal values than average, the PC2 will be smaller than average. This component explain the 23% of the variability.
#biplot of first two principal components
biplot(pca,cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)
