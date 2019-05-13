# Data analysis for Petrol additives
# Objective: Determine which additive is/are significant for petrol formulation?
# Script author: Ashish Dutt
# Script create date: 28/2/2019
# Email: ashishdutt@yahoo.com.my

# clean the workspace
rm(list = ls())

# required libraries
library(caret) # for nearZeroVar(), findCorrelation()
library(cluster) # for gower() and pam()
library(corrplot) # for correlation matrix
library(dplyr) # for mutate()
library(factoextra) # for fviz_cluster(), get_eigenvalue(), eclust()
library(FactoMineR) # for PCA() and MCA()
library(ggplot2) # for visualization
library(ggpubr) # for annotate_figure()
library(grid) # for grid.rect()
library(gridExtra) # for grid.arrange()

# Read the data
petrol.data<- read.csv("data/ingredient.csv", sep = ",", stringsAsFactors = FALSE)

# PART A: Basic EDA 
dim(petrol.data) # 214 observations in 9 columns
colnames(petrol.data)
sum(is.na(petrol.data)) # Zero missing values
str(petrol.data) # all numeric columns
# check for near zero variance in variables
badCols<- nearZeroVar(petrol.data) # no variable with near zero variance property

# check for correlation among continuous variables
cor.vals<- cor(petrol.data)
cor.vals.p<- cor(petrol.data, method = "pearson")
cor.vals.k<- cor(petrol.data, method = "kendall")
cor.vals.s<- cor(petrol.data, method = "spearman")

# Visualize high correlations
corrplot(cor.vals, method = "circle")
corrplot(cor.vals.p, method = "number")
corrplot(cor.vals.s, method = "number")
corrplot(cor.vals.k, method = "number")

# Correlation Treatment
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(cor.vals, cutoff=0.5)
# print variable names of highly correlated attributes
print(colnames(petrol.data[,highlyCorrelated]))
# remove the correlated variables
petrol.data.new<- petrol.data[,-highlyCorrelated]
dim(petrol.data.new) # 214 7

#### PART B - Distribution Study

# Since all variables are continuous in nature, so distribution visualization can be density plots, histograms, Frequency polygon, Area plots
colnames(petrol.data.new)
# density plots
plt <- ggplot(data = petrol.data.new, aes(x=b))
b<-plt + geom_density() +
  geom_vline(aes(xintercept = median(b)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=c))
c<-plt + geom_density() +
  geom_vline(aes(xintercept = median(c)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=d))
d<-plt + geom_density() +
  geom_vline(aes(xintercept = median(d)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=e))
e<-plt + geom_density() +
  geom_vline(aes(xintercept = median(e)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=f))
f<-plt + geom_density() +
  geom_vline(aes(xintercept = median(f)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=h))
h<-plt + geom_density() +
  geom_vline(aes(xintercept = median(h)),
             linetype = "dashed", size = 0.6)
plt <- ggplot(data = petrol.data.new, aes(x=i))
i<-plt + geom_density() +
  geom_vline(aes(xintercept = median(i)),
             linetype = "dashed", size = 0.6)
# Arrange the distribution plots into a grid matrix of 2 rows 4 cols
# add the above plots to a list
pList<- list(b,c,d,e,f,h,i)
fig2<-grid.arrange(grobs = pList, ncol = 2) ## display plot
annotate_figure(fig2
                ,top = text_grob("Density plots of continuous features", color = "black", face = "bold", size = 14)
                ,bottom = text_grob("Data source: Some survey data\n", color = "brown",
                                    hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))

# Histograms
plt <- ggplot(data = petrol.data.new, aes(x=b))
b<-plt + geom_area( stat = "bin", bins = 30,
                    color = "black", fill = "#00AFBB")
# Change y axis to count instead of density
plt+ geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(b)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07")
# Histogram
plt+geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(b)), 
             linetype = "dashed", size = 0.6)
# clear the graphic device
grid.newpage()

#### PART C - Unsupervised Feature Selection
# Conduct PCA for continuous variables
petrol.data.new.pca<-PCA(petrol.data.new, graph = FALSE)
# Scree plot to visualize the PCA's in continuous vars
screeplot<-fviz_screeplot(petrol.data.new.pca, addlabels = TRUE,
                          barfill = "#b4a8d1", barcolor = "black",
                          ylim = c(0, 50), xlab = "Clustering tendency for continuous features", ylab = "Percentage of explained variance",
                          main = "(A) Scree plot for additive features"
)
# Determine Variable contributions to the principal axes
# Contributions of variables to PC1
pc1<-fviz_contrib(petrol.data.new.pca, choice = "var", 
                  axes = 1, top = 10, sort.val = c("desc"),
                  fill = "#b4a8d1")+
  labs(title="(B) additive features")

# Contributions of variables to PC2
pc2<-fviz_contrib(petrol.data.new.pca, choice = "var", axes = 2, top = 10,
                  sort.val = c("desc"),
                  fill = "#b4a8d1")+
  labs(title="(C) additive feature")

fig3<- grid.arrange(arrangeGrob(screeplot), 
                    arrangeGrob(pc1,pc2, ncol=1), ncol=2, widths=c(2,1)) 
annotate_figure(fig3
                ,top = text_grob("Clustering tendency of petrol additives", color = "black", face = "bold", size = 14)
                ,bottom = text_grob("Some survey data", color = "brown",
                                    hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))

# Extract all the PC and store in a new data frame
names(petrol.data.new)
str(petrol.data.new)
petrol.data.impvars<-as.data.frame(
  petrol.data.new[,c(1:3,5:6)])
names(petrol.data.impvars)
# clear the graphic device
grid.newpage()

##### PART D: Clustering
# distance calculation
gower_dist <- daisy(petrol.data.impvars,
                    metric = "gower",
                    type = list(logratio = 3))
gower_mat <- as.matrix(gower_dist)
# Output most similar pair
petrol.data.impvars[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Output most dissimilar pair
petrol.data.impvars[
  which(gower_mat == min(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
###### Determine the clusters basis of distance calculated above
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
pam_results <- petrol.data.impvars %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
## Save the results 
sink("data/petrol_additive_clustering.txt")
print(pam_results$the_summary)
sink()  #  # returns output to the console
pam_results$the_summary

pam.res<-eclust(petrol.data.impvars, FUNcluster = c("pam"),k=3,graph = FALSE)
# Silhouette coeffecient Note: A score nearing to 1 is good
p<-fviz_silhouette(pam.res, palette = "jco", ggtheme = theme_classic()) # 0.59
annotate_figure(p,bottom = text_grob("Data source: \n Some survey data\n", color = "brown",
                                     hjust = 1, x = 1, face = "italic", size = 10)
)
# Add a black border around the 2x2 grid plot
grid.rect(width = 1.00, height = 0.99, 
          gp = gpar(lwd = 2, col = "black", fill=NA))
grid.newpage()

###### END OF SCRIPT