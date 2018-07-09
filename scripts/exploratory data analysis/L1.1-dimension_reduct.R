# working code

attach(USArrests)
dim(USArrests)
str(USArrests)
usarr.pca<-prcomp(USArrests, scale=T)
summary(usarr.pca)
screeplot(usarr.pca, type = "lines", col=2)
sum(is.na(USArrests))
print(usarr.pca)
usarr.pca
plot(usarr.pca, type="l", main = "Scree plot for PCA")
usarr.pca$rotation
rload<-abs(usarr.pca$rotation)
rload
sweep(rload, 2, colSums(rload), "/")
