
data("USArrests")
df<- scale(USArrests)
head(df, n=3)

set.seed(21) # for reproducibility
km.res<- kmeans(df, 2,nstart = 2) # where 2 refers to required number of cluster
print(km.res)
# cluster means
km.res$centers

# compute the mean of each variable by clusters using the original data
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)

# save the model to disk
save(clus_res, "./final_model.rds")

# later...

# load the model
super_model <- readRDS("./final_model.rds")
print(super_model)
