
# K-means in R

df<- c(2,4,12,14,18,22,28)

results<- kmeans(df,2)
results

results$cluster

table(df, results$cluster)
