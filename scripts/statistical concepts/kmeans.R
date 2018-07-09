
# K-means in R
# script create date: 19/Dec/2017

df<- c(2,4,12,14,18,22,28)

results<- kmeans(df,2)
results

results$cluster

table(df, results$cluster)
