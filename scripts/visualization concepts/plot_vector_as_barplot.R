# reference: https://stackoverflow.com/questions/20350361/plot-vector-as-barplot?rq=1

library(tidyverse)
library(reshape2)
a<-matrix(NA, ncol=3, nrow=100)
a[,1]<-1:100
a[,2]<-rnorm(100)
a[,3]<-rnorm(100)
x<-c(1,1,1,1,2,3,5,1,1,1,2,4,9)
y<-length(x)
a<-melt(as.data.frame(a),id.vars="V1")
ggplot(data.frame(x),aes(seq_along(x),x))+geom_bar(stat="identity")
