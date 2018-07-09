# Check the data structure
str(sf.lib.data)
# check the data dimension
dim(sf.lib.data) # 4,23,448 rows and 15 columns
# check for missing values
colSums(is.na(sf.lib.data)) # none
# check data summary
summary(sf.lib.data)

# Exploratory data graphs to search for patterns
library(ggplot2)
plt1<-ggplot(sf.lib.data, aes(x=Total.Checkouts, y=Total.Renewals))+
  geom_point(size=2.5, color="navy blue")+
  xlab("Total checkouts")+ylab("Total renewals")+
  ggtitle("Checkouts vs Renewals")
plt1

plt2<-ggplot(sf.lib.data, aes(x=Age.Range, y=Total.Checkouts))+
  geom_point(size=2.5, color="navy blue")+
  xlab("Readership age range")+ylab("Total checkouts")+
  ggtitle("Which readers are most active")
plt2

library(gridExtra)
str(sf.lib.data)
plt3<-ggplot(sf.lib.data, aes(x=Circulation.Active.Month, fill=Age.Range))+geom_bar(stat = "count")+
  theme(axis.text.x = element_text(angle = 65, hjust = 1))+
  ggtitle("Most read month among readers")
plt3
