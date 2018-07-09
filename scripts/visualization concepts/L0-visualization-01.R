# Data Visualizations
# data source: http://data.worldbank.org/indicator/GC.XPN.TOTL.GD.ZS?view=chart
# data name: Expenses (% of GDP)

# read in the data
library(data.table)
gdp.data<- fread("data/gdp.csv", skip = 4)
str(gdp.data)
setnames(gdp.data, "Country Name","cName")
myvars<-c("cName","2010","2011","2012","2013","2014","2015","2016")
# subset data
gdp.last7yr<- gdp.data[,myvars, with=FALSE] # check this SO post answer by Josh o'Brien http://stackoverflow.com/questions/13383840/select-multiple-columns-in-data-table
str(gdp.last7yr)

library(dplyr)
gdp.top10<- filter(gdp.last7yr, cName=="Canada"|cName=="China"|cName=="Germany"|
                     cName=="Spain"|cName=="France"|cName=="United Kingdom"|
                     cName=="India"|cName=="Korea"|cName=="Malaysia"|
                     cName=="United States")

library(reshape)
gdp.long.form <-melt(gdp.top10, id="cName") # group the values by country name
names(gdp.long.form) <-c("Country", "Year","GDP_USD_Trillion") # rename the columns

library(ggplot2)
ggplot(gdp.long.form, aes(x=Year, y=GDP_USD_Trillion, group=Country)) +
  geom_line(aes(colour=Country))+
  geom_point(aes(colour=Country),size =5) +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face ="italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Year") +
  ylab("GDP (in trillion USD)") +
  ggtitle("Gross Domestic Product - Top 10 Countries")