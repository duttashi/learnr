# clean the workspace
rm(list=ls())
# load the data in Environment
load("~/R playground/LearningR/ChicagoCrimes.RData")

# load the libraries
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(plyr)
library(dplyr)
library(lubridate)
library(ggmap)
library(extrafont)

coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

# read in the data
df.dat<- read_csv("C:/Users/Ashoo/Downloads/references/New folder/crimes-in-chicago/Chicago_Crimes_2012_to_2017.csv", 
               col_types = coltypes)
# lowercase the column names
names(df.dat)<- tolower(names(df.dat))

# rename the column names
df.dat<- plyr::rename(df.dat, c("case number" = "casenum", "location description" = "locdescr", 
                            "community area" = "commarea","fbi code" = "codefbi",
                            "x coordinate" = "xcord", "y coordinate" = "ycord",
                            "updated on" = "updatedon","primary type"="primtype"
                          )
                )
# convert character date to POSIXct Date type
df.dat$date<-as.POSIXct(df.dat$date,format="%m/%d/%Y %H:%M:%S",tz=Sys.timezone()) # heavy resource dependent step
class(df.dat$date)
# Split the date and time to separate columns
# reference: https://www.kaggle.com/mircat/sf-crime/violent-crime-mapping
df.dat <-
  df.dat %>%
  mutate(Year  = factor(year(date), levels=2012:2017),
         Month = factor(month(date), levels=1:12),
         Day   = day(date),
         Hour  = factor(hour(date), levels=0:23),
         dayDate = as.POSIXct(round(date, units = "days")
         )
  )

# convert character vectors to factors
df.dat$block<- as.factor(df.dat$block)
df.dat$primtype<- as.factor(df.dat$primtype)
df.dat$description<- as.factor(df.dat$description)
df.dat$locdescr<- as.factor(df.dat$locdescr)
df.dat$arrest<- as.factor(df.dat$arrest)
df.dat$domestic<- as.factor(df.dat$domestic)
df.dat$codefbi<- as.factor(df.dat$codefbi)
df.dat$primtype<- as.factor(df.dat$primtype)

########### HIGHLY RESOURCE INTENSIVE, SKIP IT ##################
# Missing data treatment
# Visualize missing data
library(VIM)
aggr_plot <- aggr(mapdata, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(mapdata), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# Impute the missing data
library(mice)
df.complete<-mice(mapdata,m=2,maxit=50,meth='pmm',seed=500)
#################################################################

# lets look at crime categories now
unique(df.dat$primtype)

# Observation: The categories for Violent crimes should be 'Assault','Robbery','Homicide','Sex offense'. Let's group these columns
mapdata<- df.dat %>% filter(primtype %in% c('ASSAULT','ROBBERY','HOMICIDE','CRIM SEXUAL ASSAULT'))

mapdata %>%
  group_by(primtype) %>%
  dplyr::summarise(n=n())

# MISSING DATA REMOVAL FOR NOW
mapdata.cmplt<- mapdata[complete.cases(mapdata),]
sum(is.na(mapdata.cmplt))
colSums(is.na(mapdata.cmplt))

# Observation: Occurences of assault outnumber the other crimes. Let's plot them
# lets get the Chicago, USA map
# Latitude and longitude coordinates are: 41.881832, -87.623177.
chicago<- c(lon= -87.623177, lat=41.881832)

# The zoom parameter specificies how far you want the map to be zoomed in. In general, 3 is a continent, 10 is a city, and 21 is a building. 
# zoom=13
map<-get_map(location= chicago, zoom=13, maptype = "roadmap",
             source='google',color='color')
lims <- coord_map(xlim=c(-122.47, -122.37), ylim=c(37.70, 37.81))

ggmap(map, extent='panel', legend="topright") +
  geom_point(aes(x=longitude, y=latitude, colour=primtype), 
             data=mapdata.cmplt,alpha=.5, size=3) +
  ggtitle('Violent Crime in Chicago')+
  theme(axis.title = element_blank(),text = element_text(size = 12))

# Observation: Not quite a useful plot. The colors are overlapping each other. No patterns are visible. Lets see if it can be made more interesting
ggmap(map, extent='panel') +
  geom_point(aes(x=longitude, y=latitude, colour=primtype), data=mapdata.cmplt, 
             alpha=.5, na.rm = T, size=3) +
  scale_colour_discrete(guide='none') +
  facet_wrap(~primtype) +
  ggtitle('Violent Crime in Chicago')+
  theme(axis.title = element_blank(), text = element_text(size = 12))

# Observation: Not quite useful still even after breaking it down by categories. Lets try the contour plot
contours <- stat_density2d(
  aes(x=longitude, y=latitude, fill = ..level..),
  size = 0.5, data = mapdata.cmplt, n=200,
  geom = "polygon")

########### Below this scripts not working properly. The crimes are not shown on the map ########
levels(mapdata.cmplt$primtype) # 33 levels
breaks<- levels(mapdata.cmplt$primtype)



ggmap(map, extent='panel', legend="topright") + 
  scale_alpha_continuous(range=c(0.25,0.1), guide='none') +
  scale_fill_gradient('Violent\nCrime\nDensity')+
  ggtitle('Violent Crime in Chicago')

ggmap(map, extent = "panel", maprange = FALSE)+
  geom_density2d(data = mapdata.cmplt, aes(x=longitude, y=latitude))+
  stat_density2d(data = mapdata.cmplt, aes(x=longitude, y=latitude,  fill = ..level.., alpha = ..level..),
                 size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(map, extent = "panel", maprange=FALSE)+
  stat_density_2d(
    data=mapdata.cmplt,
    aes(x=longitude, y=latitude, fill = ..level.., alpha = ..level..),
    size = 0.01, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

# Lets look at the map by specific Crime categories
table(mapdata$primtype)

contoursALT <- stat_density2d(
  aes(x=longitude, y=latitude, fill = ..level.., alpha=..level..),
  size = 0.10, data = filter(mapdata.cmplt, primtype=='ASSAULT'), n=200,
  geom = "polygon")

contoursROB <- stat_density2d(
  aes(x=longitude, y=latitude, fill = ..level.., alpha=..level..),
  size = 0.1, data = filter(mapdata.cmplt, primtype=='ROBBERY'), n=200,
  geom = "polygon")

contoursHOM <- stat_density2d(
  aes(x=longitude, y=latitude, fill = ..level.., alpha=..level..),
  size = 0.1, data = filter(mapdata.cmplt, primtype=='HOMICIDE'), n=200,
  geom = "polygon")

contoursCSA <- stat_density2d(
  aes(x=longitude, y=latitude, fill = ..level.., alpha=..level..),
  size = 0.1, data = filter(mapdata.cmplt, primtype=='CRIM SEXUAL ASSAULT'), n=200,
  geom = "polygon")
################################################################################

##### From here, the plots are working #####

ggplot(data=mapdata.cmplt, aes(x=Month)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') 

ggplot(data=mapdata.cmplt, aes(x=Month)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~primtype, scales='free')

ggplot(data=mapdata.cmplt, aes(x=Hour)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') 

ggplot(data=mapdata.cmplt, aes(x=Hour)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count') +
  facet_wrap(~primtype, scales='free')

ggplot(data=filter(mapdata, primtype=='ASSAULT'), aes(x=Hour)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count of Assault') +
  facet_wrap(~Month, scales='free')+
  ggtitle("Monthly & Hourly cases for Assault")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=filter(mapdata, primtype=='ROBBERY'), aes(x=Hour)) +
  geom_bar(colour="black", fill="skyblue") +
  ylab('Count of Robbery') +
  facet_wrap(~Month, scales='free')+
  ggtitle("Monthly & Hourly cases for Robbery")+
  theme(plot.title = element_text(hjust = 0.5))

############ 31 Jan 2017 ##############
# Mapping the data 
library(maps)
library(mapdata)

# Get the State maps
state_map <- map_data("state",region="illinois")
# Zoom in on Chicago and get the counties
chicago_df<- subset(state_map, region=="illinois")
head(chicago_df)
## Get the counties too
county_map_data <- map_data("county", region="illinois")
chic_county <- subset(county_map_data, region == "illinois")
head(chic_county)
## Plot the state
chic_base <- ggplot(data = chicago_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
chic_base + theme_nothing()
## Now plot the county boundaries in white
chic_base + theme_nothing() + 
  geom_polygon(data = chic_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top



