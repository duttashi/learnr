# Cheatsheet: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
library(ggmap)
myloc<- "University of Malaya"
my.latlong<- geocode(myloc)
my.latlong
myMap<- get_map(location = my.latlong, source = "stamen", maptype = "watercolor", crop = FALSE)
ggmap(myMap)
myMap1<- get_map(location = my.latlong, source = "google", maptype = c("roadmap"), crop = FALSE, 
                 zoom = 17)
ggmap(myMap1)
