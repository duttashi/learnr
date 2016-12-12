# Data Source information
# The data is derived from https://www.kaggle.com/aljarah/xAPI-Edu-Data
# clear the workspace
rm(list = ls())
# Load the data
stud.data <- read.table("C:/Users/Ashoo/Downloads/references/New folder/xAPI-Edu-Data.csv",
                        sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Data exploration by visualization
## http://r4ds.had.co.nz/data-visualisation.html
library(ggplot2)
# H1: students who read more tends to have more questions to clarify in class
# Create a scatterplot
ggplot(data = stud.data)+
  geom_point(mapping = aes(x=VisITedResources, y=raisedhands, color=gender))

# fitting a smooth line. From this graph we can see a linear relationship between checking course resources and asking questions
ggplot(data = stud.data)+
  geom_smooth(mapping = aes(x=VisITedResources, y=raisedhands, color=gender))
# H2: students accessing more course resources also check for course announcements
ggplot(data = stud.data)+
  geom_point(mapping = aes(x=VisITedResources, y=AnnouncementsView, color=gender))
## for H2, there tends to be a positive linear relationship however there are some male student outliers

# H2: segregrate the H2 on basis of total grade/mark
## use facets() for mapping categorical data 
ggplot(data = stud.data)+
  geom_point(mapping = aes(x=raisedhands, y=VisITedResources, color=gender))+
  facet_grid(.~Class)

ggplot(data = stud.data)+
  geom_point(mapping = aes(x=raisedhands, y=Discussion, color=gender))+
  facet_grid(ParentschoolSatisfaction~Class)

ggplot(data = stud.data)+
  geom_bar(mapping = aes(x=raisedhands, colour=gender, stat="identity"))+
  facet_grid(ParentschoolSatisfaction~Class)

# H2: segregrate the H2 on basis of total grade/mark
## Using boxplots to check for outliers
### http://brazenly.blogspot.com/2014/04/r-playing-with-legends-of-graph-using.html
ggplot(data = stud.data, aes(x=Class, y=raisedhands, fill=gender)) +
  geom_boxplot()+
  scale_fill_discrete(name="Student gender")

ggplot(data = stud.data, aes(x=Class, y=raisedhands, fill=gender)) +
  geom_boxplot()+
  scale_fill_discrete(name="Subject")+
  facet_grid(~ParentschoolSatisfaction)

ggplot(data = stud.data, aes(x=Class, y=raisedhands, fill=StudentAbsenceDays)) +
  geom_boxplot()+
  scale_fill_discrete(name="Absentism") # keep the graph

ggplot(data = stud.data, aes(x=Class, y=raisedhands, fill=StageID)) +
  geom_boxplot()+
  scale_fill_discrete(name="Total grade/mark") # keep the graph