# Data Source information
# The data is derived from https://www.kaggle.com/aljarah/xAPI-Edu-Data
# clear the workspace
rm(list = ls())
# Load the data
stud.data <- read.table("C:/Users/Ashoo/Downloads/references/New folder/xAPI-Edu-Data.csv",
                        sep = ",", header = TRUE, stringsAsFactors = TRUE)

# Data exploration by visualization
## http://r4ds.had.co.nz/data-visualisation.html
library(ggplot2)
str(stud.data)

# Countrywise student population
ggplot(data = stud.data, aes(x = PlaceofBirth)) + geom_bar(aes(fill = NationalITy)) + 
  labs(x = "Birth Place", y = "Student Count") + coord_flip() # usa is a mix of nationalities

# Highest Student population. The countries Kuwait & Jordan have the highest student count
ggplot(data = stud.data, aes(x = NationalITy)) + geom_bar() + 
  labs(x = "Nationality", y = "Student Count")
  scale_y_continuous(breaks = seq(0,200,20)) + coord_flip()
# check for student grades
ggplot(data = stud.data, aes(x = GradeID, fill = Class)) + geom_bar() + 
    labs(x = "Grade ID", y = "Student Count") + coord_flip() # g-05 has students with only the low grade

# class wise student plot
# class C has only IT & Science students
ggplot(data = stud.data, aes(x = SectionID, fill = Topic)) + geom_bar() +
  labs(x = "Section ID", y = "Student Count") +
  coord_flip()

# Kuwait has max students in IT, Egypt has least students in Math, Quran and English. French class has max diversity of nationaities while Chemistry class has the least diversity of nationalities
ggplot(data = stud.data, aes(x = Topic, fill = NationalITy)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,100,10)) + coord_flip()

# H1: girls read more tends to have more questions to clarify in class
ggplot(data = stud.data, aes(x=raisedhands, y=VisITedResources, color=gender))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data = stud.data, aes(x=raisedhands, y=Discussion, color=gender)) + geom_point() +
  geom_smooth(method = "lm")

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

# BOXPLOTS
str(stud.data)
# girl student have more hand raises in class
ggplot(data = stud.data, aes(x = gender, y = raisedhands)) + geom_boxplot()
# girls access more resources
ggplot(data = stud.data, aes(x = gender, y = VisITedResources)) + geom_boxplot()
# girls view slightly more announcements than boys
ggplot(data = stud.data, aes(x = gender, y = AnnouncementsView)) + geom_boxplot()
# girls are more involved in discussions
ggplot(data = stud.data, aes(x = gender, y = Discussion)) + geom_boxplot()
# Jordan more hand raises than KW (=Kuwait). lybia lowest. iraq and palestine highest hand raises.
ggplot(data = stud.data, aes(x = NationalITy, y = raisedhands)) + geom_boxplot()
# more hand raises in the middle school
ggplot(data = stud.data, aes(x = StageID, y = raisedhands)) + geom_boxplot()

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