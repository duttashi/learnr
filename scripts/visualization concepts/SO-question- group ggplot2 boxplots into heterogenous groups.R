# Objective: group ggplot2 boxplots into heterogenous groups (not the usual grouping of boxplots)?
# reference: https://stackoverflow.com/questions/56104383/how-to-group-ggplot2-boxplots-into-heterogenous-groups-not-the-usual-grouping-o

# minimal reprex
rm(list = ls())
library(datasets)
library(ggplot2)
library(dplyr)
data(airquality)
airquality$Month <- factor(airquality$Month,
                           labels = c("May", "Jun", "Jul", "Aug", "Sep"))

plot <- ggplot(airquality, aes(x = Month, y = Ozone, fill = Month)) +
  geom_boxplot()
plot

# Requirement: To have a gap between June and July as well as July and August

# Solution # 1
# This might be cheating but I'm not quite sure how much you care about the labeling on your x axis
special_x <- case_when(airquality$Month == "May" ~ 1,
                       airquality$Month == "Jun" ~ 2,
                       airquality$Month == "Jul" ~ 4,
                       airquality$Month == "Aug" ~ 6,
                       airquality$Month == "Sep" ~ 7)

airquality$special_x <- special_x

ggplot(airquality, aes(x = special_x, y = Ozone, fill = Month)) +
  geom_boxplot()

# Solution # 2
# create a variable that groups your x variable—the method depends on your data, but forcats::fct_collapse is one easy way to do this. Then use that to facet the plots. With facet_grid, you can set a free x-scale and free spacing, so that the panels are sized based on how many boxplots they each have.
airquality$Month <- factor(airquality$Month,
                           labels = c("May", "Jun", "Jul", "Aug", "Sep"))

air_groups <- airquality %>%
  mutate(group = forcats::fct_collapse(Month, 
                                       "group 1" = c("May", "Jun"),
                                       "group 2" = c("Jul"),
                                       "group 3" = c("Aug", "Sep")))
ggplot(air_groups, aes(x = Month, y = Ozone, fill = Month)) +
  geom_boxplot() +
  facet_grid(cols = vars(group), scales = "free_x", space = "free")

# remove the facet labels.
ggplot(air_groups, aes(x = Month, y = Ozone, fill = Month)) +
  geom_boxplot() +
  facet_grid(cols = vars(group), scales = "free_x", space = "free") +
  theme(strip.text = element_blank())
