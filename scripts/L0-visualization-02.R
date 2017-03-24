# clear the environment
rm(list = ls())
# Book reference: http://r4ds.had.co.nz/data-visualisation.html#geometric-objects
# load the libraries
library(tidyverse)
head(mpg)
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy)) # negative relationship 
?mpg
dim(mpg)
str(mpg)

# Aesthetic mappings
ggplot(data = mpg)+
  geom_point(mapping=aes(x=hwy, y=cyl)) # this scatterplot is not useful as it does not show any trend or relationship

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, alpha=class)) # where alpha aesthetic controls the shape of the points
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, colour=class))
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy),colour="blue")

ggplot(data = mpg)+
  geom_point(mapping = aes(x=cty, y=cty,shape="cty"))
?geom_point

ggplot(data = mpg, aes(hwy,year))+
  geom_point(shape=21, colour="black", fill="white",size=5, stroke=5)

ggplot(data = mpg)+
  geom_point(mapping = aes(x=cty, y=cty,colour=displ<5))

# Facets

# Note: The variable that you pass to facet_wrap() should be discrete or categorical
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_wrap(~class, nrow = 2)

# To facet your plot on the combination of two variables, add facet_grid() to your plot call. 
ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_grid(drv~cyl)

#If you prefer to not facet in the rows or columns dimension, use a . instead of a variable name, e.g. + facet_grid(. ~ cyl)
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_grid(.~cyl)

# lets try to facet a continuous variable
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ, y=hwy))+
  facet_wrap(~hwy, nrow=2) # The continuous variable is plotted but the plot is nonsense

# what do the empty cells in the following plot mean?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = cyl)) # I think the empty cells indicate missing values

# what plots the following code make?
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) # You could set the shape of a point, but you couldn’t set the “shape” of a line. On the other hand, you could set the linetype of a line. geom_smooth() will draw a different line, with a different linetype, for each unique value of the variable that you map to linetype.

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype=drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))+
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))+
  facet_wrap(~class, nrow = 1)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+ # global mapping
  geom_point()+
  geom_smooth()

# If you place mappings in a geom function, ggplot2 will treat them as local mappings for the layer.
# It will use these mappings to extend or overwrite the global mappings for that layer only. 
# This makes it possible to display different aesthetics in different layers.
ggplot(data=mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(colour=class))+
  geom_smooth()
# The local data argument in geom_smooth() overrides the global data argument in ggplot() for that layer only.
ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(colour=class))+
  geom_smooth(data = filter(mpg, class=="subcompact"), se=FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) + 
  geom_point() + 
  geom_smooth(se=FALSE)

# Question: Will these two graphs look different? Why/why not?
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot() + 
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# Exercise: Chapter 3.6.1- Geometric objects
# Question 6
ggplot(data = mpg,mapping = aes(x=displ, y=hwy))+
  geom_point(shape=21, fill="black", size=5)+
  geom_smooth(se=FALSE)

ggplot(data=mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(mapping = aes(x = displ, y = hwy))+
  geom_smooth(mapping = aes(colour=hwy), se=FALSE)
## Note: the question 6 exercise is incomplete

# 3.7: Statistical Transformations
str(diamonds)

ggplot(data = diamonds)+
  geom_bar(mapping = aes(x=cut))
ggplot(data = diamonds)+
  stat_count(mapping = aes(x=cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
?stat_summary
## Exercise 3.7.1
## Ans. The default geom to stat_summary() is pointrange 
ggplot(data = diamonds) + 
  geom_pointrange(mapping = aes(x=carat, y=depth,
                                ymin = min,
                                ymax = max))
ggplot(data = diamonds)+
  geom_col(mapping = aes(x=cut, y=depth))
