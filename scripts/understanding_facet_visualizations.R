# script objective: plotting multiple visualizations in a single grid or single plot or single page

library(ggplot2)
library(gridExtra)
g1<- ggplot(mpg, aes(displ, cty)) +
  # plot the geometric object
  geom_point()+
  # facet_grid forms a matrix of panels defined by row and column facetting variables. It is most useful when you have two discrete variables, and all combinations of the variables exist in the data.
  # for more information, see help(facet_grid)
  facet_grid(.~cyl)+
  theme_bw()

g2<- ggplot(mpg, aes(displ, hwy)) +
  # plot the geometric object
  geom_point()+
  # facet_grid forms a matrix of panels defined by row and column facetting variables. It is most useful when you have two discrete variables, and all combinations of the variables exist in the data.
  # for more information, see help(facet_grid)
  facet_grid(.~cyl)+
  theme_bw()

# Arrange the plot in a 1 by 2 grid or 1 column and 2 rows
grid.arrange(g1, g2, ncol=1, nrow=2)

# Arrange the plot in a 1 by 2 grid or 1 column and 2 rows
grid.arrange(g1, g2, ncol=2, nrow=1)
