library(magrittr)
library(dplyr)
?mtcars
str(mtcars)

# we read the %>% as “and then”- “take mtcars and then filter
# and then group by and then summarize and then arrange.
mtcars %>%
  filter(carb>1)%>%
  group_by(cyl)%>%
  summarise(Avg_mpg=mean(mpg))%>%
  arrange(desc(Avg_mpg))

mtcars %>%
  head %>%
  set_colnames(paste("Col",1:11, sep=""))

# The TEE pipe operator
# normal piping terminates with the plot() function resulting in
# NULL results for the summary() function
mtcars %>%
  filter(carb > 1) %>%
  extract(, 1:4) %>%
  plot() %>%
  summary()
# inserting %T>% allows you to plot and perform the functions that
# follow the plotting function
mtcars %>%
  filter(carb > 1) %>%
  extract(, 1:4) %T>%
  plot() %>%
  summary()

# regular piping results in an error
mtcars %>%
  subset(vs == 0) %>%
  cor(mpg, wt)
## Error in pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", : \
##object 'wt' not found
# using %$% allows you to specify variables of interest
mtcars %>%
  subset(vs == 0) %$%
  cor(mpg, wt)

