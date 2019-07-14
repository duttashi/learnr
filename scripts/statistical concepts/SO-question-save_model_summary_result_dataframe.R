# Reference: https://stackoverflow.com/questions/57024367/r-question-how-to-save-summary-results-into-a-dataset

library(tidyverse)
library(broom)

str(mtcars)
str(diamonds)

df<-diamonds %>% 
  nest(-cut) %>% 
  mutate(model = purrr::map(data, function(x) { 
    lm(price ~ carat + depth, data = x)}), 
    values = purrr::map(model, glance), 
    r.squared = purrr::map_dbl(values, "r.squared"), 
    pvalue = purrr::map_dbl(values, "p.value")) %>% 
  select(-data, -model, -values)
head(df)  

summary_result<-mtcars %>%
  nest(-carb) %>%
  mutate(model = purrr::map(data, function(x) {
    lm(gear ~ mpg+cyl, data = x)}),
    values = purrr::map(model, glance),
    r.squared = purrr::map_dbl(values, "r.squared"),
    pvalue = purrr::map_dbl(values, "p.value")) %>%
  select(-data, -model, -values)
summary_result
