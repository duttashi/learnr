# Book: http://r4ds.had.co.nz/pipes.html
# pipe is the '%>%' from the magrittr package. 
library(tidyverse)
# Load the data
flight.delay.data<- read.csv("data/flight-delays/flights.csv", header = T, sep = ",")
