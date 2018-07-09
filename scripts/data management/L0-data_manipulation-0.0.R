# Data Manipulation in R with dplyr
# https://rpubs.com/robertwsellers/r_bridge_final
# Load the dplyr package
library(dplyr)

# Load the hflights package
library(hflights)

# Call both head() and summary() on hflights
head(hflights)

# convert to a tibble
hflights<- tbl_df(hflights)
str(hflights)

# Create the carriers, containing only the UniqueCarrier variable of hflights
carriers <- hflights$UniqueCarrier

abc <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Use abc to translate the UniqueCarrier column of hflights
hflights$UniqueCarrier <- abc[hflights$UniqueCarrier]

# one column with missing label
table(hflights$CancellationCode)

hflights <- hflights %>%
  mutate(
    CancellationCode = ifelse(CancellationCode == "", "E", CancellationCode)
  )

table(hflights$CancellationCode)
# Build the lookup table: lut
lut <- c("A" = "carrier",
         "B" = "weather",
         "C" = "FFA",
         "D" = "security",
         "E" = "not cancelled")

# Use the lookup table to create a vector of code labels. Assign the vector to the CancellationCode column of hflights
hflights$Code <- lut[hflights$CancellationCode]

# Inspect the resulting raw values of your variables
glimpse(hflights)
