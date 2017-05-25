# clear the workspace
rm(list=ls())
# Objective: EDA of Zillow’s Home Value Prediction (https://www.kaggle.com/c/zillow-prize-1/data)


# Load the required libraries
library(data.table) # for fread
library(tidyverse)
library(ggplot2)
library(corrplot) # for corrplot

data_train<- fread("data/zillow/train_2016.csv", showProgress = FALSE)
data_properties<- fread("data/zillow/properties_2016.csv", showProgress = FALSE)

# Check for missing values
sum(is.na(data_train)) # NIL missing values
sum(is.na(data_properties)) # [1] 75210239 missing values
colSums(is.na(data_properties))

# Renaming the column names
data_properties <- data_properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

data_train <- data_train %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)
