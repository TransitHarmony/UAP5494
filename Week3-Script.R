# UAP 5494 - Week 3 ----

rm(list = ls()) # clears the Environment window

# Install/load packages

install.packages(c("tidycensus","censusapi","mapview","vtable")) 

library(censusapi)
library(tidycensus)
library(mapview)
library(tidyverse)
library(here)
library(vtable)

# Census API  ----

# Check to see if you already have a census key saved
Sys.getenv("CENSUS_API_KEY")

# If not already saved, add your key to your Renviron file
census_api_key("KEY", install = TRUE)

# Reload .Renviron
readRenviron("~/.Renviron")

# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_API_KEY")



# Search for variables
## censusreporter.org also helps

v17 <- load_variables(2022,"acs5", cache = TRUE)
View(v17)


# Washington, DC Percentage of Zero Car Households ----
DC_Data <- get_acs(
  geography = "block group",
  variables = c(ZeroCarRent = "B25044_010",
                ZeroCarOwn = "B25044_003",
                Households = "B25044_001"),
  year = 2022,
  state = "DC",
  geometry = TRUE,
  output = "wide"
)

## Calculate percentage of zero car households 
DC_Data$ZeroCarPct <- (DC_Data$ZeroCarRentE + DC_Data$ZeroCarOwnE)/DC_Data$HouseholdsE*100 

mapview(DC_Data,
        zcol = "ZeroCarPct")

## Looks like some block groups are giving NA's for some reason - we may need to do some data cleaning






# Descriptive Statistics and Data cleaning ----

## First, we need to understand the issue - descriptive statistics can help

## Descriptive statistics table
sumtable(DC_Data)
## The count (N) for our new variable "ZeroCarPct" is lower than the other variables

## Summary() is a basic R function that can allow us to summarize the issues
summary(DC_Data)
## Looking at the ZeroCarPct shows that we have 21 NA's

## Filter data by NA's to see if we can identify a problem
DC_NA <- DC_Data %>% filter(if_any(everything(), is.na)) ## everything() selects all variables
## Looking at the data we can quickly see the issue: The estimates and households have zeroes

## Let's Visually inspect these erroneous block groups... 
mapview(DC_NA)
## Looks like they're all federal buildings, other office buildings, or universities - places without residents


## Two choices: 1) Filter out the data with zero households, 2) Make NA's into zeroes 


# 1) Filter out the block groups with zero households
DC_Cleaned <- DC_Data %>% filter(!is.na(ZeroCarPct))

## Let's have a look...
mapview(DC_Cleaned,
        zcol = "ZeroCarPct")
## Looks exactly the same except now when we check out our descriptive statistics...
sumtable(DC_Cleaned)
## We have the same number of observations for every variable, saving us from annoying errors if we use these data in some kind of analysis


# 2) MAke NA's into zeroes

## What if join these data to another data set with all BG's kept? We might want to change the NA's instead of filtering them out
DC_Data_Zero <- replace(DC_Data, is.na(DC_Data), 0)

## Let's have a look...
mapview(DC_Data_Zero,
        zcol = "ZeroCarPct")
## Now check out our descriptive statistics...
sumtable(DC_Data_Zero)
## Same number of observations for all variables; check summary...
summary(DC_Data_Zero)
## All NA's are gone

## Note: Solution 2 is not appropriate for all variables and will affect your measures of central tendency 



# Comparing Work from Home ====
WFH_DC_Data <- get_acs(
  geography = "state",
  survey = "acs1", ## get_acs() defaults to the 5-year ACS with the argument survey = "acs5"; survey = "acs1" is used for 1-year ACS data
  variables = c(WFH_Rent = "B08537_020",
                WFH_Own = "B08537_021",
                Households = "B08537_001"),
  year = 2022,
  state = "DC",
  geometry = TRUE,
  output = "wide"
)

WFH_DC_Data$WFH_Pct <- (WFH_DC_Data$WFH_RentE + WFH_DC_Data$WFH_OwnE)/WFH_DC_Data$HouseholdsE*100






# Virginia percentage white population ====

VA_Data <- get_acs(
  geography = "tract",
  variables = c(WhitePop = "B02001_002",
                TotalPop = "B02001_001"),
  year = 2022,
  state = "VA",
  geometry = TRUE,
  output = "wide"
)

## Calculate percentage of zero car households 
VA_Data$VA_WhitePercent <- VA_Data$WhitePopE/VA_Data$TotalPopE*100 

mapview(VA_Data,
        zcol = "VA_WhitePercent")
