# UAP 5494 - Week 1 ----

rm(list = ls()) ## clears the Environment window

## Getting set up----

dir.create("Data")
dir.create("Data_output")
dir.create("Figure_output")

install.packages(c("tidyverse", "here")) 

library(tidyverse)
library(here)


v1 <- c(1,2,3,4,5)
v2 <- 6:12

print(v1)
print(v2)

v3 <- c(v1,v2)

print(v3)

m1 <- matrix(v3, nrow = 4, byrow = T)

?matrix

print(m1)

print(m1[, 2])

L1 <- list ("A", TRUE, v3, m1) 
 print(L1)
 
 
 
 
 
 ## FTA NTD Data----
 
 Transit_data <- read_csv(here("Data", "2021_FTA_NTD_Data.csv")) ## Using "here" function makes it easier to find a file in the working directory
 
 summary(Transit_data) ## summary statistics
 
 
 # What was the total bus ridership for Virginia? ====
 
 Virginia_bus_data <- filter(Transit_data, State == "VA", Mode == "MB"| Mode == "CB" | Mode == "RB") ## Comma separates required filters, | used as  "or"

 ## Alternative option
 Virginia_bus_data <- Transit_data %>% ## Piping allows multiple functions to be applied to the one dataframe 
   filter(State == "VA", 
          Mode == "MB" |
          Mode == "CB"|
          Mode == "RB") %>% 
   filter(! `Organization Type` == "Private-Non-Profit Corporation") ## new filter to remove items I don't want from the list (using !)

 
 ## Ridership calculation
 
 VA_Bus_Ridership_2021 <- sum(Virginia_bus_data$`Unlinked Passenger Trips`) ## Using $ allows you to select a specific variable 
 ##R will add single quotation marks if the variable is not formatted in a way it can easily read

 
 
 
 
 
 # Which Virginian bus agency had the highest average productivity for all bus operations? ====
 
 ## Group bus data by transit agency
 
 Virginia_bus_agencies <- Virginia_bus_data %>% 
   group_by(Agency) %>% ### group by agency
   summarise(across(where(is.numeric), sum)) ### sum the variables by agency if they are numeric 
 
  ## Create a new column and calculate the productivity measure using other columns of data
 Virginia_bus_agencies$Productivity <- Virginia_bus_agencies$`Unlinked Passenger Trips`/Virginia_bus_agencies$`Vehicle Revenue Hours`

 ## Sort the data frame by the new column, biggest to smallest
 Virginia_bus_agencies <- Virginia_bus_agencies[order(Virginia_bus_agencies$Productivity, decreasing = TRUE),]   
### You can now visually view the data and find the result 
 
 ## Alternatively...
 
Agency_highest_productivity <- Virginia_bus_agencies %>% 
  filter(Productivity == max(Productivity)) %>% # filter the dataframe to keep row where Productivity is highest (max)
  select(Agency) ## Select the value from the Agency column from the filtered row 





# Which bus mode had better farebox recovery in Virginia: local bus (MB) or commuter bus (CB)?  ====

## Problem is that R doesn't recognize the currency formatted numbers as numeric. 

## Change financial data type from character to numeric

Virginia_bus_data$`Fare Revenues Earned` <- parse_number(Virginia_bus_data$`Fare Revenues Earned`)
Virginia_bus_data$`Total Operating Expenses` <- parse_number(Virginia_bus_data$`Total Operating Expenses`)

## Group bus data by transit agency

Virginia_bus_types <- Virginia_bus_data %>% 
  group_by(Mode) %>% ### group by agency
  summarise(across(where(is.numeric), sum)) ### sum the variables by agency if they are numeric 

## Create a new column and calculate farebox recovery using other columns of data

Virginia_bus_types$FareboxRecovery <- Virginia_bus_types$`Fare Revenues Earned`/Virginia_bus_types$`Total Operating Expenses`

## Either view table to determine which is higher or get R to tell you which is higher...

Commuter_Bus <- Virginia_bus_types %>% 
  filter(Mode == "CB") %>% 
  select(FareboxRecovery) 

Local_Bus <- Virginia_bus_types %>% 
  filter(Mode == "MB") %>% 
  select(FareboxRecovery) 

ifelse(Commuter_Bus > Local_Bus,"Commuter is higher","Local is higher")
