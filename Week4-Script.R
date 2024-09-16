# UAP 5494 - Week 4 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("scales", "tmap")) 

library(tidyverse)
library(here)
library(tidycensus)
library(mapview)
library(scales)
library(tmap)





# 1) Show the relationship between ridership and local bus vehicle revenue hours ----

## Load the 2021 FTA NTD data
Transit_data <- read_csv(here("Data", "2021_FTA_NTD_Data.csv")) ## Using "here" function makes it easier to find a file in the working directory

## Filter for local bus (MB) data 
Bus_data <- filter(Transit_data, Mode == "MB")

Bus_data <- Transit_data %>% ## Piping allows multiple functions to be applied to the one dataframe 
  filter(Mode == "MB") %>% 
  filter(`Organization Type` == "City, County or Local Government Unit or Department of Transportation" | 
           `Organization Type` == "Independent Public Agency or Authority of Transit Service" |
           `Organization Type` == "University" |
           `Organization Type` == "State Government Unit or Department of Transportation") 

## Make variables easier to use
Bus_data$`Unlinked Passenger Trips (millions)` <- Bus_data$`Unlinked Passenger Trips`/1000000
Bus_data$`Vehicle Revenue Hours (millions)` <- Bus_data$`Vehicle Revenue Hours`/1000000

## Make a scatter plot for Unlinked Passenger Trips (ridership) and Vehicle Revenue Hours
ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`)) + 
  geom_point() ## geom_point will give us a scatter plot

## Color the plot based on Organization Type
ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`, colour = `Organization Type`)) + 
  geom_point() +
  theme(legend.position = "bottom") + ## legend position moves the legend in relation to the plot
  guides(color = guide_legend(nrow = 2)) ## the guide re-shapes the legend, in this case into 2 rows

## Play with your axis limits to better view relationships
ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`, colour = `Organization Type`)) + 
  geom_point() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2)) +  
  coord_cartesian(xlim =c(0, 3), ylim = c(0, 60)) ## using coord_cartesian allows you to zoom without clipping data

## Add lines of best fit to see trends in the data
ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`, colour = `Organization Type`)) + 
  geom_point() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2)) +  
  coord_cartesian(xlim =c(0, 3), ylim = c(0, 60)) +
  geom_smooth(method = "lm", se = FALSE) ## geom_smooth gives you a line of best fit; "lm" is a straight line; 
## se gives confidence interval; default is TRUE so it's turned off here

## Add 95% confidence envelope
ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`, colour = `Organization Type`)) + 
  geom_point() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2)) +  
  coord_cartesian(xlim =c(0, 3), ylim = c(0, 60)) +
  geom_smooth(method = "lm", alpha = .15, aes(fill = `Organization Type`)) ## alpha controls the opacity of the confidence interval

## Save plot
Bus_plot <- ggplot(Bus_data, aes(`Vehicle Revenue Hours (millions)`, `Unlinked Passenger Trips (millions)`, colour = `Organization Type`)) + 
  geom_point() + 
  theme(legend.position = "bottom") + 
  guides(color = guide_legend(nrow = 2)) +  
  coord_cartesian(xlim =c(0, 3), ylim = c(0, 60)) +
  geom_smooth(method = "lm", alpha = .15, aes(fill = `Organization Type`)) 
## this bit of code saves the plot as an object  

Bus_plot %>% ggsave(filename = here("Figure_output","Bus_Agency_Type_Plot.jpg")) ## ggsave only works for objects









# 2) Plot median household income for Virginian counties ----

## Get census data

VA_income <- get_acs(
  state = "VA",
  geography = "county",
  variables = c(hhincome = "B19013_001"),
  year = 2022,,
  geometry = TRUE
) %>%
  mutate(NAME = str_remove(NAME, ", Virginia")) ## Every  name includes this; we will remove it to make it easier to read

mapview(VA_income,
        zcol = "estimate") ## Does data look ok?


ggplot(VA_income, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", ## labs are labels; title is the title
       x = "Median Income", ## x-axis label
       y = "Top 10 Cities and Counties") + ## y-axis label
  theme_minimal(base_size = 12.5) + ## plot design type
  scale_x_continuous(labels = label_dollar()) ## this gives the x axis dollar formatting
## This is far too much data for one graph

## Filter for top 10 median incomes (estimate)
VA_top_incomes <- VA_income %>%
  top_n(10, estimate)

mapview(VA_top_incomes,
        zcol = "estimate") ## Does data look ok?

## Take another look...
ggplot(VA_top_incomes, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       x = "Median Income", 
       y = "Top 10 Cities and Counties") +
  theme_minimal(base_size = 12.5) + 
  scale_x_continuous(labels = label_dollar())

## let's add margins of error and have a look
ggplot(VA_top_incomes, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       x = "Median Income", 
       y = "Top 10 Cities and Counties") +
  theme_minimal(base_size = 12.5) + 
  scale_x_continuous(labels = label_dollar())

# Make plot into an object
VA_income_plot <- ggplot(VA_top_incomes, aes(x = estimate, y = reorder(NAME, estimate))) + 
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(size = 3, color = "darkgreen") + 
  labs(title = "Median household income", 
       x = "Median Income", 
       y = "Top 10 Cities and Counties") + 
  theme_minimal(base_size = 12.5) + 
  scale_x_continuous(labels = label_dollar())

VA_income_plot %>% ggsave(filename = here("Figure_output","VA_income_plot.jpg"),
                          width = 6, 
                          height = 4) ## ggsave only works for objects








# 3) Make a map showing median income by county/city for Virginia ----
## We already have the data from earlier

## Let's have another look...
mapview(VA_income,
        zcol = "estimate")
## Need to make a static map from this

## Define breaks for four income groups
Breaks <- c(0, 50000, 75000, 100000, 500000)

## Create map using tmap
tm_shape(VA_income, 
         unit = "imperial") + ## change units to miles
  tm_polygons(col = "estimate",
              style = "fixed",
              breaks = Breaks,
              palette = "Greens", ## colors for the choropleth
              title = "Median Income",## Title for the legend
              labels = c("<$50k", "$50-75k", "$75-100k", ">$100k")) + ## Relabel the legend
  tm_layout(title = "Virginia Counties and Cities") + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_compass(position = c("right", "top")) 

## Save plot as object
Virginia_Income_Map <- tm_shape(VA_income, 
                                unit = "imperial") + ## change units to miles
  tm_polygons(col = "estimate",
              style = "fixed",
              breaks = Breaks,
              palette = "Greens", ## colors for the choropleth
              title = "Median Income",## Title for the legend
              labels = c("<$50k", "$50-75k", "$75-100k", ">$100k")) + ## Relabel the legend
  tm_layout(title = "Virginia Counties and Cities") + 
  tm_scale_bar(position = c("right", "bottom")) + 
  tm_compass(position = c("right", "top")) 

Virginia_Income_Map %>% tmap_save(filename = here("Figure_output","Virginia_Income_Map.jpg")                                  ,
                                  width = 6, 
                                  height = 4) ## tmap_save only works for objects
