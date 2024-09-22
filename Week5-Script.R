# UAP 5494 - Week 5 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("car", "stargazer")) 

library(tidyverse)
library(here)
library(vtable)
library(car)
library(stargazer)


# Prepare data for regression ----

# Load housing data
## Data source: https://www.kaggle.com/datasets/yasserh/housing-prices-dataset/data
Housing_data <- read_csv(here("Data", "Housing.csv")) 

## code categorical variables 
### Factors are how R interprets categorical variables 
### Make sure you translate non-factors into factors before trying to use them in a regression  
Housing_data$mainroad <- factor(Housing_data$mainroad)
Housing_data$guestroom <- factor(Housing_data$guestroom)
Housing_data$basement <- factor(Housing_data$basement)
Housing_data$hotwaterheating <- factor(Housing_data$hotwaterheating)
Housing_data$airconditioning <- factor(Housing_data$airconditioning)
Housing_data$prefarea <- factor(Housing_data$prefarea)
Housing_data$furnishingstatus <- factor(Housing_data$furnishingstatus)


## Inspect data
summary(Housing_data) ### If you don't make categorical variables into factors, this won't work for them 
### You could also use try using plot() here


## Check to see if log transformations are needed
hist(Housing_data$area) ### Is it normally distributed or does it need transformation?
Housing_data$LogArea <- log(Housing_data$area)
hist(Housing_data$LogArea) ### Is this better?


## Make new variable - average housing footprint
Housing_data$footprint <- Housing_data$area/Housing_data$stories 


## Summary statistics
sumtable(Housing_data)




# Create correlation matrix ----
## Only works with numeric data
CorrelationData <- Housing_data %>%
  select(price,
         area,
         bedrooms,
         bathrooms,
         parking,
         footprint)

## Basic correlation matrix
cor(CorrelationData)
## Not super easy to read. Plenty of alternatives online. 


# Regression ----
HousingRegression <- lm(price ~ ., data=Housing_data) ## . puts in all available variables

### Display regression results
summary(HousingRegression)

## Check for multicollinearity 
vif(HousingRegression)
## If some predictors are categorical vif() will report generalized VIFs (GVIFs) and scaled GVIFs
## VIF (or squared scaled GVIF) = 1,  no added uncertainty in model estimates because of collinearity 
## VIF (or squared scaled GVIF) >4, there’s a problem you should probably try to fix
## VIF (or squared scaled GVIF) >10,  something definitely must be done 
## Source: https://stacyderuiter.github.io/s245-notes-bookdown/collinearity-and-multicollinearity.html

## Make a nicer table to show/export results. Plenty of packages you can find online.



# Evaluate regression ----

## Plot Predicted v Actual
Housing_data %>%
  ggplot(aes(x=predict(HousingRegression), y=price)) +
  geom_point() + ### Plot type (scatter)
  geom_abline(intercept=0, slope=1) +
  scale_y_continuous(labels=scales::dollar_format(), limits=c(0,14000000)) +
  scale_x_continuous(labels=scales::dollar_format(), limits=c(0,14000000)) +
  labs(title = "Predicted vs. Actual Values",
       x = "Predicted Values",
       y = "Actual Values")

## Plot residuals
residuals <- resid(HousingRegression)
plot(density(residuals)) 
