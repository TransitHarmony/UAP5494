# UAP 5494 - Week 6 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("sjPlot", "sjlabelled", "sjmisc")) 

library(tidyverse)
library(here)
library(vtable)
library(car)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(caret)

# Import 2022 bus stop amenity data -----

BusStopAmenityData22 <- read_csv(here("Data", "BusStopAmenityData2022-pax.csv"))

summary(BusStopAmenityData22)


# Prepare data -----

## filter out excluded data 
CleanBusStopData22 <- filter(BusStopAmenityData22, Exclude == "No")


## Change data to numeric as needed
CleanBusStopData22$RoutesCount <- as.numeric(CleanBusStopData22$RoutesCount)
CleanBusStopData22$WeeklyTrips <- as.numeric(CleanBusStopData22$WeeklyTrips)

summary(CleanBusStopData22)

## code categorical variables 
CleanBusStopData22$Nearside <-ifelse(CleanBusStopData22$Location=="Near side",1,0)
CleanBusStopData22$Farside <-ifelse(CleanBusStopData22$Location=="Far side",1,0)
CleanBusStopData22$SharedStop <-ifelse(CleanBusStopData22$Shared=="Yes",1,0)
CleanBusStopData22$City <-ifelse(CleanBusStopData22$CityCounty=="City",1,0)
CleanBusStopData22$StateRoad <-ifelse(CleanBusStopData22$RoadOwner=="state",1,0)
CleanBusStopData22$LocalRoad <-ifelse(CleanBusStopData22$RoadOwner=="local",1,0)

summary(CleanBusStopData22)

### Get rid of NAs
CleanBusStopData22$Nearside[is.na(CleanBusStopData22$Nearside)] <- 0
CleanBusStopData22$Farside[is.na(CleanBusStopData22$Farside)] <- 0

## Test categorical variable combinations
xtabs(~ Seating + Nearside, data=CleanBusStopData22)
xtabs(~ Seating + Farside, data=CleanBusStopData22)
xtabs(~ Seating + SharedStop, data=CleanBusStopData22)
xtabs(~ Seating + City, data=CleanBusStopData22)
xtabs(~ Seating + StateRoad, data=CleanBusStopData22)
xtabs(~ Seating + LocalRoad, data=CleanBusStopData22)
xtabs(~ Seating + Residential, data=CleanBusStopData22)
xtabs(~ Seating + Retail, data=CleanBusStopData22)
xtabs(~ Seating + Office, data=CleanBusStopData22) ### Sample size might be too small
xtabs(~ Seating + School, data=CleanBusStopData22) ### Sample size might be too small

xtabs(~ Shelter + Nearside, data=CleanBusStopData22)
xtabs(~ Shelter + Farside, data=CleanBusStopData22)
xtabs(~ Shelter + SharedStop, data=CleanBusStopData22)
xtabs(~ Shelter + City, data=CleanBusStopData22)
xtabs(~ Shelter + StateRoad, data=CleanBusStopData22)
xtabs(~ Shelter + LocalRoad, data=CleanBusStopData22)
xtabs(~ Shelter + Residential, data=CleanBusStopData22)
xtabs(~ Shelter + Retail, data=CleanBusStopData22)
xtabs(~ Shelter + Office, data=CleanBusStopData22) ### Sample size might be too small
xtabs(~ Shelter + School, data=CleanBusStopData22) ### Sample size might be too small

## Check to see if log transformations are needed
hist(CleanBusStopData22$TotalRidership) #### Looks like a log-normal distribution
CleanBusStopData22$LogRidership <- log(.01 + CleanBusStopData22$TotalRidership)
hist(CleanBusStopData22$LogRidership)

hist(CleanBusStopData22$RoutesCount)
CleanBusStopData22$LogRoutes <- log(CleanBusStopData22$RoutesCount)
hist(CleanBusStopData22$LogRoutes) ## Inclusion of routes might not be viable

hist(CleanBusStopData22$WeeklyTrips)
CleanBusStopData22$LogTrips <- log(CleanBusStopData22$WeeklyTrips)
hist(CleanBusStopData22$LogTrips)

hist(CleanBusStopData22$Population)
CleanBusStopData22$LogPop <- log(CleanBusStopData22$Population)
hist(CleanBusStopData22$LogPop)

hist(CleanBusStopData22$PctZeroCar)
CleanBusStopData22$LogZeroCar <- log(0.01 + CleanBusStopData22$PctZeroCar) ## Percent zero cars has a zero value
hist(CleanBusStopData22$LogZeroCar)

hist(CleanBusStopData22$PctTransit)
CleanBusStopData22$LogTransitMode <- log(CleanBusStopData22$PctTransit)
hist(CleanBusStopData22$LogTransitMode)

hist(CleanBusStopData22$PctNonWhite)

hist(CleanBusStopData22$PctLowIncome)
CleanBusStopData22$LogLowIncome <- log(CleanBusStopData22$PctLowIncome)
hist(CleanBusStopData22$LogLowIncome)

hist(CleanBusStopData22$EMP)
CleanBusStopData22$LogJobs <- log(CleanBusStopData22$EMP)
hist(CleanBusStopData22$LogJobs)


# Correlation Matrix -----
CorrelationData <- CleanBusStopData22 %>%
  dplyr::select(TotalRidership,
                RoutesCount,
                WeeklyTrips,
                Population,
                PctZeroCar,
                PctTransit,
                PctNonWhite,
                PctLowIncome,
                EMP)

cor(CorrelationData)
### routes has moderate-high correlation with trips
### PctTransit is moderately correlated with population and PctZeroCar
### PctNonWhite is moderately correlated with PctLowIncome


# Summary statistics

sumtable(CleanBusStopData22)


# Logit Regression Model 1 - Likelihood bus stop has seating  -----
SeatingGLM <- glm(Seating ~ LogRidership +
                    LogTrips +
                    LogPop +
                    PctNonWhite +
                    LogLowIncome +
                    LogJobs +
                    SharedStop +
                    City +
                    LocalRoad +
                    Residential +
                    Retail,
                  family = "binomial",
                  data = CleanBusStopData22)

summary(SeatingGLM)


vif(SeatingGLM)


## Logit model results
### Relative risk ratios
logit.or = exp(coef(SeatingGLM))


## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_model(SeatingGLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "Relative Risk Ratio",
           vline.color = "grey")


## McFadden's Pseudo R2
Transport.ll.null <- SeatingGLM$null.deviance/-2
Transport.ll.proposed <- SeatingGLM$deviance/-2

(Transport.ll.null - Transport.ll.proposed)/Transport.ll.null 
### Pseuedo R2 = 0.2455109 

## P-value
1 - pchisq(2*(Transport.ll.proposed - Transport.ll.null), df=(length(SeatingGLM$coefficients)-1)) 
### P = 2.338685e-12


## Confusion Matrix

Stop.predicted <- predict(SeatingGLM, 
                               CleanBusStopData22, 
                               type="response")
Stop.P_class <- ifelse(Stop.predicted > .5, "1", "0")

Stop.P_class <- as.factor(Stop.P_class)

### Confusion matrix (2x2)
table(Stop.P_class, CleanBusStopData22[["Seating"]])

### Produces table of model statistics
confusionMatrix(as.factor(CleanBusStopData22$Seating), as.factor(Stop.P_class))

?confusionMatrix

#### Accuracy : 0.8139
#### 95% CI : (0.7626, 0.8582)
#### No Information Rate : 0.7956 





# Logit Regression Model 2 - Likelihood bus stop has shelter  -----






