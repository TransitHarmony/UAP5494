# UAP 5494 - Week 7 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("sf", "units", "AER", "MASS")) 

library(tidyverse)
library(here)
library(tidycensus)
library(mapview)
library(vtable)
library(sf)
library(units)
library(AER) ## used for dispersion test
library(MASS)
library(sjPlot)
library(sjlabelled)
library(sjmisc)



# Data ----

## Census data ====
v17 <- load_variables(2022,"acs5", cache = TRUE)
View(v17)

DC_Data <- get_acs(
  geography = "block group",
  variables = c(TotalPopulation = "B01003_001",
                Households_1veh_Rent = "B25044_004",
                Households_2veh_Rent = "B25044_005",
                Households_3veh_Rent = "B25044_006",
                Households_4veh_Rent = "B25044_007",
                Households_5veh_Rent = "B25044_008",
                Households_1veh_Own = "B25044_011",
                Households_2veh_Own = "B25044_012",
                Households_3veh_Own = "B25044_013",
                Households_4veh_Own = "B25044_014",
                Households_5veh_Own = "B25044_015",
                Households_All = "B25044_001",
                Income = "B19013_001",
                Households_AllRent = "B25003_003",
                BlackPop = "B02001_003",
                BelowPoverty = "B17101_002",
                Below_and_Above_Poverty = "B17101_001"),
  year = 2022,
  state = "DC",
  geometry = TRUE,
  output = "wide"
)

## Create new variables
DC_Data$AvgVehHH <- (DC_Data$Households_1veh_RentE + 
                     DC_Data$Households_2veh_RentE*2 +
                     DC_Data$Households_3veh_RentE*3 +
                     DC_Data$Households_4veh_RentE*4 +
                     DC_Data$Households_5veh_RentE*5 +
                     DC_Data$Households_1veh_OwnE +
                     DC_Data$Households_2veh_OwnE*2 +
                     DC_Data$Households_3veh_OwnE*3 +
                     DC_Data$Households_4veh_OwnE*4 +
                     DC_Data$Households_5veh_OwnE)/DC_Data$Households_AllE 

DC_Data$RentPct <- DC_Data$Households_AllRentE/DC_Data$Households_AllE*100 

DC_Data$PovertyPct <- DC_Data$BelowPovertyE/DC_Data$Below_and_Above_PovertyE*100 

DC_Data <- DC_Data %>%  ### Calculate area of each polygom
  mutate(area = st_area(.))

DC_Data$area <- set_units(DC_Data$area, mi^2) ### Change units from metres squared to miles squared

mapview(DC_Data,
        zcol = "AvgVehHH") 

## Keep variables of interest
DC_Data <- DC_Data %>%
  dplyr::select(GEOID, ## MASS also uses select; need to tell R which select function to use
         area,
         TotalPopulationE,
         IncomeE,
         RentPct,
         AvgVehHH, 
         BlackPopE,
         PovertyPct)




# Other data ====

## Load shapefiles downloaded from DC Open Data
DC_2023Crime <- st_read('data/Crime Incidents - 2023/Crime Incidents - 2023.shp')
CCTV_Locations <- st_read('data/Closed_Circuit_TV_Street_Cameras/Closed_Circuit_TV_Street_Cameras.shp') 

mapview(CCTV_Locations,
        zcol = "WARD") 

## Check out how many crimes of each type were observed
DC_2023Crime %>% count(OFFENSE)

## Subset crime data to create two new crime variables: burglary, homicide

Burglary_2023 <- DC_2023Crime %>% ## Piping allows multiple functions to be applied to the one dataframe 
  filter(OFFENSE == "BURGLARY")

mapview(Burglary_2023,
        zcol = "WARD")

Homicide_2023 <- DC_2023Crime %>% ## Piping allows multiple functions to be applied to the one dataframe 
  filter(OFFENSE == "HOMICIDE")

mapview(Homicide_2023,
        zcol = "WARD")


# Spatial data wrangling ====

# Check coordinate reference systems
st_crs(DC_Data)
st_crs(DC_2023Crime)
st_crs(CCTV_Locations)
## coordinate reference systems have to match

## Change coordinate system to match census data
DC_2023Crime <- st_transform(DC_2023Crime, 4269)
CCTV_Locations <- st_transform(CCTV_Locations, 4269)



# Find points (all offenses) in polygons (block groups)
DC_CensusCrimeData <- st_join(DC_2023Crime, DC_Data, join = st_within) ## x = points, y = polygons

DC_CensusCrimeData_Count <- count(as_tibble(DC_CensusCrimeData), GEOID) ## count points in polygons
DC_CensusCrimeData_Count <- rename(DC_CensusCrimeData_Count, CrimeCount = n) ## relabel n

DC_CensusAllCrime <- merge(DC_Data, ## add count to census data (x = spatial object, y = data frame)
                               DC_CensusCrimeData_Count, 
                               by = 'GEOID',
                               all.x = T) ## this ensures all x observations are kept

DC_CensusAllCrime <- replace(DC_CensusAllCrime, is.na(DC_CensusAllCrime), 0) ## Get ride of NAs

## Check the results to see if it worked
mapview(DC_CensusAllCrime, ## check to see if the polygons have counts
        zcol = "CrimeCount")

sum(DC_CensusAllCrime$CrimeCount) ## check counts sum to total crimes



## Change coordinate system to match census data
Burglary_2023 <- st_transform(Burglary_2023, 4269)
Homicide_2023 <- st_transform(Homicide_2023, 4269)



# Find points (burglaries) in polygons (block groups)
DC_CensusBurglData <- st_join(Burglary_2023, DC_Data, join = st_within) ## x = points, y = polygons

DC_CensusBurglData_Count <- count(as_tibble(DC_CensusBurglData), GEOID) ## count points in polygons
DC_CensusBurglData_Count <- rename(DC_CensusBurglData_Count, BurglaryCount = n) ## relabel n

DC_CensusCrimeBurglary <- merge(DC_CensusAllCrime, ## add count to census data (x = spatial object, y = data frame)
                                DC_CensusBurglData_Count, 
                               by = 'GEOID',
                               all.x = T) ## this ensures all x observations are kept

DC_CensusCrimeBurglary <- replace(DC_CensusCrimeBurglary, is.na(DC_CensusCrimeBurglary), 0) ## Get ride of NAs

## Check the results to see if it worked
mapview(DC_CensusCrimeBurglary, ## check to see if the polygons have counts
        zcol = "CrimeCount")

sum(DC_CensusCrimeBurglary$BurglaryCount) ## check counts sum to total crimes



# Find points (homicides) in polygons (block groups)
DC_CensusHomicideData <- st_join(Homicide_2023, DC_Data, join = st_within) ## x = points, y = polygons

DC_CensusHomicideData_Count <- count(as_tibble(DC_CensusHomicideData), GEOID) ## count points in polygons
DC_CensusHomicideData_Count <- rename(DC_CensusHomicideData_Count, HomicideCount = n) ## relabel n

DC_CensusCrimeCombine <- merge(DC_CensusCrimeBurglary, ## add count to census data (x = spatial object, y = data frame)
                                DC_CensusHomicideData_Count, 
                                by = 'GEOID',
                                all.x = T) ## this ensures all x observations are kept

DC_CensusCrimeCombine <- replace(DC_CensusCrimeCombine, is.na(DC_CensusCrimeCombine), 0) ## Get ride of NAs

## Check the results to see if it worked
mapview(DC_CensusCrimeCombine, ## check to see if the polygons have counts
        zcol = "HomicideCount")

sum(DC_CensusCrimeCombine$HomicideCount) ## check counts sum to total crimes


# Find points (camera) in polygons (block groups)
DC_CensusCameraData <- st_join(CCTV_Locations, DC_Data, join = st_within) ## x = points, y = polygons

DC_CensusCameraData_Count <- count(as_tibble(DC_CensusCameraData), GEOID) ## count points in polygons
DC_CensusCameraData_Count <- rename(DC_CensusCameraData_Count, CameraCount = n) ## relabel n

DC_CensusDataCombine <- merge(DC_CensusCrimeCombine, # add count to census data (x = spatial object, y = data frame)
                              DC_CensusCameraData_Count, 
                              by = 'GEOID',
                              all.x = T) ## this ensures all x observations are kept

DC_CensusDataCombine <- replace(DC_CensusDataCombine, is.na(DC_CensusDataCombine), 0) ## Get ride of NAs

## Check the results to see if it worked
mapview(DC_CensusDataCombine, ## check to see if the polygons have counts
        zcol = "CameraCount")

sum(DC_CensusDataCombine$CameraCount) ## check counts sum to total crimes


## Drop geometry to make spatial file a dataframe
DC_All_Data <- st_drop_geometry(DC_CensusDataCombine)

## Drop unneeded variable
DC_All_Data$GEOID <- NULL

## Export csv
write.csv(DC_All_Data, here("Data_Output", "DC_All_Data.csv"), row.names=FALSE)





# Poisson and Negative Binomial Regression ----

## Load  data
DC_All_Data <- read_csv(here("Data_Output", "DC_All_Data.csv")) 


## Create new variables as desired
DC_All_Data$PopDensity <- DC_All_Data$TotalPopulationE/DC_All_Data$area 

DC_All_Data$PopDensity <- as.numeric(DC_All_Data$PopDensity)
DC_All_Data$area <- as.numeric(DC_All_Data$area)

## Inspect/transform data as necessary 
summary(DC_All_Data)

hist(DC_All_Data$CameraCount)

hist(DC_All_Data$CrimeCount)
DC_All_Data$SqrtCrime <- sqrt(DC_All_Data$CrimeCount)
hist(DC_All_Data$SqrtCrime)

hist(DC_All_Data$PopDensity)
DC_All_Data$SqrtPopDensity <- sqrt(DC_All_Data$PopDensity)
hist(DC_All_Data$SqrtPopDensity)

hist(DC_All_Data$BlackPopE)
DC_All_Data$LogBlkPop <- log(.01 + DC_All_Data$BlackPopE)
hist(DC_All_Data$LogBlkPop)

hist(DC_All_Data$RentPct)



# Summary statistics ====
sumtable(DC_All_Data)


# Correlation matrix ====
CorrelationData <- DC_All_Data %>%
  dplyr::select(CameraCount, ## MASS also has a select function; using two colons tells R which package I want for the function 
         CrimeCount,
         BurglaryCount,
         HomicideCount,
         PovertyPct,
         AvgVehHH,
         BlackPopE,
         PopDensity,
         RentPct)

CorMatrix <- cor(CorrelationData)


# Poisson regression ====
Poisson_GLM <- glm(CameraCount ~ PovertyPct + 
                     SqrtCrime + 
                     AvgVehHH, 
                   data=DC_All_Data, 
                   family = poisson(link = "log")) 
summary(Poisson_GLM)

## Exponentiate results
Poisson_Results = exp(coef(Poisson_GLM))

Poisson_Results

## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_model(Poisson_GLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "CCTV Poission Regression",
           vline.color = "grey")

# Dispersion test (to determine if we should use Poisson or Negative Binomial)
dispersiontest(Poisson_GLM)
### results suggest over dispersion (p=0.03) and thus negative binomial is more appropriate


# Negative Binomial regression ====
NB1_GLM <- glm.nb(CameraCount ~ PovertyPct + 
                    SqrtCrime + 
                    AvgVehHH, 
                  data=DC_All_Data)
summary(NB1_GLM)

NB_Results = exp(coef(NB1_GLM))

NB_Results

## Forest plot
### https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
plot_model(NB1_GLM, 
           show.values = TRUE, 
           value.offset = .4,
           axis.title = "CCTV Negative Binomial Regression",
           vline.color = "grey")

NB0_GLM <- glm.nb(CameraCount ~ 1, data=DC_All_Data)
summary(NB0_GLM)

# Is model better than the null negative binomial model?
anova(NB1_GLM, NB0_GLM)
## likelihood ratio test suggests that it is better


