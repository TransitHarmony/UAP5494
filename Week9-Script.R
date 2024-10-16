# UAP 5494 - Week 9 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("spdep", "spatialreg")) 

library(tidyverse)
library(here)
library(tidycensus)
library(tidytransit)
library(mapview)
library(units)
library(sf)
library(spdep)
library(spatialreg)
library(stargazer)

# Data ----

# Census Data ====
v17 <- load_variables(2022,"acs5", cache = TRUE)
View(v17)

### Stucky & Ottensmann (2009) paper from week 7
### Population, % black, % Hispanic, high density (yes/no) disadvantage index, land use
## Infrastructure variables: roads

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
                Households_AllRent = "B25003_003",
                BlackPop = "B02001_003",
                BelowPoverty = "B17101_002",
                Below_and_Above_Poverty = "B17101_001"),
  year = 2022,
  state = "DC",
  geometry = TRUE,
  output = "wide"
)


DC_Data <- DC_Data %>%  ### Calculate area of each polygon
  mutate(area = st_area(.))

DC_Data$area <- set_units(DC_Data$area, mi^2) ### Change units from metres squared to miles squared

DC_Data$area <- as.numeric(DC_Data$area) ### Make variable a number 

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

DC_Data$BlackPct <- DC_Data$BlackPopE/DC_Data$TotalPopulationE*100 

DC_Data$PopDensity <- DC_Data$TotalPopulationE/DC_Data$area 


mapview(DC_Data,
        zcol = "AvgVehHH") 

summary(DC_Data)

DC_Data <- replace(DC_Data, is.na(DC_Data), 0) ## Get rid of NAs



## Keep variables of interest
DC_Data <- DC_Data %>%
  dplyr::select(GEOID,
         PopDensity,
         PovertyPct,
         RentPct,
         BlackPct,
         AvgVehHH)

summary(DC_Data)



# Crime Data ====

## Load shapefiles downloaded from DC Open Data
DC_2023Crime <- st_read('data/Crime Incidents - 2023/Crime Incidents - 2023.shp')

## Check out how many crimes of each type were observed
DC_2023Crime %>% count(OFFENSE)
### Let's choose violent crimes like Stucky & Ottensmann (2009)


mapview(DC_2023Crime,
        zcol = "OFFENSE") 


## Subset crime data to create two new crime variables: burglary, homicide
### Let's choose violent crimes like Stucky & Ottensmann (2009)
### ASSAULT W/DANGEROUS WEAPON, ROBBERY, HOMICIDE, SEX ABUSE      

ViolentCrime <- DC_2023Crime %>% ## Piping allows multiple functions to be applied to the one dataframe 
  filter(OFFENSE == "ASSAULT W/DANGEROUS WEAPON" |
           OFFENSE == "ROBBERY" |
           OFFENSE == "HOMICIDE" |
           OFFENSE == "SEX ABUSE")

## Change coordinate system to match census data
ViolentCrime <- st_transform(ViolentCrime, 4269)


mapview(ViolentCrime,
        zcol = "OFFENSE") 
## Any spatial patterns emerging?


## # Find points (all offenses) in polygons (block groups)
DC_CensusCrimeData <- st_join(ViolentCrime, DC_Data, join = st_within) ## x = points, y = polygons

DC_Data <- DC_Data %>%
  dplyr::select(GEOID, 
         PopDensity,
         PovertyPct,
         RentPct,
         BlackPct,
         AvgVehHH,
         PovertyPct)

DC_CensusCrimeData_Count <- count(as_tibble(DC_CensusCrimeData), GEOID) ## count points in polygons
DC_CensusCrimeData_Count <- rename(DC_CensusCrimeData_Count, ViolentCrimeCount = n) ## relabel n

DC_All <- merge(DC_Data, ## add count to census data (x = spatial object, y = data frame)
                           DC_CensusCrimeData_Count, 
                           by = 'GEOID',
                           all.x = T) ## this ensures all x observations are kept


mapview(DC_All,
        zcol = "ViolentCrimeCount")

DC_All <- replace(DC_All, is.na(DC_All), 0) ## Get rid of NAs



# Infrastructure data ====

# Read roads shapefile ====
DC_Roads <- read_sf('data/Roadway_Functional_Classification/Roadway_Functional_Classification.shp') 

## Remove features with empty geometries
DC_Roads <- DC_Roads[!st_is_empty(DC_Roads$geometry), ]
### This was something I had to get help with so I reached out to the stackoverflow community
### https://stackoverflow.com/questions/79068210/how-to-resolve-potential-z-and-m-dimension-issues-with-a-road-centerline-shapefi/79068284

## Change coordinate reference system to match census data
DC_Roads <- st_transform(DC_Roads, 4269)

## Remove z (elevation) and m (specific measurement) dimensions - these often don't work well in R
DC_Roads <- st_zm(DC_Roads)

mapview(DC_Roads)

## Intersect road geometry with census data
DC_All_Roads <- st_intersection(DC_Roads, DC_All)

DC_All_Roads$RoadLength = st_length(DC_All_Roads) ## Measure length of road
DC_All_Roads$RoadLength <- set_units(DC_All_Roads$RoadLength, mi) ### Change units from metres to miles

## Keep variables of interest
DC_All_Roads <- DC_All_Roads %>%
  select(GEOID, 
         RoadLength) %>%
  group_by(GEOID) %>%
  summarise(RoadLength = sum(RoadLength))

Roads_df <- st_drop_geometry(DC_All_Roads)

DC_All <- merge(DC_All, ## add count to census data (x = spatial object, y = data frame)
                                Roads_df, 
                                by = 'GEOID',
                                all.x = T) ## this ensures all x observations are kept




# Load streetlight shapefiles ====
DC_Streetlights <- st_read('data/Street_Lights/Street_Lights.shp')

## Remove features with empty geometries
DC_Streetlights <- DC_Streetlights[!st_is_empty(DC_Streetlights$geometry), ]

## Change coordinate reference system to match census data
DC_Streetlights <- st_transform(DC_Streetlights, 4269)

## Remove z (elevation) and m (specific measurement) dimensions - these often don't work well in R
DC_Streetlights <- st_zm(DC_Streetlights)

mapview(DC_Streetlights,
        zcol = "WARD")



## # Find points (streetlights) in polygons (block groups)
DC_Light <- st_join(DC_Streetlights, DC_All, join = st_within) ## x = points, y = polygons

DC_Light <- count(as_tibble(DC_Light), GEOID) ## count points in polygons
DC_Light <- rename(DC_Light, StreetlightCount = n) ## relabel n

DC_Everything <- merge(DC_All, ## add count to census data (x = spatial object, y = data frame)
                       DC_Light, 
                by = 'GEOID',
                all.x = T) ## this ensures all x observations are kept

DC_Everything$StreetlightCount <- as.numeric(DC_Everything$StreetlightCount)
DC_Everything$RoadLength <- as.numeric(DC_Everything$RoadLength)

mapview(DC_Everything,
        zcol = "StreetlightCount")

summary(DC_Everything)


## Export shapefile
st_write(DC_Everything, "Data_output/DC_SpatialRegression.shp")




# DC Spatial Regression Data ----
## Load shapefile
DC_Everything <- read_sf('Data_output/DC_SpatialRegression.shp') 

##Relabel (saving as a shapefile reduces labels down to 7 characters)
DC_Everything$PopDensity <- DC_Everything$PpDnsty          
DC_Everything$PovertyPct <- DC_Everything$PvrtyPc          
DC_Everything$ViolentCrimeCount <- DC_Everything$VlntCrC         
DC_Everything$RoadLength <- DC_Everything$RdLngth            
DC_Everything$StreetlightCount <- DC_Everything$StrtlgC 
DC_Everything$BlackPct <- DC_Everything$BlckPct  
DC_Everything$AvgVehHH <- DC_Everything$AvgVhHH             

DC_Everything <- DC_Everything %>%
  dplyr::select(ViolentCrimeCount,
                PopDensity,
                PovertyPct,
                RentPct,
                BlackPct,
                AvgVehHH,
                RoadLength,
                StreetlightCount)

summary(DC_Everything)



# Correlation matrix ====
CorrelationData <- DC_Everything %>%
  dplyr::select(ViolentCrimeCount,
                PopDensity,
                PovertyPct,
                RentPct,
                BlackPct,
                AvgVehHH,
                RoadLength,
                StreetlightCount)

## Drop geometry to make spatial file a dataframe
CorrelationData <- st_drop_geometry(CorrelationData)

CorMatrix <- cor(CorrelationData)
## RoadLength is moderately correlated with StreetlightCount

## Inspect/transform data as necessary ====
summary(DC_Everything)

hist(DC_Everything$ViolentCrimeCount)
DC_Everything$SqrtViolentCrime <- sqrt(DC_Everything$ViolentCrimeCount)
hist(DC_Everything$SqrtViolentCrime)

hist(DC_Everything$PopDensity)
DC_Everything$SqrtPopDensity <- sqrt(DC_Everything$PopDensity)
hist(DC_Everything$SqrtPopDensity)

hist(DC_Everything$RentPct)

hist(DC_Everything$BlackPct)

hist(DC_Everything$PovertyPct)
DC_Everything$sqrtPoverty <- sqrt(DC_Everything$PovertyPct)
hist(DC_Everything$sqrtPoverty)

hist(DC_Everything$AvgVehHH)
DC_Everything$SqrtVehHH <- sqrt(DC_Everything$AvgVehHH)
hist(DC_Everything$SqrtVehHH)

hist(DC_Everything$RoadLength)
DC_Everything$SqrtRoadLength <- sqrt(DC_Everything$RoadLength)
hist(DC_Everything$SqrtRoadLength)

hist(DC_Everything$StreetlightCount)
DC_Everything$LogStreetlight <- log(DC_Everything$StreetlightCount)
hist(DC_Everything$LogStreetlight)


# Spatial Regression ----

## Start with running linear regression ====
ViolentCrime.ols <- lm(SqrtViolentCrime ~ SqrtPopDensity +
                     RentPct +
                     BlackPct +
                     SqrtVehHH +
                     sqrtPoverty +
                     SqrtRoadLength +
                     LogStreetlight, 
                   data = DC_Everything)
summary(ViolentCrime.ols)


## Check for spatial autocorrelation ====

#Define neighboring polygons
DC_Neighbors <- poly2nb(DC_Everything, queen=T)

#Assign weights to neighbors 
DC_weights <- nb2listw(DC_Neighbors, style="W")

#Moran’s I
moran.test(DC_Everything$ViolentCrimeCount, DC_weights) 
### Moran I statistic 0.3876384649 suggests there is spatial autocorrelation     

#Monte Carlo simulation 
moran.mc(DC_Everything$ViolentCrimeCount, DC_weights, nsim = 999)
### p=value 0.001 so the Moran I is statistically significant
### Spatial regression is justified

## Spatial lag regression ====
ViolentCrime.lag<-lagsarlm(SqrtViolentCrime ~ SqrtPopDensity +
                             RentPct +
                             BlackPct +
                             SqrtVehHH +
                             sqrtPoverty +
                             SqrtRoadLength +
                             LogStreetlight,  
                  data = DC_Everything, 
                  listw = DC_weights)

summary(ViolentCrime.lag)
### Rho is the spatial lag coefficient
### In this model, Rho is both positive and statistically significant
### an increase in violent crime in a BG is associated with an increase in neighboring BGs


## Spatial error regression ====
ViolentCrime.err <- errorsarlm(SqrtViolentCrime ~ SqrtPopDensity +
                                 RentPct +
                                 BlackPct +
                                 SqrtVehHH +
                                 sqrtPoverty +
                                 SqrtRoadLength +
                                 LogStreetlight,  
                    data = DC_Everything, 
                    listw = DC_weights) 

summary(ViolentCrime.err)
### Instead of Rho, the lag error parameter is Lambda, which is a lag on the error
### It is positive and significant indicating the need to control for spatial autocorrelation in the error.


# Compare results
stargazer(ViolentCrime.ols, ViolentCrime.lag, ViolentCrime.err, type = "text",
          title="Title: Regression Results")



## Lagrange Multiplier test ====
Tests <- lm.RStests(ViolentCrime.ols, listw = DC_weights, test = "all",  zero.policy=TRUE)

summary(Tests)







