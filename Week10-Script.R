# UAP 5494 - Week 10 ----

# Get set up
rm(list = ls()) ## clears the Environment window

library(tidyverse)
library(here)
library(tidycensus)
library(sf)
library(mapview)


# Data ----
v17 <- load_variables(2022,"acs5", cache = TRUE)
View(v17)

## MWCOG TPB Equity Emphasis Areas https://www.mwcog.org/assets/1/6/methodology.pdf

## TPB Geographies
## DC
## VA
TPB_VA <- c("Fairfax county", 
            "Arlington county",
            "Loudoun county",
            "Alexandria city",
            "Falls Church city",
            "Fairfax city",
            "Prince William county",
            "Manassas city",
            "Manassas Park city")

## MD
TPB_MD <- c("Charles county",
            "Frederick county",
            "Montgomery county",
            "Prince George's county")


MD_EquityData <- get_acs(
  geography = "tract", 
  variables = c(TotalPovPopulation = "B06012_001",
                Below100PctPovertyLvl = "B06012_002",
                Btw_100_150_PctPovertyLvl = "B06012_003",
                Race_TotPop = "B02001_001",
                Race_Black = "B02001_003",
                Race_Asian = "B02001_005",
                Ethnicity_TotPop = "B03002_001",
                Ethnicity_HispLat = "B03002_012"),
  year = 2022,
  state = "MD",
  county = TPB_MD,
  geometry = TRUE,
  output = "wide"
)

VA_EquityData <- get_acs(
  geography = "tract", 
  variables = c(TotalPovPopulation = "B06012_001",
                Below100PctPovertyLvl = "B06012_002",
                Btw_100_150_PctPovertyLvl = "B06012_003",
                Race_TotPop = "B02001_001",
                Race_Black = "B02001_003",
                Race_Asian = "B02001_005",
                Ethnicity_TotPop = "B03002_001",
                Ethnicity_HispLat = "B03002_012"),
  year = 2022,
  state = "VA",
  county = TPB_VA,
  geometry = TRUE,
  output = "wide"
)


DC_EquityData <- get_acs(
  geography = "tract", 
  variables = c(TotalPovPopulation = "B06012_001",
                Below100PctPovertyLvl = "B06012_002",
                Btw_100_150_PctPovertyLvl = "B06012_003",
                Race_TotPop = "B02001_001",
                Race_Black = "B02001_003",
                Race_Asian = "B02001_005",
                Ethnicity_TotPop = "B03002_001",
                Ethnicity_HispLat = "B03002_012"),
  year = 2022,
  state = "DC",
  geometry = TRUE,
  output = "wide"
)

## Combine the data from the three MWCOG TPB states
TPB_EquityData <- rbind(DC_EquityData, VA_EquityData, MD_EquityData)

mapview(TPB_EquityData)

## New variables

TPB_EquityData$PctLowIncome <- (TPB_EquityData$Below100PctPovertyLvlE + TPB_EquityData$Btw_100_150_PctPovertyLvlE)/TPB_EquityData$TotalPovPopulationE*100
TPB_EquityData$PctBlack <- TPB_EquityData$Race_BlackE/TPB_EquityData$Race_TotPopE*100
TPB_EquityData$PctAsian <- TPB_EquityData$Race_AsianE/TPB_EquityData$Race_TotPopE*100
TPB_EquityData$PctHispanic <- TPB_EquityData$Ethnicity_HispLatE/TPB_EquityData$Ethnicity_TotPopE*100

summary(TPB_EquityData)

TPB_EquityData <- replace(TPB_EquityData, is.na(TPB_EquityData), 0) ## Get rid of NAs


TPB_EEA_Data <- TPB_EquityData %>%
  dplyr::select(PctLowIncome,
                PctBlack,
                PctAsian,
                PctHispanic)

## Check correlations
TPB_EEA_DataCorr <- st_drop_geometry(TPB_EEA_Data)

CorMatrix <- cor(TPB_EEA_DataCorr)
## Some moderate correlations between poverty and the Black population percentages

# Identify EEAs

## Calculate averages
AvgPctLowIncome <- mean(TPB_EEA_Data$PctLowIncome)
AvgPctBlack <- mean(TPB_EEA_Data$PctBlack)
AvgPctAsian <- mean(TPB_EEA_Data$PctAsian)
AvgPctHispanic <- mean(TPB_EEA_Data$PctHispanic)

## Calculate Ratio of Concentrations
TPB_EEA_Data$ROC_LowIncome <- TPB_EEA_Data$PctLowIncome/AvgPctLowIncome
TPB_EEA_Data$ROC_Black <- TPB_EEA_Data$PctBlack/AvgPctBlack
TPB_EEA_Data$ROC_Asian <- TPB_EEA_Data$PctAsian/AvgPctAsian
TPB_EEA_Data$ROC_Hispanic <- TPB_EEA_Data$PctHispanic/AvgPctHispanic

## Max value is 3 for index so replace values greater than 3 with 3
TPB_EEA_Data$ROC_LowIncome <- ifelse(TPB_EEA_Data$ROC_LowIncome > 3, 3, TPB_EEA_Data$ROC_LowIncome)
TPB_EEA_Data$ROC_Black <- ifelse(TPB_EEA_Data$ROC_Black > 3, 3, TPB_EEA_Data$ROC_Black)
TPB_EEA_Data$ROC_Asian <- ifelse(TPB_EEA_Data$ROC_Asian > 3, 3, TPB_EEA_Data$ROC_Asian)
TPB_EEA_Data$ROC_Hispanic <- ifelse(TPB_EEA_Data$ROC_Hispanic > 3, 3, TPB_EEA_Data$ROC_Hispanic)

## Index is zero if <1.5 so replace values less than 1.5 with 0
TPB_EEA_Data$ROC_LowIncome <- ifelse(TPB_EEA_Data$ROC_LowIncome < 1, 0, TPB_EEA_Data$ROC_LowIncome) ## Low-income is 0 when <1
TPB_EEA_Data$ROC_Black <- ifelse(TPB_EEA_Data$ROC_Black < 1.5, 0, TPB_EEA_Data$ROC_Black)
TPB_EEA_Data$ROC_Asian <- ifelse(TPB_EEA_Data$ROC_Asian < 1.5, 0, TPB_EEA_Data$ROC_Asian)
TPB_EEA_Data$ROC_Hispanic <- ifelse(TPB_EEA_Data$ROC_Hispanic < 1.5, 0, TPB_EEA_Data$ROC_Hispanic)

## The ROC for low-income is multiplied by three to determine the index score but capped at 9.0.
TPB_EEA_Data$ROC_LowIncome <- TPB_EEA_Data$ROC_LowIncome*3

summary(TPB_EEA_Data)

## Deterime EEAs
### Criteria 1: Tracts must have a concentration of individuals identified as low-income more than 1.5x the regional average
### Criteria 2: high concentrations of 2+ minority population groups and/or high concentrations of 1+ minority population groups together with low income concentrations at or above the regional average

TPB_EEA_Data$EEA <- ifelse((TPB_EEA_Data$ROC_LowIncome +
                             TPB_EEA_Data$ROC_Black +
                             TPB_EEA_Data$ROC_Asian +
                             TPB_EEA_Data$ROC_Hispanic) > 4, "Yes", "No")

mapview(TPB_EEA_Data, 
        zcol = "EEA")

## Filter EEAs
TPB_EEAs <- TPB_EEA_Data %>% ## Piping allows multiple functions to be applied to the one dataframe 
  filter(EEA == "Yes")

mapview(TPB_EEAs)

## Check correlations
TPB_EEA_Data <- TPB_EEAs %>%
  dplyr::select(PctLowIncome,
                PctBlack,
                PctAsian,
                PctHispanic)

TPB_EEA_Data <- st_drop_geometry(TPB_EEA_Data)

CorMatrix <- cor(TPB_EEA_Data)
## Some stronger correlations in the EEA only data



# Principal Component Analysis (PCA) ----
PCA <- prcomp(TPB_EEA_Data, scale=TRUE)

PCA

plot(PCA$x[,1], PCA$x[,2]) #### x contains the principal components (PCs) for drawing a graph
### this function used first two columns in x to draw a 2-D plot, using the first two PCs
### this shows us how we can use 2 dimensions to explain most of the data - data reduction! 

pca.var <- PCA$sdev^2 #### calculation variation in the original data each PC accounts for; Eigenvalues
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) #### calculating percentage variation

#### PC1 acocunts for 49.4% of variation and PC2 accounts for 24.4% of the variation
#### PC1 and PC2 account for 73.8% of the variation,

## Make a Scree Plot using eigenvalues
PCAVar <- as.data.frame(pca.var)
PCAVar$PCnum <- 1:nrow(PCAVar)
PCAVar$PCnum <- factor(PCAVar$PCnum)
PCAVar <- rename(PCAVar, Eigenvalue = pca.var)
PCAVar <- PCAVar[,c(2,1)]

ggplot(PCAVar, aes(x=PCnum, y=Eigenvalue, group=1)) + 
  geom_line() +
  geom_point() +
  labs(title = "Scree  - Eigenvalue", x = "Principal Component", y = "Eigenvalue") 


## Make a Scree Plot using percent variation
pca.var.percent <- as.data.frame(pca.var.per) #### Change to df
pca.var.percent$PCnum <- 1:nrow(pca.var.percent) #### add sequential numbers for PCs
pca.var.percent$PCnum <- factor(pca.var.percent$PCnum) #### change new PCs to factors for labeling purposes
pca.var.percent <- rename(pca.var.percent, Percent = pca.var.per) #### rename column

### Plot
ggplot(pca.var.percent, aes(x=PCnum, y=Percent)) + 
  geom_bar(stat = "identity", fill = "darkseagreen4") +
  ylim(0,50) +
  labs(title = "Scree  - Variations", x = "Principal Component", y = "Percent Variation (%)") + 
  geom_text(aes(label = Percent), vjust = -0.5, size =3)

loading_scores <- PCA$rotation[,1] #### prcomp calls loading scores rotation
var_scores <- abs(loading_scores) #### Interested in both negative and positive relationships so use absolute values to get magnitude
var_ranked <- sort(var_scores, decreasing=TRUE) ### sort high to low
top_variables <- names(var_ranked[1:4])

top_variables

Top <- PCA$rotation[top_variables,1] ### show the scores (and +/- sign)
TopNeat <- round(Top, digits=3)
TopVar <- data.frame(TopNeat)
TopVar <- cbind(Variable = rownames(TopVar), TopVar)

TopVar
## While the TPB Equity Emphasis Areas are primarily defined by income, the Black population is the largest influence 

