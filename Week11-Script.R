# UAP 5494 - Week 11 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("NbClust", "cluster")) 

library(tidyverse)
library(tidycensus)
library(mapview)
library(sf)
library(NbClust)
library(cluster)
library(here)


# Get data ----
# Data ----
v17 <- load_variables(2022,"acs5", cache = TRUE)
View(v17)


VA_data <- get_acs(
  state = "VA",
  geography = "county",
  variables = c(Households_1veh_Rent = "B25044_004",
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
                All_Commutes = "B08301_001",
                Drive_alone = "B08301_003"),
  year = 2022,,
  geometry = TRUE,
  output = "wide") %>%
  mutate(NAME = str_remove(NAME, ", Virginia")) ## Every  name includes this; we will remove it to make it easier to read


## Create new variables
VA_data$AvgVehHH <- (VA_data$Households_1veh_RentE + 
                       VA_data$Households_2veh_RentE*2 +
                       VA_data$Households_3veh_RentE*3 +
                       VA_data$Households_4veh_RentE*4 +
                       VA_data$Households_5veh_RentE*5 +
                       VA_data$Households_1veh_OwnE +
                       VA_data$Households_2veh_OwnE*2 +
                       VA_data$Households_3veh_OwnE*3 +
                       VA_data$Households_4veh_OwnE*4 +
                       VA_data$Households_5veh_OwnE)/VA_data$Households_AllE 

VA_data$PctDriveAlone <- (VA_data$Drive_aloneE/VA_data$All_CommutesE)*100

mapview(VA_data,
        zcol = "PctDriveAlone") 

mapview(VA_data,
        zcol = "AvgVehHH") 

## Keep variables of interest
VA_cl_data <- VA_data %>%
  dplyr::select(NAME,
                PctDriveAlone,
                AvgVehHH)

## Remove geometry
VA_cl_data <- st_drop_geometry(VA_cl_data)

## plot data
plot(VA_cl_data$PctDriveAlone ~ VA_cl_data$AvgVehHH, data = VA_cl_data)



# Cluster Analysis ----
## Scale data
VA_cl <- data.frame(scale(VA_cl_data[,c("AvgVehHH","PctDriveAlone")]))

## Calculate distance matrix
d <- dist(VA_cl) 

# Hierarchical Clustering ====
VA_hc <- hclust(d,method = "ward.D2") ## Ward's method
                
plot(VA_hc, cex = 0.5, labels = VA_cl_data$NAME)


# k-means ====

## Determine how many clusters you should use
VA_Cluster <- NbClust(VA_cl, distance = "euclidean",
                      min.nc = 2,
                      max.nc = 15, 
                      method = "ward.D2")
## NbClust recommends 2 clusters

### k-means clustering algorithm starts with k randomly selected centroids 
### thus, it's recommended to use the set.seed() function in order to set a seed for R’s random number generator
set.seed(1234)

VA_k.means <- kmeans(VA_cl, 2, nstart = 10) 

# compare the clusters
VA_k.means$size ### how many counties/cities in each cluster
VA_k.means$centers ### Centroid values for each cluster

## Add back county names
VA_clusters <- cbind(VA_cl_data, cluster = VA_k.means$cluster)

# Silhouette Plot
## Used to determine how well values fit in their clusters

plot(silhouette(VA_k.means$cluster, d))
## If members of the group are similar, Si values will be high
## If members of the group are NOT similar, Si values will be low
## Negative values might indicate the object is in the wrong cluster

## Avg silhouette 0.48



# Create a scatterplot to visualize the clusters ====
## With points
ggplot(data = VA_clusters, aes(x = AvgVehHH, y = PctDriveAlone, color = as.factor(VA_k.means$cluster))) +
  geom_point(size = 4) +
  labs(title = "Virginia City and County Transportaton Clusters",
       x = "Average Vehicles per Household",
       y = "Drive Alone Commuting Percentage",
       legend = "Cluster") +
  scale_colour_discrete(name= "Cluster") +
  coord_cartesian(xlim = c(1.3, 2.7), ylim = c(40, 90)) +
  theme_minimal()

## With jurisdiction names plotted 
ggplot(data = VA_clusters, aes(x = AvgVehHH, y = PctDriveAlone, color = as.factor(VA_k.means$cluster))) +
  geom_text(aes(label=NAME)) +
  labs(title = "Virginia City and County Transportaton Clusters",
       x = "Average Vehicles per Household",
       y = "Drive Alone Commuting Percentage",
       legend = "Cluster") +
  scale_colour_discrete(name= "Cluster") + 
  coord_cartesian(xlim = c(1.3, 2.7), ylim = c(40, 90)) +
  theme_minimal()


# Try k-means clustering with different value... ====
## NbClust second highest was 6 clusters

set.seed(1234)

VA_k.means.six <- kmeans(VA_cl, 6, nstart = 10) 

# compare the clusters
VA_k.means.six$size ### how many counties/cities in each cluster
VA_k.means.six$centers ### Centroid values for each cluster

## Add back county names
VA_six_clusters <- cbind(VA_cl_data, cluster = VA_k.means.six$cluster)

# Silhouette Plot
## Used to determine how well values fit in their clusters

plot(silhouette(VA_k.means.six$cluster, d))
## If members of the group are similar, Si values will be high
## If members of the group are NOT similar, Si values will be low
## Negative values might indicate the object is in the wrong cluster

## Avg silhouette 0.36 (compared to 0.48 for 2 clusters)
## higher average silhouettes indicate better clustering


# Create a scatterplot to visualize the clusters ====
## With points
ggplot(data = VA_six_clusters, aes(x = AvgVehHH, y = PctDriveAlone, color = as.factor(VA_k.means.six$cluster))) +
  geom_point(size = 4) +
  labs(title = "Virginia City and County Transportaton Clusters",
       x = "Average Vehicles per Household",
       y = "Drive Alone Commuting Percentage",
       legend = "Cluster") +
  scale_colour_discrete(name= "Cluster") +
  coord_cartesian(xlim = c(1.3, 2.7), ylim = c(40, 90)) +
  theme_minimal()

## With jurisdiction names plotted 
VA.6.cluster.plot <- ggplot(data = VA_six_clusters, aes(x = AvgVehHH, y = PctDriveAlone, color = as.factor(VA_k.means.six$cluster))) +
  geom_text(aes(label=NAME)) +
  labs(title = "Virginia City and County Transportaton Clusters",
       x = "Average Vehicles per Household",
       y = "Drive Alone Commuting Percentage",
       legend = "Cluster") +
  scale_colour_discrete(name= "Cluster") + 
  coord_cartesian(xlim = c(1.3, 2.7), ylim = c(40, 90)) +
  theme_minimal()

VA.6.cluster.plot %>% ggsave(filename = here("Figure_output","VA_6_clusters.jpg"),
                          width = 10, 
                          height = 6) ## ggsave only works for objects
