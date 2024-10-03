# UAP 5494 - Week 8 ----

# Get set up
rm(list = ls()) ## clears the Environment window

install.packages(c("tidytransit", "sf")) 

library(tidyverse)
library(here)
library(tidycensus)
library(mapview)
library(tidytransit)
library(sf)

 # Data ----

  # Census Data ====
 v17 <- load_variables(2022,"acs5", cache = TRUE)
 View(v17)
 
 Jurisdictions28A <- c("Fairfax county", "Arlington county", "Alexandria city","Falls Church city")
 
 VA_language <- get_acs(
   geography = "tract",
   variables = c(Spanish = "C16001_002",
                 French = "C16001_003",
                 German = "C16001_006",
                 Russian = "C16001_009",
                 OtherIndoEuro = "C16001_012",
                 Korean = "C16001_015",
                 Chinese = "C16001_018",
                 Vietnamese = "C16001_021",
                 Tagalog = "C16001_024",
                 OtherAPI = "C16001_027",
                 Arabic = "C16001_030",
                 OtherLang = "C16001_033"),
   year = 2022,
   state = "VA",
   county = Jurisdictions28A,
   geometry = TRUE,
   output = "wide"
 )
 
 mapview(VA_language,
         zcol = "VietnameseE")

 #Keep estimates
 VA_language <- VA_language %>%
                select(SpanishE,
                 FrenchE,
                 GermanE,
                 RussianE,
                 OtherIndoEuroE,
                 KoreanE,
                 ChineseE,
                 VietnameseE,
                 TagalogE,
                 OtherAPIE,
                 ArabicE,
                 OtherLangE) 
 
 ## Checking census crs
 st_crs(VA_language)
 
 # GTFS Data ====
 # Load GTFS files 
 WMATA_GTFS <- read_gtfs(here("Data","WMATA-2024-GTFS.zip")) 
 
 ## Filter stops for a common service ID (2) for route 28A
 Route_28A_Stops <- filter_stops(WMATA_GTFS, "2", "28A")
 
 ## Make the stops spatial objects 
 Route_28A_Points <- stops_as_sf(Route_28A_Stops,crs = 4269) ## This is the crs of census data
 
 mapview(Route_28A_Points,
         zcol = "stop_name")
 
 ## Add 1/2 mile buffer around the stops
 Route28A_buffer <- st_buffer(Route_28A_Points, 800)
 
 ## Dissolve buffers into one geographic unit
 Route28A_dissolved <- st_union(Route28A_buffer)
 
 
 sf_use_s2(FALSE)
 
 # Areal Weighting Interpolation ----
 Languages_28A <- st_interpolate_aw(
   VA_language, 
   Route28A_dissolved,
   extensive = TRUE ###  if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved
 )
 
 mapview(VA_language,
         zcol = "SpanishE") +
   mapview(Languages_28A)  
 
 ## Remove geometry
 Languages_28A_Table <- st_drop_geometry(Languages_28A)
 
 ## transpose data
 Languages_28A_Table <- as.data.frame(t(Languages_28A_Table))
 

 
 
 