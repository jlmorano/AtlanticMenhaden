# NYBight_data_forChristina.R
######################################
# Janelle L. Morano
# Summary of surveys in NY Bight and wind lease areas
#   NY Bight= Cape May, NY to Montauk Point, NY; latitude 38.9297 - 41.07112
# Includes: NEFSC and NEAMAP data


# last updated 25 October 2022
###############################################
###############################################

# Table of Contents
## Data Setup
## Map of survey area and wind lease areas
## Abundance v. Year
## Abundance v. Temp

############################
#### Data Setup
############################
library(dplyr)

#### Full data are subset for NYBight prior to sending to Christina
# # Keeping here for documentation
# surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl.csv", header = TRUE)
# # Remove salinity cols because not working with now
# surveydata <- select(surveydata, Survey, Stratum, Year, Season, Lat, Lon, Depth, Bottemp, Abundance, Biomass)
# # filter NYBight lat 
# surveydata <- filter(surveydata, Lat >= 38.9297 & Lat <= 41.07112) 
# # remove NAs
# sapply(surveydata, function(x) sum(is.na(x)))
# surveydata <- na.omit(surveydata)
# 
# # Write dataset to send to Christina
# write.csv(surveydata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/NYBight_data_forChristina.csv", row.names = FALSE)

# Use the dataset sent to Christina
# Change the pathname to where data are saved on your computer
surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/NYBight_data_forChristina.csv", header = TRUE)


############################
#### Map of survey data
############################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")  

strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
nmp.strata <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
wind <- st_read("/Users/janellemorano/DATA/BOEM-Renewable-Energy-Shapefiles_10_2022/BOEMWindPlanningAreas_10_21_22.shp")

# Map of NY Bight with NEFSC, NEAMAP, and wind lease areas
# Not pretty, but a quick and dirty one
ggplot(data = world) +  
  geom_sf(data = strata, color = "black", fill = "#1f78b4") + 
  geom_sf(data = nmp.strata, color = "#b2df8a", fill = "#b2df8a") + 
  geom_sf(data = wind, color = "red") + 
  theme_classic() +
  coord_sf(xlim = c(-77, -70), ylim = c(38, 41), expand = FALSE)


############################
#### log(Abundance) vs Temp
############################
ggplot(surveydata, aes(x=Bottemp, y=log(Abundance +1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  labs(x= "Bottom Temperature (°C)", y = "log(Abundance +1)") 


############################
#### log(Abundance) v. Year
############################
# Spring & Fall on same plot
ggplot(surveydata, aes(x=Year, y=log(Abundance +1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  labs(x= " ", y = "log(Abundance + 1)") 
