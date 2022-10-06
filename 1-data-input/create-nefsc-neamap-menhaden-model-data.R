# create-nefsc-neamap-menhaden-model-data.R
######################################
# Janelle L. Morano
# Compile data from NEFSC and NEAMAP into: 
#   - sample data
#   - covariate data
# last updated 9/28/2022
# These data are updated to include NEFSC data from 2022
###############################################
###############################################

library(tidyverse)
library(janitor)

###################
# Make Sample data
###################

# NEFSC data
#############
# These are NEFSC bottom trawl data from Survdat output. These data were then cleaned in:
# "Git/surveydata/create-NEFSC-menhaden-data.R"
# These data have menhaden positive catch and absences in NEFSC bottom trawl

# nefsc 1963-2019
# nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.csv") 

# nefsc 1963-2021
# nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.1963-2021.csv") 
# nefsc 1963-2022
# copy also at "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/nefsc.menhaden.1963-2022.csv"
nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.1963-2022.csv") 

# Clean up NEFSC data to make it match desired df
nefsc.2 <- nefsc %>%
  select(cruise6, station, stratum, year, season, lat, lon, abundance, biomass) %>%
  rename(cruise = cruise6,
         latitude = lat,
         longitude = lon) %>%
  add_column(Survey = "NEFSC", .before = "cruise") %>%
#### AREA SWEPT = 0.0384 from http://archive.nefmc.org/nemulti/closed%20area%20working%20group/121106/Swept%20Area%20and%20CPUE.pdf and https://github.com/pinskylab/OceanAdapt/issues/27
  add_column(areasw = 0.0384, .before = "abundance")
nefsc.2$cruise <- as.character(nefsc.2$cruise)
nefsc.2$station <- as.character(nefsc.2$station)
nefsc.2$stratum <- as.character(nefsc.2$stratum)

# Add NEAMAP data to dataframe
#################
neamap <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
colnames(neamap)
# [1] "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# [9] "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# [17] "vimscode"  "count"     "weight"   
neamap.2 <- neamap %>%
  select(cruise, station, region, year, season, latitude, longitude, areasw, count, weight) %>%
  rename(stratum = region,
         abundance = count,
         biomass = weight) %>%
  add_column(Survey = "NEAMAP", .before = "cruise")
neamap.2$cruise <- as.character(neamap.2$cruise)
neamap.2$station <- as.character(neamap.2$station)
neamap.2$stratum <- as.character(neamap.2$stratum)

# bind into biodata
biodata <- bind_rows(nefsc.2, neamap.2)
biodata <- clean_names(biodata, "upper_camel")

# verify it looks ok
plot(biodata$Longitude, biodata$Latitude)
plot(biodata$Year, biodata$Abundance)
plot(biodata$Year, biodata$Biomass)

# Write dataset as .csv file
####################
#save this as a new dataset
# THIS OVERWRITES EXISTING FILE!!
write.csv(biodata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", row.names = TRUE)

######################
# Make Covariate data
######################
# Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
# Go back and grab the covariate data from nefsc and neamap data originally read in
colnames(nefsc)

covariate_data <- nefsc %>%
  select(cruise6, station, stratum, year, season, lat, lon, depth, surftemp, surfsalin, bottemp, botsalin) %>%
  rename(Cruise = cruise6,
         Latitude = lat,
         Longitude = lon) %>%
  janitor::clean_names(., "upper_camel") %>%
  add_column(Survey = "NEFSC", .before = "Cruise")
covariate_data$Cruise <- as.character(covariate_data$Cruise)
covariate_data$Station <- as.character(covariate_data$Station)
covariate_data$Stratum <- as.character(covariate_data$Stratum)

colnames(neamap)
cv.neamap <- neamap %>%
  select(cruise, station, region, year, season, latitude, longitude, depth, WT, SA) %>%
  rename(Stratum = region,
         #missing Surftemp
         #missing Surfsalin
         Bottemp = WT,
         Botsalin = SA) %>%
  janitor::clean_names(., "upper_camel") %>%
  add_column(Survey = "NEAMAP", .before = "Cruise") %>%
  add_column(Surftemp = NA, .after = "Depth") %>%
  add_column(Surfsalin = NA, .before = "Bottemp")
cv.neamap$Cruise <- as.character(cv.neamap$Cruise)
cv.neamap$Station <- as.character(cv.neamap$Station)
cv.neamap$Stratum <- as.character(cv.neamap$Stratum)

# Verify columns are the same
colnames(covariate_data)
colnames(cv.neamap)

# Bind NEFSC and NEAMAP covariate data
covariate_data <- bind_rows(covariate_data, cv.neamap)
# Verify both surveys are there
unique(covariate_data$Survey)

# Write dataset as .csv file
####################
#save this as a new dataset
# THIS OVERWRITES EXISTING FILE!!
write.csv(covariate_data,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", row.names = TRUE)


###################
# Make Complete dataset
###################
# This is all data together for correlations and other data analyses.

# NEFSC data
#############
nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.1963-2022.csv")

# Grab catch and envt'l data
corr.nefsc <- nefsc %>%
  select(stratum, year, season, lat, lon, depth, surftemp, surfsalin, bottemp, botsalin, abundance, biomass) %>%
  add_column(Survey = "NEFSC", .before = "stratum")

corr.nefsc$stratum <- as.character(corr.nefsc$stratum)

# Add NEAMAP data to dataframe
#################
neamap <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
colnames(neamap)
# [1] "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# [9] "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# [17] "vimscode"  "count"     "weight"   
corr.neamap <- neamap %>%
  select(region, year, season, latitude, longitude, depth, WT, SA, count, weight) %>%
  rename(stratum = region,
         lat = latitude,
         lon = longitude,
         bottemp = WT,
         botsalin = SA,
         abundance = count,
         biomass = weight) %>%
  add_column(Survey = "NEAMAP", .before = "stratum") %>%
  add_column(surftemp = NA, .after = "depth") %>%
  add_column(surfsalin = NA, .before = "bottemp")

# verify they match
colnames(corr.nefsc)
colnames(corr.neamap)
str(corr.nefsc)
str(corr.neamap)

# bind into biodata
corrdata <- bind_rows(corr.nefsc, corr.neamap)
corrdata <- clean_names(corrdata, "upper_camel")

# verify it looks ok
plot(corrdata$Lon, corrdata$Lat)
plot(corrdata$Year, corrdata$Abundance)
plot(corrdata$Year, corrdata$Biomass)

# Write dataset as .csv file
####################
#save this as a new dataset
# THIS OVERWRITES EXISTING FILE!!
write.csv(corrdata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl.csv", row.names = TRUE)
