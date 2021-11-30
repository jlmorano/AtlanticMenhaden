# Atlantic_menhaden_model_data.R
######################################
# Janelle L. Morano
# Compile data from NEFSC and NEAMAP into: 
#   - sample data
#   - covariate data
# last updated 10/1/2021

###############################################
###############################################

library (tidyverse)

###############
# Sample data
###############

# NEFSC data
#############
# These are NEFSC bottom trawl data 
# still have questions about are all individuals weighed and sexed and measured length?

fall_twl_svbio <- read_csv("/Users/janellemorano/DATA/NEFSC_trawl/Fall_trawl/22560_FSCSTables/22560_UNION_FSCS_SVBIO.csv")
# SVSPP for menhaden is 036
menhaden <-  filter(fall_twl_svbio, SVSPP == '036')

biodata <- NEFSC %>%
  select(CRUISE6, STATION, STRATUM, YEAR, SEASON, LAT, LON, ABUNDANCE, BIOMASS) %>%
  rename(Cruise = CRUISE6,
         Station = STATION,
         Stratum = STRATUM,
         Year = YEAR,
         Season = SEASON,
         Latitude = LAT,
         Longitude = LON,
         Count = ABUNDANCE,
         Weight = BIOMASS) %>%
  add_column(Survey = "NEFSC", .before = "Cruise") %>%
  add_column(Areasw = 0.01, .before = "Count")
biodata$Cruise <- as.character(biodata$Cruise)
biodata$Station <- as.character(biodata$Station)
biodata$Stratum <- as.character(biodata$Stratum)

# Add NEAMAP data to dataframe
#################
NEAMAP <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
colnames(NEAMAP)
# [1] "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# [9] "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# [17] "vimscode"  "count"     "weight"   
NEAMAP.2 <- NEAMAP %>%
  select(cruise, station, region, year, season, latitude, longitude, areasw, count, weight) %>%
  rename(Cruise = cruise,
         Station = station,
         Stratum = region,
         Year = year,
         Season = season,
         Latitude = latitude,
         Longitude = longitude,
         Areasw = areasw,
         Count = count,
         Weight = weight) %>%
  add_column(Survey = "NEAMAP", .before = "Cruise")
NEAMAP.2$Cruise <- as.character(NEAMAP.2$Cruise)
NEAMAP.2$Station <- as.character(NEAMAP.2$Station)
NEAMAP.2$Stratum <- as.character(NEAMAP.2$Stratum)

# bind to biodata
biodata <- bind_rows(biodata, NEAMAP.2)

# verify it looks ok
plot(biodata$Longitude, biodata$Latitude)
plot(biodata$Year, biodata$Count)
plot(biodata$Year, biodata$Weight)

# Write dataset as .csv file
####################
#save this as a new dataset
write.csv(biodata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", row.names = TRUE)

###############
# Covariate data
###############
# Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
# Go back and grab the covariate data from NEFSC and NEAMAP
colnames(NEFSC)

covariate_data <- NEFSC %>%
  select(CRUISE6, STATION, STRATUM, YEAR, SEASON, LAT, LON, DEPTH, SURFTEMP, SURFSALIN, BOTTEMP, BOTSALIN) %>%
  rename(Cruise = CRUISE6,
         Station = STATION,
         Stratum = STRATUM,
         Year = YEAR,
         Season = SEASON,
         Lat = LAT,
         Lon = LON,
         Depth = DEPTH,
         Surftemp = SURFTEMP,
         Surfsalin = SURFSALIN,
         Bottemp = BOTTEMP,
         Botsalin = BOTSALIN) %>%
  add_column(Survey = "NEFSC", .before = "Cruise")
covariate_data$Cruise <- as.character(covariate_data$Cruise)
covariate_data$Station <- as.character(covariate_data$Station)
covariate_data$Stratum <- as.character(covariate_data$Stratum)

colnames(NEAMAP)
cv.NEAMAP <- NEAMAP %>%
  select(cruise, station, region, year, season, latitude, longitude, depth, WT, SA) %>%
  rename(Cruise = cruise,
         Station = station,
         Stratum = region,
         Year = year,
         Season = season,
         Lat = latitude,
         Lon = longitude,
         Depth = depth,
         #missing Surftemp
         #missinn Surfsalin
         Bottemp = WT,
         Botsalin = SA) %>%
  add_column(Survey = "NEAMAP", .before = "Cruise") %>%
  add_column(Surftemp = NA, .after = "Depth") %>%
  add_column(Surfsalin = NA, .before = "Bottemp")
cv.NEAMAP$Cruise <- as.character(cv.NEAMAP$Cruise)
cv.NEAMAP$Station <- as.character(cv.NEAMAP$Station)
cv.NEAMAP$Stratum <- as.character(cv.NEAMAP$Stratum)

#Bind NEFSC and NEAMAP covariate data
covariate_data <- bind_rows(covariate_data, cv.NEAMAP)
unique(covariate_data$Survey)

# Write dataset as .csv file
####################
#save this as a new dataset
write.csv(covariate_data,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", row.names = TRUE)
