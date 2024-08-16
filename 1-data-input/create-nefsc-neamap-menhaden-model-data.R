# create-nefsc-neamap-menhaden-model-data.R
######################################
# Janelle L. Morano
# Compile data from NEFSC and NEAMAP into: 
#   A. "combined-catch-envtl": all variables
#   B. "menhaden-covariate-data" (for VAST): only has temp, salinity, chlorophyll, and depth with the dates and locs
#   C. sample data (for VAST): only has Abundance and Biomass with the dates and locs


# Adds presence/absence column.
# Adds inshore/offshore column designation based on stratum number.
# Adds state column which assigns stratum to nearest state waters.
# Adds strata centroids. 
# Adds in Chlorophyll, NOAA S-NPP VIIRS, Science Quality, Global 4km, Level 3, 2012-present, Monthly. Create another sample and covariate dataset with chlorophyll-a data

# last updated 17 June 2024

###############################################
###############################################

library(tidyverse)
library(janitor)



#----- A. Make "combined-catch-envtl" menhaden survey data ---------------------------------------------------


#----- 1. Bring in NEFSC data

# These are NEFSC bottom trawl data from Survdat output. These data were then cleaned in:
# "Git/surveydata/create-NEFSC-menhaden-data.R"
# These data have menhaden positive catch and absences in NEFSC bottom trawl

# nefsc 1963-2019
# nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.csv") 

# nefsc 1963-2021
# nefsc <- read.csv("/Users/janellemorano/DATA/NEFSC-Survdat/nefsc.menhaden.1963-2021.csv") 
# nefsc 1963-2022
# copy also at "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/nefsc.menhaden.1963-2022.csv"
nefsc <- read.csv("/Volumes/Eurybia/NEFSC-Survdat/nefsc.menhaden.1963-2023.csv") 



# NOTES ABOUT STRATA
# Offshore strata belong to strata group "01" and inshore to "03" (at least north of Hatteras).  If the strata column is numeric, it typically drops the leading zero.  There is also a trailing zero on most strata numbers except for the rare instances where a strata was split.  So offshore strata 1 is 01010 and offshore strata 2 is 01020.  Survdat lists them as 1010 and 1020 respectively.  Inshore would be 03010 and 03020 or 3010 and 3020 in survdat.  The split strata are in the northern Gulf of Maine and are 01351 and 01352 or essentially offshore strata 35-1 and 35-2. 
# Strata groups: 
# 01 Offshore
# 03 Inshore
# 05 Scotian Shelf strata "offshore"
# 07 Inshore South of Hatteras
# 08 Offshore South of Hatteras
# in nefsc.strata$Strata, begins with 1- offshore, begins with 3- inshore
# The next 2 digits are the strata
### HOWEVER, some of strata assignments have erroneous assignments (see details and fixes below)


# Clean up NEFSC data to make it match desired df
nefsc.2 <- nefsc %>%
  select(cruise6, station, stratum, year, season, lat, lon, depth, surftemp, surfsalin, bottemp, botsalin, abundance, biomass) %>%
  rename(cruise = cruise6,
         latitude = lat,
         longitude = lon) %>%
  add_column(Survey = "NEFSC", .before = "cruise") %>%
#### AREA SWEPT = 0.0384 from http://archive.nefmc.org/nemulti/closed%20area%20working%20group/121106/Swept%20Area%20and%20CPUE.pdf and https://github.com/pinskylab/OceanAdapt/issues/27
  add_column(areasw = 0.0384, .after = "longitude") %>%
  # Add numeric presence/absence
  mutate(presence = ifelse(.$abundance >0, 1, 0)) %>%
  # Add inshore/offshore designation
  mutate(inoffshore = case_when(.$stratum <2000 ~ "offshore", 
                                .$stratum >3000 & .$stratum <5000 ~ "inshore",
                                .$stratum >5000 ~ "offshore",
                                .$stratum >=7000 & .$stratum <8000 ~ "inshore",
                                .$stratum >=8000 ~ "offshore"), .after = "stratum") %>%
  # Add state designation. Strata are assigned to the closest state waters. RI-CT-NY are collectively assigned because of the overlap of the strata with the span of the states and the location of the mouth of Long Island Sound.
  mutate(state = case_when(.$stratum >=3570 & .$stratum <=3990 ~ "GME", # Inshore GME (north of 42 lat) 
                           .$stratum >=1260 & .$stratum <=1420 ~ "GME", # Offshore GME
                           .$stratum ==1490 ~ "GME", # Offshore GME
                           .$stratum >=5430 & .$stratum <=5480 ~ "GME", # Offshore GME
                           .$stratum ==3460 ~ "MA", #Inshore MA
                           .$stratum >=3480 & .$stratum <=3560 ~ "MA", #Inshore MA
                           .$stratum >=1090 & .$stratum <=1250  ~ "MA", #Offshore MA (south of 42 lat)
                           .$stratum >=3010 & .$stratum <=3140 ~ "RICTNY", #Inshore RICTNY
                           .$stratum ==3450 ~ "RICTNY", #Inshore RICTNY
                           .$stratum ==3470 ~ "RICTNY", #Inshore RICTNY
                           .$stratum ==3490 ~ "RICTNY", #Inshore RICTNY
                           .$stratum ==3910 ~ "RICTNY", #Inshore RICTNY
                           .$stratum >=1010 & .$stratum <=1140 ~ "RICTNY", #Offshore RICTNY
                           .$stratum >=3150 & .$stratum <=3230 ~ "NJ", #Inshore NJ                           
                           .$stratum >=1730 & .$stratum <=1760 ~ "NJ", #Offshore NJ
                           .$stratum >=3240 & .$stratum <=3290 ~ "DEMD", #Inshore DEMD
                           .$stratum >=1690 & .$stratum <=1720 ~ "DEMD", #Offshore DEMD
                           .$stratum >=3300 & .$stratum <=3380 ~ "VA", #Inshore VA
                           .$stratum >=1650 & .$stratum <=1680 ~ "VA", #Offshore VA
                           .$stratum >=3390 & .$stratum <=3580 ~ "NC", #Inshore NC and south
                           .$stratum >=1610 & .$stratum <=1640 ~ "NC", #Offshore NC and south
                           .$stratum >7000 ~ "NC", #Offshore NC and south (includes confirmed strata and numbers including 7000+ and 8000+ that don't directly correspond to maps, but follow convention described above)
                           
                           .$stratum >7000 & .$latitude >36 ~ "GME",
                           .$stratum == 1990 & .$latitude >36 ~ "GME",
                           .$stratum == 1990 & .$latitude <36 ~ "NC"), .after = "inoffshore") %>% #1990, 7940, 7980 have to be an error assignment for some because it spans different lats
  janitor::clean_names(., "lower_camel")
nefsc.2$cruise <- as.character(nefsc.2$cruise)
nefsc.2$station <- as.character(nefsc.2$station)
nefsc.2$stratum <- as.character(nefsc.2$stratum)

# Verify state assignments
ggplot(data = nefsc.2, aes(longitude, latitude)) +
  geom_point(aes(color = factor(state)))
# There are a couple of outliers, so identify them and drop them
plot(nefsc.2$longitude, nefsc.2$latitude)
# identify(nefsc.2$longitude, nefsc.2$latitude, labels = row.names(nefsc.2))
# 89 7009 7038
# Remove those
nefsc.2 <- nefsc.2[-c(89, 7009, 7038),]
plot(nefsc.2$longitude, nefsc.2$latitude)


#----- 1b. Add centroids of strata to NEFSC

library(sf)
sf_use_s2(FALSE)

# Bring in NEFSC shapefile and extract centroids
# BTS_Strata.shp: CENTROIDS from these data are CORRECT
nefsc.strata <- st_read("/Volumes/Eurybia/NEFSC strata/BTS_Strata.shp")
# get the geometry only
nefsc.geo <- st_geometry(nefsc.strata)
plot(nefsc.geo)
plot(st_centroid(nefsc.geo), add = TRUE, pch = 3, col = 'red')

# Get the centroids and put them in a dataframe with Strata label and keep
nefsc.cent <- data.frame(st_centroid(nefsc.geo))
nefsc.cent2 <- cbind(nefsc.strata$STRATA, nefsc.cent)
nefsc.centroids <- data.frame(cbind(nefsc.strata$STRATA, st_coordinates(st_cast(nefsc.cent2$geometry,"MULTIPOINT"))))
nefsc.centroids <- nefsc.centroids %>%
  select(-c(L1)) %>%
  rename(stratum = V1,
         centroid.lat = X,
         centroid.lon = Y)
# Write to keep centroids for mapping elsewhere
# write.csv(nefsc.centroids,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/nefsc-strata-centroids.csv", row.names = TRUE)

# Put centroids back into nefsc.2
nefsc.centroids$stratum <- as.character(nefsc.centroids$stratum)
nefsc.2 <- nefsc.2 %>%
  left_join(nefsc.centroids, 
          by = c("stratum"),relationship = "many-to-many")


#----- 2. Add NEAMAP data to dataframe

# neamap <- read.csv("/Volumes/Eurybia/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
neamap <- read.csv("/Volumes/Eurybia/NEAMAP/NEAMAP_AtlanticMenhaden_Catch_2007-2023.csv", header = TRUE)
colnames(neamap)
# [1] "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# [9] "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# [17] "vimscode"  "count"     "weight"   


# Modify region identification columns
# For unknown reasons, this next step only works here, not in the select, rename, add step
neamap <- neamap %>%
  mutate(inoffshore = case_when(.$dstrat == 1 ~ "inshore",
                              .$dstrat == 2 ~ "offshore",
                              .$dstrat == 3 ~ "offshore",
                              .$dstrat == 4 ~ "offshore"), .after = "region") %>%
  mutate(region = case_when(.$region == 1 ~ "01",
                            .$region == 2 ~ "02",
                            .$region == 3 ~ "03",
                            .$region == 4 ~ "04",
                            .$region == 5 ~ "05",
                            .$region == 6 ~ "06",
                            .$region == 7 ~ "07",
                            .$region == 8 ~ "08",
                            .$region == 9 ~ "09",
                            .$region == 10 ~ "10",
                            .$region == 11 ~ "11",
                            .$region == 12 ~ "12",
                            .$region == 13 ~ "13",
                            .$region == 14 ~ "14",
                            .$region == 15 ~ "15",
                            .$region == "BI" ~ "BI",
                            .$region == "RI" ~ "RI")) %>%
  mutate(state = case_when(.$region == 1 ~ "RICTNY",
                           .$region == 2 ~ "RICTNY",
                           .$region == 3 ~ "RICTNY",
                           .$region == 4 ~ "RICTNY",
                           .$region == 5 ~ "RICTNY",
                           .$region == 6 ~ "NJ",
                           .$region == 7 ~ "NJ",
                           .$region == 8 ~ "NJ",
                           .$region == 9 ~ "DEMD",
                           .$region == 10 ~ "DEMD",
                           .$region == 11 ~ "VA",
                           .$region == 12 ~ "VA",
                           .$region == 13 ~ "VA",
                           .$region == 14 ~ "NC",
                           .$region == 15 ~ "NC",
                           .$region == "BI" ~ "RICTNY",
                           .$region == "RI" ~ "RICTNY"))
# Create new stratum category that combines region and depthzone
neamap <- unite(neamap, stratum, c("region","dstrat"), sep = "0", remove = FALSE)


neamap.2 <- neamap %>%
  select(cruise, station, stratum, inoffshore, state, year, season, latitude, longitude, areasw, depth, WT, SA, count, weight) %>%
  rename(Bottemp = WT,
         Botsalin = SA,
         abundance = count,
         biomass = weight) %>%
  # Add numeric presence/absence
  mutate(presence = ifelse(.$abundance >0, 1, 0)) %>%
  janitor::clean_names(., "lower_camel") %>%
  add_column(survey = "NEAMAP", .before = "cruise") %>%
  add_column(surftemp = NA, .after = "depth") %>%
  add_column(surfsalin = NA, .before = "bottemp")
neamap.2$cruise <- as.character(neamap.2$cruise)
neamap.2$station <- as.character(neamap.2$station)
neamap.2$stratum <- as.character(neamap.2$stratum)



#----- 2b. Bring in NEAMAP shapefile and extract centroids

neamap.strata <- st_read("/Volumes/Eurybia/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")

# Get the centroids and put them in a dataframe with Region label and keep
neamap.strata.geo <- st_geometry(neamap.strata)
plot(neamap.strata.geo)
plot(st_centroid(neamap.strata.geo), add = TRUE, pch = 3, col = 'red')

neamap.strata.centroids <- st_centroid(neamap.strata.geo)
neamap.strata.centroids <- cbind(neamap.strata$Index, st_coordinates(neamap.strata.centroids))
neamap.strata.centroids <- data.frame(neamap.strata.centroids)
neamap.strata.centroids <- neamap.strata.centroids %>%
  rename(stratum = V1,
         centroid.lat = X,
         centroid.lon = Y)
neamap.strata.centroids$centroid.lat <- as.numeric(neamap.strata.centroids$centroid.lat)
neamap.strata.centroids$centroid.lon <- as.numeric(neamap.strata.centroids$centroid.lon)
# Write to keep centroids for mapping elsewhere
# write.csv(neamap.strata.centroids,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/neamap-strata-centroids.csv", row.names = TRUE)

# Put centroids back into neamap.2
neamap.2 <- neamap.2 %>%
  left_join(neamap.strata.centroids, 
            by = c("stratum"),relationship = "many-to-many")

# Add "N" at end of stratum NEAMAP region to distinguish from NEFSC strata
neamap.2$stratum <- paste0(neamap.2$stratum, "N")




#----- 3. Bind NEFSC and NEAMAP into alldata which has NEFSC and NEAMAP in one

alldata <- bind_rows(nefsc.2, neamap.2)
alldata <- clean_names(alldata, "upper_camel")




#----- 4. Add Chlorophyll data

# Spring chlorophyll data
chl.sp <- read.csv("/Volumes/Eurybia/Chlorophyll-a/chlorophyll-NOAA-S-NPPVIIRS-USeastcoast-2012-2021-spring-monthly-ave.csv", header = TRUE)
# Fall chlorophyll data
chl.fa <- read.csv("/Volumes/Eurybia/Chlorophyll-a/chlorophyll-NOAA-S-NPPVIIRS-USeastcoast-2012-2021-fall-monthly-ave.csv", header = TRUE)

# Round Lat/Lon for matching Avechlor to within 10 km of covariate data
library(janitor)
chl.sp <- clean_names(chl.sp, case = "upper_camel")
chl.fa <- clean_names(chl.fa, case = "upper_camel")

chl.sp2 <- chl.sp %>%
  mutate(LatCat = Latitude) %>%
  mutate(LonCat = Longitude) %>%
  mutate(across(c("LatCat", "LonCat"), round, digits = 1))
chl.fa2 <- chl.fa %>%
  mutate(LatCat = Latitude) %>%
  mutate(LonCat = Longitude) %>%
  mutate(across(c("LatCat", "LonCat"), round, 1))

# Chlorophyll available only for 2013-2021, so create subset of alldata
alldata.2 <- alldata %>%
  filter(Year >= 2013) %>%
  # Duplicate Lat/Lon and round to match with chl
  mutate(LatCat = Latitude) %>%
  mutate(LonCat = Longitude) %>%
  mutate(across(c("LatCat", "LonCat"), round, 1))

# Match coordinates in chl and covariate_data
# For spring,
alldata.SP <- alldata.2 %>%
  filter(Season == "SPRING") %>%
  left_join(chl.sp2,
            by = c("LatCat", "LonCat", "Year"), relationship = "many-to-many")
# For fall,
alldata.FA <- alldata.2 %>%
  filter(Season == "FALL") %>%
  left_join(chl.fa2, 
            select("Avechlor"),
            by = c("LatCat", "LonCat", "Year"),relationship = "many-to-many")

# Above creates a lot of duplicates, and I can't figure out how to streamline it here, so I'll do it next.

# Bind Spring and Fall covariate chlorophyll data
alldata.CHL <- bind_rows(alldata.SP, alldata.FA)
alldata.CHL <- alldata.CHL %>% 
  select(-c(X, Latitude.y, Longitude.y)) %>%
  rename(Latitude = Latitude.x,
         Longitude = Longitude.x)

dups <- alldata.CHL %>%
  janitor::get_dupes(-c(Avechlor))
# Yup, lots
# Drop them
alldata.CHL <- distinct(alldata.CHL, Survey, Cruise, Station, Stratum, Inoffshore, State, Year, Season, Latitude, Longitude, Areasw, Depth, Abundance, Surftemp, Surfsalin, Bottemp, Botsalin, Abundance, Biomass, Presence, CentroidLat, CentroidLon, LatCat, LonCat, .keep_all = TRUE) 

# Quick check chlorophyll data
plot(alldata.CHL$Year, alldata.CHL$Avechlor)

# Add back years before 2012 to re-create alldata with chlorophyll...
# First prep alldata with additional columns and drop years >2012
alldata.b2012 <- alldata %>%
  mutate(LatCat = Latitude) %>%
  mutate(LonCat = Longitude) %>%
  add_column(Avechlor = NA, .after = "LonCat") %>%
  mutate(across(c("LatCat", "LonCat"), round, 0)) %>%
  filter(Year <= 2012)
# Then bind alldata.3 with alldata.CHL for a complete set
alldata.complete <- bind_rows(alldata.b2012, alldata.CHL) 
alldata.complete <- arrange(alldata.complete, Year, Season, Latitude)

# Verify if there are duplicates
dups <- alldata.complete %>%
  janitor::get_dupes(-c(Avechlor))
                       
                       


#-----  5. Write dataset as .csv file

#save this as a new dataset. Amend the date to update
# THIS OVERWRITES EXISTING FILE!!
# write.csv(alldata.complete,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240617.csv", row.names = TRUE)




#----- B. Make "menhaden-covariate-data" for VAST ---------------------------------------------------------

# For VAST, Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
# Go back and grab the covariate data from alldata.complete
colnames(alldata.complete)

covariatedata <- alldata.complete %>%
  select(Survey, Cruise, Station, Stratum, Inoffshore, Year, Season, Latitude, Longitude, Areasw, Depth, Surftemp, Surfsalin, Bottemp, Botsalin, Avechlor) %>%
  rename(Lat = Latitude,
         Lon = Longitude)

# THIS OVERWRITES EXISTING FILE!! Amend the date to update
# write.csv(covariatedata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/menhaden-covariate-data-20240617.csv", row.names = TRUE)




#----- C. Make "menhaden-sample-data" for VAST ---------------------------------------------------------
# For VAST, abundance and biomass must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
# Go back and grab the abundance and biomasss data from alldata.complete
colnames(alldata.complete)

sampledata <- alldata.complete %>%
  select(Survey, Cruise, Station, Stratum, Inoffshore, Year, Season, Latitude, Longitude, Abundance, Biomass) %>%
  rename(Lat = Latitude,
         Lon = Longitude)

# THIS OVERWRITES EXISTING FILE!! Amend the date to update
# write.csv(sampledata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/menhaden-sample-data-20240617.csv", row.names = TRUE)
