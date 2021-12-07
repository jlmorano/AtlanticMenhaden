# create-NEFSC-menhaden-data.R
######################################
# Janelle L. Morano
# This takes the copy of Survdat I have (currently from Kevin Friedland)
# and cleans it to include just menhaden samples, 
# specifically removing duplications resulting from catch-size rows
# last updated 12/6/2021
###############################################
###############################################

library(tidyverse)
library(janitor)


# NEFSC data
###############
# These are NEFSC bottom trawl data from Survdat output that I got from Kevin Friedland, but those data duplicate biomass and abundance by row for every length

load("/Users/janellemorano/DATA/NEFSC-Survdat/Survdat_3_2020.RData")
# NUMLEN is the number of fish at each LENGTH at the lat/lon sampled, but the ABUNDANCE and BIOMASS is the sum of all fish (of that species), so ABUNDANCE and BIOMASS values are repeated for each LENGTH.
# Therefore, I can drop LENGTH and NUMLEN and then pull only one row at each lat/long for each species.

# Get rid of those ugly uppercase col names
survdat <- clean_names(survdat)

# Drop LENGTH and NUMLEN
nefsc <- survdat %>%
  select(-c(length, numlen)) 

# Drop duplicates
nefsc <- nefsc %>%
  distinct() 
# This should now be abundance and biomass for each species at each sample station

###################
# Get Menhaden Positive Catches
###################
# I know that menhaden is distinguished SVSPP == 36, so grab all menhaden data
# 36=menhaden
nefsc.menhaden <- subset(nefsc, svspp %in% c(36))
nefsc.menhaden$species <- c("menhaden")

###################
# Get All Survey Sites
###################
# Go back and create the unique sample sites from the Survdat
# create a copy to mess with to verify
nefsc.samples <- survdat
# Replace the species id with zero because it doesn't matter what species caught, making them more duplicates
nefsc.samples$svspp <- 0
# Get all unique survey locations
nefsc.samples <- distinct_at(nefsc.samples, vars(cruise6, station, stratum, tow, svvessel, year, season, lat, lon, est_towdate, depth), .keep_all=TRUE)
nefsc.samples <- select(nefsc.samples, -c(length, numlen)) 
# Now turn those locations into zero catch 
nefsc.samples$abundance <- 0
nefsc.samples$biomass <- 0
# Add species column to survdat2
nefsc.samples$species <- c("sample")

# Now bind these 2 into 1 df, but put menhaden catch first because of duplication clean up step to follow.
dim(nefsc.menhaden)
dim(nefsc.samples)
combined <- rbind(nefsc.menhaden,nefsc.samples)

# Remove "samples" where there are menhaden catch, leaving positive catches and absences
# Here, the order in the bind above is important because it will only keep the first instance, which means the menhaden catch here.
combined2 <- distinct(combined, cruise6, station, stratum, tow, svvessel, year, season, lat, lon, est_towdate, depth, .keep_all = TRUE) 
# verify I have menhaden presence
plot(combined$year, combined$abundance)
plot(combined2$year, combined2$abundance)

# Keep this as a dataset for NEFSC survey locations
write.csv(nefsc.samples,"/Users/janellemorano/DATA/NEFSC-Survdat/NEFSCsurvlocs_DecUpdated.csv", row.names = TRUE)

# Save the menhaden positive catch and absences as a new dataset
write.csv(combined2,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/nefsc.menhaden.csv", row.names = TRUE)
