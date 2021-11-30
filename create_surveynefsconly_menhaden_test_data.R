### Create_FULLsurvey_menhaden_test_data.R
### This creates test data for initial VAST model building and exploration using NEFSC bottom trawl data that Kevin Friedland cleaned and shared. Menhaden positive counts are pulled out and saved with covariates AND adds ALL SAMPLE LOCATIONS so there are zero counts for menhaden.
### Janelle L. Morano

### Updated November 30, 2021 to address duplications in Survdat data

############

#Using survdat data (from Kevin Friedland) to create a dataset of NEFSC sample locations and menhanden samples along Northeast US coast

# setup
library (tidyverse)

load("/Users/janellemorano/DATA/NEFSC-Survdat/Survdat_3_2020.RData")
dim(survdat)
#2892498      21
head(survdat)
# unique number of values for each column
sapply(survdat, function(x) length(unique(x)))
# CRUISE6     STATION     STRATUM         TOW       SVSPP    CATCHSEX    SVVESSEL        YEAR      SEASON 
# 109         642         198          30         674           7           5          57           2 
# LAT         LON EST_TOWDATE       DEPTH    SURFTEMP   SURFSALIN     BOTTEMP    BOTSALIN   ABUNDANCE 
# 14632       14859        4470         401        2632        5152        2364        5176        4772 
# BIOMASS      LENGTH      NUMLEN 
# 106690        1715        3850 

min(survdat$YEAR)
# 1963
max(survdat$YEAR)
# 2019

# Get all unique survey locations
survdat2 <- distinct_at(survdat, vars(STATION, STRATUM, TOW), .keep_all=TRUE)
# Now turn those locations into zero catch 
survdat2$ABUNDANCE <- 0
survdat2$BIOMASS <- 0
survdat2$LENGTH <- 0
survdat2$NUMLEN <- 0

# and replace the species id with zero
survdat2$SVSPP <- 0

# Keep this as a dataset for NEFSC survey locations
write.csv(survdat2,"/Users/janellemorano/DATA/NEFSCsurvlocs.csv", row.names = TRUE)

#I know that menhaden is distinguised with #36, so grab all menhaden data
# 36=menhaden
survdat.menhaden <- subset(survdat, SVSPP %in% c(36))
survdat.menhaden$species <- c("menhaden")

#Add species column to survdat2
survdat2$species <- c("sample")
ncol(survdat2)
ncol(survdat.menhaden)
survdat.menhaden <- rbind(survdat2, survdat.menhaden)
#save this as a new dataset
write.csv(survdat.menhaden,"/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/survdat.menhaden.csv", row.names = TRUE)
