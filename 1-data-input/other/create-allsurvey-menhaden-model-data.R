# create-allsurvey-menhaden-model-data.R
######################################
# Janelle L. Morano
# Compile data from allsurveys (see surveydata repo) into: 
#   - sample data
#   - covariate data
# last updated 20 April 2022
###############################################
###############################################

library(tidyverse)
library(janitor)

###################
# Make Sample data
###################
alldata <- read_csv("/Users/janellemorano/Git/surveydata/allsurveys.csv")

# This includes all surveys. I'm going to start with just grabbing NEFSC and NEAMAP, but I'm doing this here because I can get date data with this dataset (not just season).

sample_data <- alldata %>%
  filter(Survey == "NEFSC" | Survey == "NEAMAP") %>%
  mutate(Month = month(Date)) %>%
  select(Survey, Cruise, Station, Stratum, Year, Month, Season, Latitude, Longitude, Areasw, Abundance, Biomass)

# Write dataset as .csv file
####################
#save this as a new dataset
write.csv(sample_data,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/allsurveymenhaden_sample_data", row.names = TRUE)


######################
# Make Covariate data
######################
# Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
# Go back and grab the covariate data from allsurveys

covariate_data <- alldata %>%
  filter(Survey == "NEFSC" | Survey == "NEAMAP") %>%
  mutate(Month = month(Date)) %>%
  select(Survey, Cruise, Station, Stratum, Year, Month, Season, Latitude, Longitude, Depth, Bottemp)

# Write dataset as .csv file
####################
#save this as a new dataset
write.csv(covariate_data,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/allsurveymenhaden_covariate_data", row.names = TRUE)
