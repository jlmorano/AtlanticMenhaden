# Spatial-bystrata.R
######################################
# Janelle L. Morano

# Objectives:
# Figure to show average abundance by strata

# last updated 26 April 2023
###############################################
###############################################

#----- Table of Contents -------------------------------------------------

# 1. Setup data
# 2. NMFS/NEAMAP by strata
# 3. State by survey area



library(tidyverse)



#----- 1. Setup NMFS & NEAMAP Survey Data -------------------------------------------------

surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230419.csv", header = TRUE)
# Remove salinity cols and X because not working with now
surveydata <- surveydata %>% select(-c(X, Surfsalin, Botsalin))

#----- Create Spring and Fall datasets
## surveydata
surveydata.spring <- surveydata[surveydata$Season == "SPRING",]
surveydata.fall <- surveydata[surveydata$Season == "FALL",]

#----- Create 1972-2021 dataset
surveydata.spring <- filter(surveydata.spring, Year >=1972)
surveydata.fall <- filter(surveydata.fall, Year >= 1972)

#----- Create 2012-2021 that has chlorophyll
surveydata.chl.spring <- filter(surveydata.spring, Year >=2012)
surveydata.chl.fall <- filter(surveydata.fall, Year >= 2012)
