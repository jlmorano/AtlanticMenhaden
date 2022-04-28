# Atlantic-menhaden-distribution-model-20220420.R
###############################################
# Janelle L. Morano
#
# Menhaden distribution model in VAST 3.8.2 
# Data:  NEFSC and NEAMAP created from "allsurvey" data in "create-allsurvey-menhaden-model-data.R"/Users/janellemorano/Git/AtlanticMenhaden/data/create-allsurvey-menhaden-model-data.R"
# See also surveydata repo for more info
#
# last updated 20 April 2022
###############################################
###############################################


# Model Description
# (should go here)
####################


# Setup & Libraries
####################
# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

# Working directory needs to have the extrapolation grid (User_Region.rds)
# Currently, this extrapolation grid is inclusive of the entire NEAMAP and NEFSC survey site, which should be ok, because NEAMAP covers regions that were not sampled by NEFSC in some years, but need to verify this is OK.
# Previous runs that produce Kmeans_knots and Kmeans_extrapolation should be kept in the directory for faster processing (but only if the extrapolation grid has not changed!)
setwd("/Users/janellemorano/MODEL_OUTPUT/loop_run")

library(VAST)
sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 10.16
# FishStatsUtils_2.10.2 units_0.7-2           
# VAST_3.8.2           
# TMB_1.7.22   
# Matrix_1.3-4
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects


# Data Prep
####################

# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/allsurveymenhaden_sample_data", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/allsurveymenhaden_covariate_data", header = TRUE)

# Latitude and Longitude must be labeled at Lat and Lon
data$Lat <- data$Latitude
data$Lon <- data$Longitude
covariate_data$Lat <- covariate_data$Latitude
covariate_data$Lon <- covariate_data$Longitude

# check for NAs
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are NAs in Depth, Bottemp, so remove them
covariate_data <- na.omit(covariate_data)


# I have previously modify the dataset for only overlapping years, 2007-2019, AND dividing into SPRING and FALL seasons. I'd like to build in season into the model and not separate, but for now I'm going to stick with just running this for the spring to clean up and build out.

library(dplyr)
# subset for 2009-2019, spring, for now.
# data <- data %>%
#   filter(Year >= 2009 & Year <= 2019) %>%
#   filter(Season == "SPRING") 
# covariate_data <- covariate_data %>%
#   filter(Year >= 2009 & Year <= 2019) %>%
#   filter(Season == "SPRING") 

# subset for 2009-2019, spring, NEFSC only.
data <- data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 2009 & Year <= 2019) %>%
  filter(Season == "SPRING") #or SPRING
covariate_data <- covariate_data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 2009 & Year <= 2019) %>%
  filter(Season == "SPRING") #or SPRING

# Model Settings
####################
settings = make_settings( n_x = 350, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid', #must be 'samples' or 'grid'
                          ObsModel= c(2,0), #1st value catch rate= gamma = 2; 2nd value encounter probabilities= 0 default log-link,
                          use_anisotropy=FALSE,
                          bias.correct = FALSE,
                          fine_scale=TRUE )


# Covariate Settings
####################

## Depth AND Bottom Temperature
####################
# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
covariate_data$DepthScale = covariate_data$Depth / 100

# Define formulas for the covariates
# This formula is based on J. Thorson's covariate example which is a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
# Define formula for Depth
X1_formula = ~ bs( log(DepthScale), degree=2, intercept=FALSE)
# Define formula for Bottom Temp
X2_formula = ~ bs( log(Bottemp), degree=2, intercept=FALSE)


# User-Defined Extrapolation Grid
####################
# Set the extrapolation grid, created in "create extrapolation grid for menhaden.R" 
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/loop_run/user_region.rds')
# Set units, not working
# make_extrapolation_info(user_region, as_units(b_i,'kg'), as_units(a_i,'km^2'))


# Fit the model
####################
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects, along with depth covariate

# SPRING
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$Lat,
                 "Lon_i" = data$Lon,
                 "t_i" = data$Year, #time
                 "b_i" = data$Biomass, #catch
                 "a_i" = data$Areasw, #area swept
                 "v_i"= data$Cruise, #vessel effects, I think wrong
                 "X1_formula" = X1_formula, #depth
                 "X2_formula" = X2_formula, #bottemp
                 "covariate_data" = covariate_data,
                 "input_grid" = user_region)


# Model Results
####################
plot( fit )


# Examining Model Results
####################
# I've put this in examine-model-output.R