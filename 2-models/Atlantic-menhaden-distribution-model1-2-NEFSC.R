# Atlantic-menhaden-distribution-model1-2-NEFSC.R
###############################################
# Janelle L. Morano
#
# NOTE: THESE MODELS AREN'T WORKING SO GO BACK TO THE 20220401 MODEL TO RESOLVE BEFORE UPDATING THIS AND THE OTHER FILE
#
# Menhaden distribution models in VAST 3.8.2 
# Data:  NEFSC and NEAMAP created from "allsurvey" data in "create-allsurvey-menhaden-model-data.R"/Users/janellemorano/Git/AtlanticMenhaden/data/create-allsurvey-menhaden-model-data.R"
# See also surveydata repo for more info
#
# Model comparison:
# Spring AND Fall separate
# m1: spatial, temporal; depth, bottemp; NEFSC; 1968-2021
# m2: spatial, temporal, spatio-temporal; depth, bottemp; NEFSC; 1968-2021

# These models are in a separate file:
# m3: spatial, temporal; depth, bottemp; NEFSC & NEAMAP; 2007-2021
# m4: spatial, temporal, spatio-temporal; depth, bottemp; NEFSC & NEAMAP; 2007-2021
#
# last updated 28 April 2022
###############################################
###############################################


# Setup & Libraries
####################
# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

# Working directory needs to have the extrapolation grid (User_Region.rds)
# Currently, this extrapolation grid is inclusive of the entire NEAMAP and NEFSC survey site, which should be ok, because NEAMAP covers regions that were not sampled by NEFSC in some years, but need to verify this is OK.
# Previous runs that produce Kmeans_knots and Kmeans_extrapolation should be kept in the directory for faster processing (but only if the extrapolation grid has not changed!)
setwd("/Users/janellemorano/MODEL_OUTPUT/menhaden-model-comp")

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


# SPRING and FALL Models are SEPARATE
# I have previously modified the dataset for only overlapping years, 2007-2019, AND dividing into SPRING and FALL seasons. I'd like to build in season into the model and not separate, but for now I'm going to stick with just running this for the spring to clean up and build out.

library(dplyr)


####################################################################################################
# Model 1: spatial, temporal; depth, bottemp; NEFSC; 1968-2021
# Not working right now
####################################################################################################


# Data Filter: NEFSC
####################
# subset for 1968-2021*** (excluding 2021 until get covariate data), Spring
data <- data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 1968 & Year <= 2020) %>%
  filter(Season == "SPRING") 
covariate_data <- covariate_data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 1968 & Year <= 2020) %>%
  filter(Season == "SPRING")


# Model Settings
####################
settings = make_settings( n_x = 250, 
                          Region = 'User', 
                          purpose = "index2", 
                          knot_method = 'grid', #must be 'samples' or 'grid'
                          FieldConfig = c("Omega1" = 0, "Omega2" = 1, "Epsilon1" = 0, "Epsilon2" = 1), #0 for both epsilon to exclude spatio-temporal variation
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
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/menhaden-model-comp/user_region.rds')
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
                 "X1_formula" = X1_formula, #depth
                 "X2_formula" = X2_formula, #bottemp
                 "covariate_data" = covariate_data,
                 "input_grid" = user_region)


# Print Model Results to Folder
#################################
plot( fit )

# Model results can be examined in examine-model-output.R


####################################################################################################
# Model 2: spatial, temporal, spatio-temporal; depth, bottemp; NEFSC; 1968-2021
####################################################################################################


# Data Filter: NEFSC
####################
# ****go back and read in full dataset before filtering again****
# subset for 2009-2019*** fix dates here, Spring
data <- data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 2009 & Year <= 2019) %>%
  filter(Season == "SPRING") 
covariate_data <- covariate_data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 2009 & Year <= 2019) %>%
  filter(Season == "SPRING")


# Model Settings
####################
settings = make_settings( n_x = 250, 
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
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/menhaden-model-comp/user_region.rds')
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


# Print Model Results to Folder
#################################
plot( fit )

# Model results can be examined in examine-model-output.R



####################################################################################################
# Model 2: spatial, temporal, spatio-temporal; depth, bottemp; NEFSC; 1968-2021
# Not currently working
####################################################################################################


# Data Filter: NEFSC
####################
# subset for 1968-2021*** (excluding 2021 until get covariate data), Spring
data <- data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 1968 & Year <= 2020) %>%
  filter(Season == "SPRING") 
covariate_data <- covariate_data %>%
  filter(Survey == "NEFSC") %>%
  filter(Year >= 1968 & Year <= 2020) %>%
  filter(Season == "SPRING")


# Model Settings
####################
settings = make_settings( n_x = 500, 
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
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/menhaden-model-comp/user_region.rds')
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


# Print Model Results to Folder
#################################
plot( fit )

# Model results can be examined in examine-model-output.R
