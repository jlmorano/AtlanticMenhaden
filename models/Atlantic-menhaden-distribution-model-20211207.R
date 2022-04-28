# Atlantic menhaden distribution model
###############################################
# Janelle L. Morano
# Model in VAST 3.8.2 using NEFSC and NEAMAP (updated as of 22 Feb 2022) data
# 2021 covariate data is missing right now, so subsetted to keep out 2021 from analysis
# last updated 1 April 2022
###############################################
###############################################


# Model Description
# (should go here, but this is currently inadequate and more of a placeholder)
####################
# 1. Spatial domain
#   + extrapolation grid of the full sampled strata/region area
# 1. Categories to include (species or size or age)
#   + species: Atlantic menhaden
# 1. Analyze encounter, abundance, and/or biomass-sampling data
#   + abundance (maybe revise)
# 1. Including spatial and/or spatio-temporal variation
#   + Omega1 = 1, spatial random effects for encounter probability
#   + Omega2 = 1, spatial random effects for positive catch rates
#   + Epsilon1 = 1, spatiotemporal random effects for encounter probability
#   + Epsilon2 = 1, spatiotemporal random effects for positive catch rates
# 1. Spatial smoother and resolution: SPDE approximation (default) with either isotropic Matern function (2D Mesh) or geometric anisotropy or isotropic  exponential correlation function
#   + Method = 'Mesh' (default)
# 1. Number of spatial and spatio-temporal factors HELP
# 1. Specifying temporal correlation on model components: fixed effect for each year and independent among years (default)
# 1. Density covariates as a semi-parametric model HELP
# 1. Accounting for catchability covariates and confounding variables
# 1. Including area swept as a catchability covariate or offset
# 1. Including vessel effects as overdispersion
# 1. Choosing link functions and distributions
# 1. Derived quantities (other output? HELP)
# 1. Bias correction for derived quantities (HELP)
# 1. Model selection


# Setup & Libraries
####################
# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

# Working directory needs to have the extrapolation grid (User_Region.rds)
# Previous runs that produce Kmeans_knots and Kmeans_extrapolation should be kept in the directory for faster processing (but only if the extrapolation grid has not changed!)
setwd("/Users/janellemorano/MODEL_OUTPUT/_currentrun/")

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
# Data were compiled in create-nefsc-neamap-menhaden-model-data.R
# data include NEAMAP and NEFSC

# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)
# If I didn't name the covariate data correctly, fix the lat/lon
library(dplyr)
covariate_data <- covariate_data %>%
  rename(Lat = Latitude,
         Lon = Longitude)

# check for NAs
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are NAs in Depth, Surftemp, Surfsalin, Bottemp, Botsalin.
# The large number missing values for Surftemp and Surfsalin are because these were not recorded for NEAMAP
# Remove them, because the missing values for Bottemp will be a problem
covariate_data <- na.omit(covariate_data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2021, and NEFSC is 1963-2019.
# Modify the dataset for only overlapping years, 2007-2019, AND dividing into SPRING and FALL seasons.
library(dplyr)

# SPRING
data.spring <- data %>%
  filter(Year >= 2007 & Year <= 2020) %>%
  filter(Season == "SPRING")
covariate_data.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2020) %>%
  filter(Season == "SPRING")

# FALL
data.fall <- data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "FALL")
covariate_data.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "FALL")


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

## Depth Only
####################
# # Depth covariate is "static" (not changing among years),
# #  so set Year = NA to cause values to be duplicated internally for all values of Year
# covariate_data.spring$Year = NA
# covariate_data.fall$Year = NA
# # Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
# covariate_data.spring$DepthScale = covariate_data.spring$Depth / 100
# covariate_data.fall$DepthScale = covariate_data.fall$Depth / 100
# # Define formula for the covariates
# # This formula is based on J. Thorson's covariate example which is a basis-spline with
# # three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# # based on example developed by Nicholas Ducharme-Barth.
# X1_formula = ~ bs( log(DepthScale), degree=2, intercept=FALSE)

## Depth AND Bottom Temperature
####################
# All covariates are a mix of "dynamic",
#  then duplicate rows for static covariates for every value of Year
# BUT I don't think I have to bother with above since each bio sample point has a corresponding covariate
# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
covariate_data.spring$DepthScale = covariate_data.spring$Depth / 100
covariate_data.fall$DepthScale = covariate_data.fall$Depth / 100

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
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/_currentrun/user_region.rds')

# Fit the model
####################
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects, along with depth covariate

# SPRING
fit.spring = fit_model( "settings" = settings,
                 "Lat_i" = data.spring$Lat,
                 "Lon_i" = data.spring$Lon,
                 "t_i" = data.spring$Year, #time
                 "b_i" = data.spring$Biomass, #catch
                 "a_i" = data.spring$Areasw, #area swept
                 "v_i"= data.spring$Cruise, #vessel effects, I think wrong
                 "X1_formula" = X1_formula, #depth
                 "X2_formula" = X2_formula, #bottemp
                 "covariate_data" = covariate_data.spring,
                 "input_grid" = user_region)

# FALL
fit.fall = fit_model( "settings" = settings,
                 "Lat_i" = data.fall$Latitude,
                 "Lon_i" = data.fall$Longitude,
                 "t_i" = data.fall$Year, #time
                 "b_i" = data.fall$Biomass, #catch
                 "a_i" = data.fall$Areasw, #area swept
                 "v_i"= data.fall$Cruise,
                 "X1_formula" = X1_formula, #depth
                 "X2_formula" = X2_formula, #bottemp
                 "covariate_data" = covariate_data.fall,
                 "input_grid" = user_region)

# Model Results
####################
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
# !Currently, output from spring and fall model are going to same place, so need to manually move output
plot( fit.spring )

# !!! Make sure you've moved the spring output before over-writing with these fall output!!!
plot( fit.fall )

# Examining Model Results
####################
# I've put this in examine-model-output.R