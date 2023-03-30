# Atlantic-menhaden-distribution-model-20230111.R
###############################################
# Janelle L. Morano
# Model in VAST 3.9.1 using NEFSC and NEAMAP data^
# model structure last updated 11 January 2023

# ^NEFSC and NEAMAP data 2007-2022 last updated 28 Sep 2022^^
# ^^2022 depth/temp data is missing right now, so when NAs excluded, 2022 is omitted from analysis

# System Info updated 30 March 2023

###############################################
###############################################


# Model Description
####################
# 1. Spatial domain: 
#   + extrapolation grid of the full sampled strata/region area, which is inclusive of the NEFSC strata. NEAMAP falls within some of the coastal strata in later years to supplement what NEFSC boat change couldn't do.
# 1. Categories to include
#   + Univariate for species: Atlantic menhaden
# 1. Analyze encounter, abundance, and/or biomass-sampling data
#   + Biomass
# 1. Including spatial and/or spatio-temporal variation
#   + Omega1 = 1, spatial random effects for encounter probability
#   + Omega2 = 1, spatial random effects for positive catch rates
#   + Epsilon1 = 1, spatiotemporal random effects for encounter probability
#   + Epsilon2 = 1, spatiotemporal random effects for positive catch rates
# 1. Spatial smoother and resolution: SPDE approximation (default) with either isotropic Matern function (2D Mesh) or geometric anisotropy or isotropic  exponential correlation function
#   + Method = 'Mesh' (default)
# 1. Number of spatial and spatio-temporal factors
#   + HELP- not sure how to think about this
# 1. Specifying temporal correlation on model components: 
#   + fixed effect for each year and independent among years (default)
#   + BUT, need to develop also a seasonal model with season (or month) as a fixed effect
# 1. Density covariates as a semi-parametric model
#   + HELP- not sure how to think about this
# 1. Accounting for catchability covariates and confounding variables
#   + HELP- not sure which information to gather to add. NEFSC data is corrected for catchability for some species but not menhaden.
# 1. Including area swept as a catchability covariate or offset
#   + HELP- related to above and area swept is not available for NEFSC, or I'm not sure where to get that info
# 1. Including vessel effects as overdispersion
#   + Probably don't need to if I'm correctly accounting for catchability, but maybe?
# 1. Choosing link functions and distributions
#   + my catch data are zero-skewed, but the log of the >0 biomass are normally distributed, is it better to assume a lognormal distribution or a gamma distribution? And/or given that the abundance data is the same, is it advantageous to assume a negative binomial? Or, should I explore all options and see how the model behaves?
# 1. Derived quantities
#   + n/a
# 1. Bias correction for derived quantities
#   + n/a
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
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects

sessionInfo()
# R version 4.2.3 (2023-03-15)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.6.3
# FishStatsUtils_2.12.1         
# VAST_3.10.1      
# TMB_1.9.3           


# Data Prep
####################

# Data were compiled in create-nefsc-neamap-menhaden-model-data.R
# data include NEAMAP and NEFSC

# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)
# If I didn't name the covariate data correctly (as Lat and Lon), fix the lat/lon
library(dplyr)
covariate_data <- covariate_data %>%
  rename(Lat = Latitude,
         Lon = Longitude)

# check for NAs
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are NAs in Depth, Surftemp, Surfsalin, Bottemp, Botsalin.
# The large number missing values for Surftemp and Surfsalin are because these were not recorded for NEAMAP, but I'm not using those columns anyway. Drop those columns
covariate_data <- select(covariate_data, -c(Surftemp, Surfsalin, Botsalin))

# Remove NAs, because the missing values for Bottemp will be a problem
covariate_data <- na.omit(covariate_data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2022, and NEFSC is 1963-2021.
# Selecting 1972-2021

# SPRING
data.spring <- data %>%
  filter(Year >= 1972 & Year <= 2021) %>%
  filter(Season == "SPRING")
covariate_data.spring <- covariate_data %>%
  filter(Year >= 1972 & Year <= 2021) %>%
  filter(Season == "SPRING")

# FALL
data.fall <- data %>%
  filter(Year >= 1972 & Year <= 2021) %>%
  filter(Season == "FALL")
covariate_data.fall <- covariate_data %>%
  filter(Year >= 1972 & Year <= 2021) %>%
  filter(Season == "FALL")



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
# All covariates are a mix of "dynamic"
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

## Should is be this instead??
# X1_formula = ~ poly(log(DepthScale), degree=2) + poly( log(Bottemp), degree=2 )
# And should X2 be the same?


# User-Defined Extrapolation Grid
####################
# Set the extrapolation grid, created in "create extrapolation grid for menhaden.R" 
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/_currentrun/user_region.rds')


# Model Settings
####################
# Beta = temporal variation
# Omega = spatial variation
# Epsilon = spatio-temporal variation

settings = make_settings( n_x = 350, #350 knots with gamma dist has worked
                          Region = 'User', 
                          purpose = "index2", 
                          knot_method = 'grid', #must be 'samples' or 'grid'
                          FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                          RhoConfig = c("Beta1" = 0, "Beta2" = 0, "Epsilon1" = 1, "Epsilon2" = 1),
                          ObsModel= c(2,0), # 1st value catch rate= lognormal = 1 OR gamma = 2; 
                          # 2nd value encounter probabilities= 0 default log-link
                          use_anisotropy=FALSE,
                          bias.correct = FALSE,
                          fine_scale=TRUE )


# Fit the model
####################
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects, along with depth covariate

# SPRING
fit.spring = fit_model( "settings" = settings,
                        "Lat_i" = data.spring$Lat,
                        "Lon_i" = data.spring$Lon,
                        "t_i" = data.spring$Year, #time
                        "b_i" = as_units(data.spring$Biomass, "kg"),
                        "a_i" = as_units(data.spring$Areasw, "km^2"), #area swept
                        # "v_i"= data.spring$Cruise, #vessel effects, I think very wrong, so keep out
                        "X1_formula" = X1_formula, #depth
                        "X2_formula" = X2_formula, #bottemp
                        "covariate_data" = covariate_data.spring,
                        "input_grid" = user_region)

# FALL
fit.fall = fit_model( "settings" = settings,
                      "Lat_i" = data.fall$Latitude,
                      "Lon_i" = data.fall$Longitude,
                      "t_i" = data.fall$Year, #time
                      "b_i" = as_units(data.fall$Biomass, "kg"), #catch
                      "a_i" = as_units(data.fall$Areasw, "km^2"), #area swept
                      # "v_i"= data.fall$Cruise, #vessel effects, I think very wrong, so keep out
                      "X1_formula" = X1_formula, #depth
                      "X2_formula" = X2_formula, #bottemp
                      "covariate_data" = covariate_data.fall,
                      "input_grid" = user_region)

# Model Results
####################

# Save R environment for creating minimal example to share
# save.image(file = "Atlantic-menhaden-distribution-model-20220401_output.RData")

# load("/Users/janellemorano/Git/AtlanticMenhaden/model-output/Atlantic-menhaden-distribution-model-20220401_output.RData")

# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
# !Currently, output from spring and fall model are going to same place, so need to manually move output
plot( fit.spring )

# !!! Make sure you've moved the spring output before over-writing with these fall output!!!
plot( fit.fall )


# Examining Model Results
####################
# I've put this in examine-model-output.R

Options =  c("SD_site_density"=1, "SD_site_logdensity"=1, "Calculate_Range"=0, "Calculate_evenness"=0, "Calculate_effective_area"=0,"Calculate_Cov_SE"=0, 'Calculate_Synchrony'=0, 'Calculate_Coherence'=0)
