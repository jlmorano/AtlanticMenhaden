# Atlantic menhaden distribution model
######################################
# Janelle L. Morano
# Model in VAST using NEFSC and NEAMAP data
# last updated 9/29/2021

###############################################
###############################################
setwd("/Users/janellemorano/MODEL_OUTPUT/_currentrun/")

library(VAST)
sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.9.1
# VAST_3.7.1           
# TMB_1.7.18 
# Matrix_1.2-8
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects

##########################
# Data Prep
##########################
# Data were compiled in Atlantic_menhaden_model_data.R
# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)

# check for NAs
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are in Depth, Surftemp, Surfsalin, Bottemp, Botsalin.
# The large number missing values for Surftemp and Surfsalin are because these were not recorded for NEAMAP

# Just remove them for now
covariate_data <- na.omit(covariate_data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2021, and NEFSC is 1963-2019.
# Here's an amended dataset for only overlapping years 2007-2019.
data.sm <- data %>%
  filter(Year >= 2007 & Year <= 2019)
covariate_data.sm <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019)
# when using this smaller dataset, use alternative model fit

#########
# Model Settings
#########
settings = make_settings( n_x = 300, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid',
                          ObsModel= c(2,0),#, 1st value encounter probabilities = gamma = 2, Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc)),
                          use_anisotropy=FALSE,
                          bias.correct = FALSE,
                          fine_scale=TRUE )

# Covariate Settings
#########

## JUST DEPTH
# # So all covariates are "static" (not changing among years),
# #  then set Year = NA to cause values to be duplicated internally for all values of Year
covariate_data$Year = NA
# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
covariate_data$DepthScale = covariate_data$Depth / 100
# Define formula for the covariates
# In this case I'm demonstrating how to use a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
X1_formula = ~ bs( log(DepthScale), degree=2, intercept=FALSE)

## BOTTEMP
# # So all covariates are a mix of "dynamic",
# #  then duplicate rows for static covariates for every value of Year
# 
# # Define formula for the covariates
# X1_formula = ~ bs( log(Bottemp), degree=2, intercept=FALSE)


##-- User Defined extrapolation grid
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/_currentrun/user_region.rds')

# Fit the model
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects, along with depth covariate.
# fit = fit_model( "settings" = settings,
#                  "Lat_i" = data$Latitude,
#                  "Lon_i" = data$Longitude,
#                  "t_i" = data$Year, #time
#                  "b_i" = data$Weight, #catch
#                  "a_i" = data$Areasw, #area swept
#                  "v_i"= data$Cruise,
#                  "X1_formula" = X1_formula,
#                  "covariate_data" = covariate_data,
#                  "input_grid" = user_region)

# This is an alternative to above using the smaller dataset of 2007-2019.
fit = fit_model( "settings" = settings,
                 "Lat_i" = data.sm$Latitude,
                 "Lon_i" = data.sm$Longitude,
                 "t_i" = data.sm$Year, #time
                 "b_i" = data.sm$Weight, #catch
                 "a_i" = data.sm$Areasw, #area swept
                 "v_i"= data.sm$Cruise,
                 "X1_formula" = X1_formula,
                 "covariate_data" = covariate_data.sm,
                 "input_grid" = user_region)

# Plot the results
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
setwd("/Users/janellemorano/MODEL_OUTPUT/_currentrun/'Kmeans_extrapolation-2500.RData")
plot( fit )
