# Janelle Morano's Atlantic menhaden model
# example shared with Jim Thorson

library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects
# sessionInfo()
# When run on Janelle's system:
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.1
# FishStatsUtils_2.10.2 units_0.7-2         
# VAST_3.8.2      
# TMB_1.7.22           
# compiler_4.1.2  Matrix_1.3-4    tools_4.1.2     sp_1.4-6        INLA_21.02.23   Rcpp_1.0.8     
# splines_4.1.2   grid_4.1.2      lattice_0.20-45

# Data Prep
####################
# Data include Atlantic menhaden abundance and biomass from 2 bottom trawl surveys: NEAMAP and NEFSC. The model currently is separate for the 2 sample seasons: Spring, Fall, and uses only the years of overlap between the surveys: 2007-2021 (except 2021 covariate data are missing right now).


# Covariates: Depth AND Bottom Temperature
####################

# Covariates are rescaled following the use-case example, but maybe should be standardizing by mean
# covariate_data.spring$DepthScale = covariate_data.spring$Depth / 100
# covariate_data.fall$DepthScale = covariate_data.fall$Depth / 100

# Define formulas for the covariates
# Define formula for Depth
# X1_formula = ~ bs( log(DepthScale), degree=2, intercept=FALSE)
# # Define formula for Bottom Temp
# X2_formula = ~ bs( log(Bottemp), degree=2, intercept=FALSE)


# User-Defined Extrapolation Grid
####################
# Set the extrapolation grid: extend of NEFSC and NEAMAP survey


# Load all data shared by Janelle
#################################
load("forsharing.RData")

# Model Settings
####################
settings = make_settings( n_x = 800, #350 knots with gamma dist has worked, too
                          Region = 'User', 
                          purpose = "index2", 
                          knot_method = 'grid', #must be 'samples' or 'grid'
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
                        "a_i" = as_units(data.spring$Areasw, km^2), #area swept
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
                      "b_i" = data.fall$Biomass, #catch
                      "a_i" = data.fall$Areasw, #area swept
                      # "v_i"= data.fall$Cruise, #vessel effects, I think very wrong, so keep out
                      "X1_formula" = X1_formula, #depth
                      "X2_formula" = X2_formula, #bottemp
                      "covariate_data" = covariate_data.fall,
                      "input_grid" = user_region)

# Model Results
####################
# !Currently, output from spring and fall model are going to same place, so need to manually move output
plot( fit.spring )

# !!! Make sure you've moved the spring output before over-writing with these fall output!!!
plot( fit.fall )
