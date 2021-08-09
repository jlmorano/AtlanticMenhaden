## Simple Menhaden model
# Detailed context in in the "basic mehaden model in VAST.rmd"
# This is the simplified code without much comment for error testing.

### Load packages
library(VAST)
sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.8.0 
# VAST_3.6.1           
# TMB_1.7.19 
# Matrix_1.2-8

##-- Data Prep
data <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/survdat.menhaden.csv", header = TRUE)
data$AreaSwept_km2 <- 0.01

##-- Model Settings
settings = make_settings( n_x = 100, #number of knots is arbitrary right now, keeping low to start
                         Region = 'User', 
                         purpose = "index2", 
                         knot_method = 'grid',
                         ObsModel= c(1,0),#, 1st value encounter probabilities = Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc),
                         bias.correct = FALSE )
# Turn off temporal components
#settings$FieldConfig[2,] <- 0

##-- User Defined extrapolation grid
user_region <- readRDS('user_region.rds')

##-- Fit the model
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$LAT,
                 "Lon_i" = data$LON,
                 "t_i" = data$YEAR, #time
                 "b_i" = data$ABUNDANCE, #catch
                 "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
                 "v_i"= data$SVVESSEL,
                 "input_grid" = user_region)

#-- Errors in fitting
# Getting the following errors:

# NA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluation 
# Error in optimHess(parameter_estimates$par, fn = fn, gr = gr) : gradient in optim evaluated to length 1 not 110

## after turning off the temporal components
# Error in optimHess(parameter_estimates$par, fn = fn, gr = gr) : 
#   gradient in optim evaluated to length 1 not 110
# In addition: There were 19 warnings (use warnings() to see them)

## switched from Poisson to lognormal distribution and it worked


##-- Plot the results
plot( fit )
