## Simple Menhaden model
# Detailed context in in the "basic mehaden model in VAST.rmd"
# This is the simplified code that represents the latest (hopefully, working) version.

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
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/survdat.menhaden.csv", header = TRUE)
data$AreaSwept_km2 <- 0.01

##-- Model Settings
settings = make_settings( n_x = 250, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid',
                          ObsModel= c(2,0),#, 1st value encounter probabilities = Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc)),
                          bias.correct = FALSE )
#need to look more into next step
settings$FieldConfig[2,] <- 0 ## turn off temporal components with 0

##-- User Defined extrapolation grid
user_region <- readRDS('user_region.rds')

# Fit the model
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects. This doesn't have any covariates (depth or bottom temp) yet.
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$LAT,
                 "Lon_i" = data$LON,
                 "t_i" = data$YEAR, #time
                 "b_i" = data$ABUNDANCE, #catch
                 "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
                 "v_i"= data$SVVESSEL,
                 "input_grid" = user_region)

# Plot the results
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
setwd("/Users/janellemorano/Git/VAST output_DONOTGIT/_currentrun")
plot( fit )

