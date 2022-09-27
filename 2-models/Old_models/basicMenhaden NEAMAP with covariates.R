##############################
# Basic menhaden model with covariates using NEAMAP data
#############################

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

##########
# Data Prep
##########
data <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
colnames(data)
# "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# "vimscode"  "count"     "weight"  

# Make covariate data
# Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
covariate_data = data.frame(
  "Lat" = data$latitude,
  "Lon" = data$longitude,
  "Year" = data$year,
  "DEPTH" = data$depth,
  "WTEMP" = data$WT
)

# check for NAs
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are in WTemp. The model won't run if there are NA values for a covariate
# Just remove them for now
covariate_data <- na.omit(covariate_data)

# If all covariates as "static" (not changing among years),
#  then set Year = NA to cause values to be duplicated internally for all values of Year
# If using a mix of static and dynamic covariates,
#  then duplicate rows for static covariates for every value of Year
# For Static variables (variables that don't change year to year), set year = NA
covariate_data$Year = NA

# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
covariate_data$DEPTHscale = covariate_data$DEPTH / 100

#########
# Model Settings
#########
settings = make_settings( n_x = 250, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid',
                          ObsModel= c(2,0),#, 1st value encounter probabilities = Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc)),
                          use_anisotropy=FALSE,
                          bias.correct = FALSE,
                          fine_scale=TRUE )

# Define formula for the covariates
# In this case I'm demonstrating how to use a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
X1_formula = ~ bs( log(DEPTHscale), degree=2, intercept=FALSE)

##-- User Defined extrapolation grid
user_region <- readRDS('user_region_NEAMAP.rds')

# Fit the model
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects. This doesn't have any covariates (depth or bottom temp) yet.
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$latitude,
                 "Lon_i" = data$longitude,
                 "t_i" = data$year, #time
                 "b_i" = data$weight, #catch
                 "a_i" = data$areasw, #area swept
                 "v_i"= data$cruise,
                 "X1_formula" = X1_formula,
                 "covariate_data" = covariate_data,
                 "input_grid" = user_region)

# Plot the results
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
setwd("/Users/janellemorano/Git/VAST output_DONOTGIT/_currentrun")
plot( fit )
