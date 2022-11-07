# Atlantic-menhaden-seasonal-model
###############################################
# Janelle L. Morano
# Model in VAST 3.9.1 using NEFSC and NEAMAP data*
# model structure last updated 20 October 2022

# *NEFSC and NEAMAP data 2007-2022 last updated 28 Sep 2022**
# **2022 depth/temp data is missing right now, so when NAs excluded, 2022 is ommitted from analysis

# System Info updated 20 October 2022

###############################################
###############################################

# Goal: 

# Add to your dataset a column that identifies each season and year combination as a factor
# Then create a set of dummy data that is a duplicate but adds a column "dummy" as TRUE
# 
# matrix of year, season
# for each year, the season available

# Model with year as a fixed effect (RhoConfig, Beta = 3, Epsilon = 4 )
# 3 = autocorrelation is a fixed effect among years
# 4 = autocorrelation follows an AR1 process (i.e. lag of 1 year)


setwd("/Users/janellemorano/MODEL_OUTPUT/_seasonalrun")
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects
library(dplyr)
sessionInfo()
# R version 4.2.1 (2022-06-23)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.6

# dplyr_1.0.10          effects_4.2-2         carData_3.0-5        
# FishStatsUtils_2.11.0 units_0.8-0           VAST_3.9.1           
# TMB_1.9.1

#################
# Prep the Data #
#################

## Load data and remove NAs
# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)

# check for NAs, the model won't run if there are NA values for a covariate
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are NAs in Depth, Surftemp, Surfsalin, Bottemp, Botsalin.
# The large number missing values for Surftemp and Surfsalin are because these were not recorded for NEAMAP, but I'm not using those columns anyway. Drop those columns
covariate_data <- select(covariate_data, -c(Surftemp, Surfsalin, Botsalin))
# Remove NAs, because the missing values for Bottemp will be a problem
covariate_data <- na.omit(covariate_data)


## Build a label of Year-Season to add to the data
# Set of all years and seasons
year_set = sort(unique(data[,'Year']))
season_set = c("SPRING", "FALL") #select so not in alpha order

# Create a grid with all unique combinations of seasons and years and then combine these into one "year_season" variable
yearseason_grid = expand.grid("season" = season_set, "year" = year_set)
yearseason_levels = apply(yearseason_grid[,2:1], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_labels = round(yearseason_grid[,'year'] + (as.numeric(factor(yearseason_grid[,'season'], levels = season_set))-1)/length(season_set), digits=1)

# Similar process, but for the observations
yearseason_i = apply(data[,c("Year","Season")], MARGIN = 1, FUN = paste, collapse = "_")
yearseason_i = factor(yearseason_i, levels = yearseason_levels)

# Add the year_season factor column to our sampling_data data set
data$Year_Season = yearseason_i
data$Season = factor(data$Season, levels = season_set)

# clean up data
data = select(data, -X)

## Make dummy observation for each season-year combination BECAUSE...
dummy_data = data.frame(
  Survey = "DUMMY",
  Cruise = "DUMMY",
  Station = "DUMMY",
  Stratum = "DUMMY",
  Year = yearseason_grid[,'year'],
  Season = yearseason_grid[,'season'],
  Latitude = mean(data[,'Latitude']),
  Longitude = mean(data[,'Longitude']),
  Areasw = mean(data[,'Areasw']),
  Abundance = 0,
  Biomass = 0,
  Year_Season = yearseason_levels,
  dummy = TRUE)

# Combine with sampling data
full_data = rbind(cbind(data, dummy = FALSE), dummy_data)

# Create sample data for the model
samp_dat = data.frame(
  "Year_Season" = as.numeric(full_data$Year_Season)-1,
  "Lat" = full_data$Latitude,
  "Lon" = full_data$Longitude,
  "Biomass" = full_data$Biomass,
  "Areasw" = full_data$Areasw,
  "Dummy" = full_data$dummy )

# # Covariate data. Note here, case sensitive!
cov_dat = data.frame(
  "Year" = as.numeric(full_data$Year_Season)-1,
  "Year_Cov" = factor(full_data$Year, levels = year_set),
  "Season" = full_data$Season,
  "Lat" = full_data$Latitude,
  "Lon" = full_data$Longitude)

# Inspect
table("year_season"=cov_dat$Year, "Actual_year"=cov_dat$Year_Cov)
table("year_season"=cov_dat$Year, "Actual_season"=cov_dat$Season)


###################
# Build the Model #
###################

# Set the extrapolation grid, created in "create extrapolation grid for menhaden.R" 
user_region <- readRDS('/Users/janellemorano/MODEL_OUTPUT/_seasonalrun/user_region.rds')

# Make settings
settings = make_settings(n_x = 100,
                         Region = 'User', 
                         purpose = "index2", 
                         knot_method = 'grid', #must be 'samples' or 'grid'
                         FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),
                         RhoConfig = c("Beta1" = 3, "Beta2" = 3, "Epsilon1" = 4, "Epsilon2" = 4),
                         ObsModel = c(1, 1),
                         bias.correct = FALSE,
                         Options = c('treat_nonencounter_as_zero' = TRUE) )


# Creating model formula
formula_use = ~ Season + Year_Cov #Original seasonal example

# formula_use = ~ poly(log(DepthScale), degree=2) + poly( log(Bottemp), degree=2 + Season + Year_Cov) #Is this correct?

# Implement corner constraint for linear effect but not spatially varying effect:
# * one level for each term is 2 (just spatially varying)
# * all other levels for each term is 3 (spatially varying plus linear effect)
X1config_cp_use = matrix( c(2, rep(3,nlevels(cov_dat$Season)-1), 2, rep(3,nlevels(cov_dat$Year_Cov)-1) ), nrow=1 )
X2config_cp_use = matrix( c(2, rep(3,nlevels(cov_dat$Season)-1), 2, rep(3,nlevels(cov_dat$Year_Cov)-1) ), nrow=1 )

#####
## Model fit -- make sure to use new functions
#####

fit_orig = fit_model("settings" = settings,
                     "Lat_i" = samp_dat[, 'Lat'],
                     "Lon_i" = samp_dat[, 'Lon'],
                     "t_i" = samp_dat[, 'Year_Season'],
                     "b_i" = samp_dat[, 'Biomass'],
                     "a_i" = samp_dat[, 'Areasw'],
                     "X1config_cp" = X1config_cp_use,
                     "X2config_cp" = X2config_cp_use,
                     "covariate_data" = cov_dat,
                     "X1_formula" = formula_use, #first linear predictor
                     "X2_formula" = formula_use, #second linear predictor
                     "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
                     "run_model" = FALSE,
                     "PredTF_i" = samp_dat[, 'Dummy'],
                     "input_grid" = user_region)

#####
# I don't know why this has to happen
####

# Adjust mapping for log_sigmaXi and fitting final model -- pool variance for all seasons and then set year's to NA
Map_adjust = fit_orig$tmb_list$Map

# Pool variances for each term to a single value
Map_adjust$log_sigmaXi1_cp = factor(c(rep(as.numeric(Map_adjust$log_sigmaXi1_cp[1]), nlevels(cov_dat$Season)),
                                      rep(as.numeric(Map_adjust$log_sigmaXi1_cp[nlevels(cov_dat$Season)+1]), nlevels(cov_dat$Year_Cov))))
Map_adjust$log_sigmaXi2_cp = factor(c(rep(as.numeric(Map_adjust$log_sigmaXi2_cp[1]), nlevels(cov_dat$Season)),
                                      rep(as.numeric(Map_adjust$log_sigmaXi2_cp[nlevels(cov_dat$Season)+1]), nlevels(cov_dat$Year_Cov))))

# Fit final model with new mapping
fit  = fit_model("settings" = settings,
                 "Lat_i" = samp_dat[, 'Lat'],
                 "Lon_i" = samp_dat[, 'Lon'],
                 "t_i" = samp_dat[, 'Year_Season'],
                 "b_i" = samp_dat[, 'Biomass'],
                 "a_i" = samp_dat[, 'Areasw'],
                 "X1config_cp" = X1config_cp_use,
                 "X2config_cp" = X2config_cp_use,
                 "covariate_data" = cov_dat,
                 "X1_formula" = formula_use,
                 "X2_formula" = formula_use,
                 "X_contrasts" = list(Season = contrasts(cov_dat$Season, contrasts = FALSE), Year_Cov = contrasts(cov_dat$Year_Cov, contrasts = FALSE)),
                 "PredTF_i" = samp_dat[, 'Dummy'],
                 "Map" = Map_adjust,
                 "input_grid" = user_region,
                 "run_model" = FALSE)

# Model didn't work so added "run_model" = FALSE, then,
ParHat = TMBhelper:::extract_fixed(fit$tmb_list$Obj)
Gr = fit$tmb_list$Obj$gr( ParHat )
which(Gr == 0)


plot( fit,
      projargs='+proj=natearth +lon_0=-68 +units=km',
      country = "united states of america",
      year_labels = yearseason_labels )
plot(fit)
