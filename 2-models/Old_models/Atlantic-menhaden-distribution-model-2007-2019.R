# Atlantic menhaden distribution model
###############################################
# Janelle L. Morano
# Model in VAST using NEFSC and NEAMAP data
# last updated 10/14/2021

# Description
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
# Restart R now

# Working directory needs to have the extrapolation grid (User_Region.rds)
# Previous runs that produce Kmeans_knots and Kmeans_extrapolation should be kept in the directory for faster processing (but only if the extrapolation grid has not changed!)
setwd("/Users/janellemorano/MODEL_OUTPUT/_currentrun/")

library(VAST)
sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.10.2 units_0.7-2           
# VAST_3.8.2           
# TMB_1.7.22   
# Matrix_1.3-4
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects


# Data Prep
####################
# Data were compiled in Atlantic_menhaden_model_data.R
# data include NEAMAP and NEFSC

# sample data
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv", header = TRUE)
# covariate data
covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)

# check for NAs
# The model won't run if there are NA values for a covariate
sapply(covariate_data, function(x) sum(is.na(x)))
# Yes, there are NAs in Depth, Surftemp, Surfsalin, Bottemp, Botsalin.
# The large number missing values for Surftemp and Surfsalin are because these were not recorded for NEAMAP
# Just remove them for now
covariate_data <- na.omit(covariate_data)

# check for NAs in data, too
sapply(data, function(x) sum(is.na(x)))
# remove them
data <- na.omit(data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2021, and NEFSC is 1963-2019.
# Modify the dataset for only overlapping years, 2007-2019, AND dividing into SPRING and FALL seasons.
library(dplyr)

# SPRING
data.spring <- data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "SPRING")
covariate_data.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "SPRING")

# FALL
data.fall <- data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "FALL")
covariate_data.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
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
                 "b_i" = data.spring$Weight, #catch
                 "a_i" = data.spring$Areasw, #area swept
                 "v_i"= data.spring$Cruise,
                 "X1_formula" = X1_formula, #depth
                 "X2_formula" = X2_formula, #bottemp
                 "covariate_data" = covariate_data.spring,
                 "input_grid" = user_region)

# FALL
fit.fall = fit_model( "settings" = settings,
                 "Lat_i" = data.fall$Lat,
                 "Lon_i" = data.fall$Lon,
                 "t_i" = data.fall$Year, #time
                 "b_i" = data.fall$Weight, #catch
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

# Look at density estimates
summary_list <- summary(fit.spring)
# log biomass density estimates at each extrapolation grid cell
est_density <- summary_list$Density_dataframe
# This figure should match the Abundance Index figure
library(tidyverse)
ggplot(est_density, aes(Year, Density)) +
  geom_boxplot()

# Now compare where values for SS3 table come. Should be sum of biomass
total_biomass <- est_density %>%
  group_by(Year) %>%
  mutate(Density, funs(cumsum))
total_biomass <- aggregate(est_density$Density, by=list(Category=est_density$Year), FUN=sum)  

# Density Predictions
#############

# Spring
#############
## Remake map list locally for recreating plots
mdl <- make_map_info(Region = settings$Region,
                     spatial_list = fit.spring$spatial_list,
                     Extrapolation_List = fit.spring$extrapolation_list)

## Below shows to you get the model estimate of density, D_gct,
## for each grid (g), category (c; not used here single
## univariate); and year (t); and link it spatially to a lat/lon
## extrapolation point.  You can do this for any _gct or _gc
## variable in the Report.
names(fit.spring$Report)[grepl('_gc|_gct', x=names(fit.spring$Report))]

D_gt <- fit.spring$Report$D_gct[,1,] # drop the category
#dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years) #not working
## tidy way of doing this, reshape2::melt() does
## it cleanly but is deprecated
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "YearRep", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
PredDensity.spring <- D %>%
  mutate(Year = case_when(
    YearRep == "V1" ~ "2007",
    YearRep == "V2" ~ "2008",
    YearRep == "V3" ~ "2009",
    YearRep == "V4" ~ "2010",
    YearRep == "V5" ~ "2011",
    YearRep == "V6" ~ "2012",
    YearRep == "V7" ~ "2013",
    YearRep == "V8" ~ "2014",
    YearRep == "V9" ~ "2015",
    YearRep == "V10" ~ "2016",
    YearRep == "V11" ~ "2017",
    YearRep == "V12" ~ "2018",
    YearRep == "V13" ~ "2019",
  ))
PredDensity.spring <- PredDensity.spring %>%
  mutate(D, logD =log(D))
PredDensity.spring <- tibble(PredDensity.spring)
write_csv(PredDensity.spring, file = "PredDensity_spring.csv")
min(PredDensity.spring$logD) #-24.31025
max(PredDensity.spring$logD) #24.31025

# Fall
#############
## Remake map list locally for recreating plots
mdl.fall <- make_map_info(Region = settings$Region,
                     spatial_list = fit.fall$spatial_list,
                     Extrapolation_List = fit.fall$extrapolation_list)

## Below shows to you get the model estimate of density, D_gct,
## for each grid (g), category (c; not used here single
## univariate); and year (t); and link it spatially to a lat/lon
## extrapolation point.  You can do this for any _gct or _gc
## variable in the Report.
names(fit.fall$Report)[grepl('_gc|_gct', x=names(fit.fall$Report))]

D_gt <- fit.fall$Report$D_gct[,1,] # drop the category
#dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years) #not working
## tidy way of doing this, reshape2::melt() does
## it cleanly but is deprecated
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "YearRep", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
PredDensity.fall <- D %>%
  mutate(Year = case_when(
    YearRep == "V1" ~ "2007",
    YearRep == "V2" ~ "2008",
    YearRep == "V3" ~ "2009",
    YearRep == "V4" ~ "2010",
    YearRep == "V5" ~ "2011",
    YearRep == "V6" ~ "2012",
    YearRep == "V7" ~ "2013",
    YearRep == "V8" ~ "2014",
    YearRep == "V9" ~ "2015",
    YearRep == "V10" ~ "2016",
    YearRep == "V11" ~ "2017",
    YearRep == "V12" ~ "2018",
    YearRep == "V13" ~ "2019",
  ))
PredDensity.fall <- PredDensity.fall %>%
  mutate(D, logD =log(D))
PredDensity.fall <- tibble(PredDensity.fall)
write_csv(PredDensity.fall, file = "PredDensity_fall.csv")
min(PredDensity.fall$logD) #-24.53828
max(PredDensity.fall$logD) #33.16595


# Correcting scales between seasons
#############
# The max and min density between seasons is not equal, so to directly compare, re-scale the density

# Read in the density output to make sure I have what I need
den.spring <- read_csv("/Users/janellemorano/MODEL_OUTPUT/_currentrun/PredDensity_spring.csv")
den.fall <- read_csv("/Users/janellemorano/MODEL_OUTPUT/_currentrun/PredDensity_fall.csv")

# 33.2 was max value across both seasons
den.spring <- mutate(den.spring, CorrDen = logD/33)
max(den.spring$CorrDen)
# 0.7366744
min(den.spring$CorrDen)
den.fall <- mutate(den.fall, CorrDen = logD/33)
max(den.fall$CorrDen)
# 1.005029
min(den.fall$CorrDen)

# Now use den.spring and den.fall datasets and $CorrDen to graph predicted density

########
# Create basemap
library(tidyverse)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")

## Draw just 1 or 2 years for Spring
ggplot(data = world) +
  geom_sf(data = us) + 
  geom_sf(data = canada) +
  coord_sf (xlim = c(-83,-60), ylim = c (25,48), expand = FALSE ) +
  geom_point(data = subset(den.spring, Year %in% c(2007:2018)), 
             aes(Lon, Lat, color=CorrDen, group=NULL), #used to be log(D)
   ## These settings are necessary to avoid
   ## overlplotting which is a problem here. May need
   ## to be tweaked further.
   size=.5, stroke=0.5,shape=16) + 
  facet_wrap('Year', ncol = 1) +
  theme(strip.text.x = element_text(size = 6),
        strip.background = element_blank()) +
  scale_color_viridis_c(option = "magma", limit = c(-0.79, 1), oob = scales::squish) +
  theme_classic() +
  theme(axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Spring")

## Draw just 1 or 2 years for Fall
ggplot(data = world) +
  geom_sf(data = us) + 
  geom_sf(data = canada) +
  coord_sf (xlim = c(-83,-60), ylim = c (25,48), expand = FALSE ) +
  geom_point(data = subset(den.fall, Year %in% c(2019)), 
             aes(Lon, Lat, color=CorrDen), #used to be log(D)   #, group=NULL
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=.5, stroke=0.5,shape=16) + 
  facet_wrap(~Year, ncol = 1) +
  theme(strip.background = element_blank(), 
        strip.text = element_blank()) +
  scale_color_viridis_c(option = "magma", limit = c(-0.79, 1), oob = scales::squish) +
  theme_classic() +
  theme(axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Fall")

## Now draw all years
# I can't seem to modify the year labels
ggplot(data = world) +
  # geom_sf(data = us) + 
  # geom_sf(data = canada) +
  geom_sf() + # use this when not using "us" and "canada"
  coord_sf (xlim = c(-83,-60), ylim = c (30,48), expand = FALSE ) +
  geom_point(data = subset(den.fall, Year %in% c(2007:2018)), # if all data (data = den.spring, 
             aes(Lon, Lat, color=CorrDen, group=NULL), #used to be log(D)
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=.5, stroke=0.5,shape=16) + 
  facet_wrap('Year') +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank()) +
  # scale_color_continuous(type = "viridis") +
  scale_color_viridis_c(option = "magma") +
  theme_classic() + #theme_bw()
  theme(axis.text = element_blank()) #+
  # xlab("longitude") + 
  # ylab("latitude") 


# Center of Gravity
###############
fit.spring[["Report"]][["mean_Z_ctm"]]
# 1 is eastings
easting <- fit.spring[["Report"]][["mean_Z_ctm"]][,,1]
# 2 is northings
northing <- fit.spring[["Report"]][["mean_Z_ctm"]][,,2]
# Year, not sure where it is so I'm going to guess it's in order from low to high and create
year <- c(2007:2019)

cog <- data.frame(year, northing, easting)
write_csv(cog, file = "CenterofGravity_spring.csv")
ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
  geom_line()+
  theme_classic()

fit.fall[["Report"]][["mean_Z_ctm"]]
# 1 is eastings
easting <- fit.fall[["Report"]][["mean_Z_ctm"]][,,1]
# 2 is northings
northing <- fit.fall[["Report"]][["mean_Z_ctm"]][,,2]
# Year, not sure where it is so I'm going to guess it's in order from low to high and create
year <- c(2007:2019)

cog <- data.frame(year, northing, easting)
write_csv(cog, file = "CenterofGravity_fall.csv")
ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
  geom_line()+
  theme_classic()





#convert northing/easting to UTM
cord.dec = SpatialPoints(cbind(cog$long, -cog$lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))
cord.UTM

#northing
ggplot(cog, aes(x=year, y=northing)) +
  geom_line()
#easting
ggplot(cog, aes(x=year, y=easting)) +
  geom_line()
