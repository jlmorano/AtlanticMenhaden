# sdmTMB-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Objectives:
# Build spatial model in sdmTMB

# Primarily to compare with VAST model
# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 27 October 2023
###############################################
###############################################

# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

library(tidyverse)
library(ggplot2)
# install.packages("sdmTMB", dependencies = TRUE)
library(sdmTMB)
library(sf)
library(tictoc)
library(viridisLite)

sessionInfo()
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.5.2

# Set working directory on Mac
setwd("/Users/janellemorano")
# Set working directory on Cloud Server PC
setwd("D/:")



#----- Data Prep ------------------------------------------------------------

# Full dataset
# SKIP if using Test dataset
menhaden <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)
# For Virtual Windows Server
menhaden <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)


# Remove NAs
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.)))

# Make "State" a factor
menhaden$State <- as.factor(menhaden$State)

# add UTM
menhaden <- menhaden[-1]
get_crs(menhaden, c("Longitude", "Latitude"))
menhaden <- add_utm_columns(menhaden, c("Longitude", "Latitude"))
# convert UTM from meters (default) to km
# menhaden$X <- menhaden$X/1000
# menhaden$Y <- menhaden$Y/1000

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972)
menhaden.fall <- menhaden %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)

# # Create smaller set for testing, 2010-2020
# test.spring <- menhaden.spring %>%
#   filter(Year >=2010 & Year <=2020)
# test.fall <- menhaden.fall %>%
#   filter(Year >=2010 & Year <=2020) 
# # Write test data set
# write.csv(test.spring,"~/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", row.names = TRUE)
# write.csv(test.fall,"~/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", row.names = TRUE)

# # Read in test datasets created above
# menhaden.spring <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)
# # FOR VIRTUAL PC
# menhaden.spring <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)

#----- Make the mesh

mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("Latitude", "Longitude"), n_knots = 200, type = "cutoff_search") #500
# plot(mesh.spring)
# mesh.spring$mesh$n # extract number of vertices/knots
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("Latitude", "Longitude"), n_knots = 200, type = "cutoff_search")
# plot(mesh.fall)


#----- For Spatio-Temporal Models, Create Extrapolation Grid ---------------------------------

#----- Skip down to bottom to read in grid with years
# # Prepare prediction grid by repeating info for every year
# # on mac
# # nd.grid.yrs <- readRDS("~/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
# # on Virtual PC
# nd.grid.yrs <- readRDS("~/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
# 
# # Extract years
# years <- sort(unique(menhaden.spring$Year)) #collect years
# 
# # Prep some dfs to run loop properly
# temp <- nd.grid.yrs
# temp$Year <- years[1]
# new.nd.grid.yrs <- temp
# # Loop to amend the newdf2 prepped above
# for (i in years[2:length(years)]) {
#   temp$Year <- i #add the next year to the original list of lat/lon, bottemp
#   new.nd.grid.yrs <- rbind(new.nd.grid.yrs, temp)
# }
# # After checking, copy back to replace nd.grid
# nd.grid.yrs <- new.nd.grid.yrs
# 
# # Add Bottemp
# mediantemp <- menhaden.spring %>%
#   group_by(Year) %>%
#   summarise(Bottemp = median(Bottemp))
# 
# nd.grid.yrs <- nd.grid.yrs %>%
#   left_join(mediantemp, 
#             by = c("Year"),relationship = "many-to-many")
# 
# # Check for NAs
# sapply(nd.grid.yrs, function(x) sum(is.na(x)))
# 
# # Save as file
# saveRDS(nd.grid.yrs, "~/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years_NEFSC-NEAMAP.rds")

# Read in grid by years
nd.grid.yrs <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years_NEFSC-NEAMAP.rds")



#----- Model fit  ------------------------------------------------------------

#----- Poisson-link Delta Model ----------------------
tic()
delta.fit <- sdmTMB(
  Biomass ~ 0 + as.factor(Year) + s(log(Depth)), #1+ s(Bottemp) +s(Depth)
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  family = delta_poisson_link_gamma(), #delta_gamma(link1 = "logit", link2 = "log)"; alternative, delta_lognormal() with binomial(link="logit") & lognormal(link="log")
  # spatial = "on",
  # spatiotemporal = "iid",
  anisotropy = FALSE,
  reml = TRUE 
)
toc()
# 6821.11 sec elapsed sec elapsed s(Bottemp)+s(Depth)
# 576.69 sec elapsed s(Bottemp)
# 644.38 sec elapsed (poisson link, delta_poisson_link_gamma())

## Basic sanity checks on model
sanity(delta.fit)
#See `?run_extra_optimization()`
#ℹ Or refit with `control = sdmTMBcontrol(newton_loops = 1)`
#Try simplifying the model, adjusting the mesh, or adding priors

# Save and then move it to DATA storage
# # on Virtual PC
# saveRDS(delta.fit, file = "D:/MODEL_OUTPUT/delta-spatiotemporal-fit.rds")

# Read in delta.fit file
# delta.fit <- readRDS("~/MODEL_OUTPUT/delta-spatiotemporal-fit.rds")
# delta.fit <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/delta-spatiotemporal-fit.rds")
tidy(delta.fit)
param <- tidy(delta.fit, effects = "ran_pars", conf.int = TRUE)



## Make Predictions
nd.grid.yrs <- readRDS("D:/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years_NEFSC-NEAMAP.rds")
# Now predict
tic()
p.delta <- predict(delta.fit, newdata = nd.grid.yrs)
toc()
# 10.64 sec elapsed
saveRDS(p.delta, file = "D:/MODEL_OUTPUT/delta-spatiotemporal-predictions.rds" )

# Get the Index Standardization
delta.index <- get_index(p.delta, bias_correct = FALSE)

# Plot the conditional effects of covariates (depth, temp)
visreg_delta(delta.fit, xvar ="Bottemp", model = 1, gg = TRUE)
visreg_delta(delta.fit, xvar ="Depth", model = 1, gg = TRUE)


# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, color = {{ column }})) +
    geom_point() +
    coord_fixed()
}

ggplot(p.delta, aes(X, Y, color = exp(est))) +
  geom_point() +
  coord_fixed() +
  facet_wrap(~Year)

plot_map(p.delta, est) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "grey", limits = c(0, quantile(exp(p.delta$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(p.delta$est))))
  )

# Predictions with just fixed effects
plot_map(p.delta, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p.delta, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")








#----- Spatial Model ----------
# for ref: GAM model...Biomass ~ s(Year) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log")
tic()
fit <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  family = tweedie(link = "log"), #nbinom2(link = "log")
  spatial = "on"
)
toc()
fit

## Extract parameters as a data frame
tidy(fit, conf.int = TRUE)
tidy(fit, effects = "ran_pars", conf.int = TRUE)

## Basic sanity checks on model
sanity(fit)

## Look at smoother effect in link space with randomized quantile partial residuals
visreg::visreg(fit, xvar = "Bottemp")

# Or on the response scale
visreg::visreg(fit, xvar = "Bottemp", scale = "response")

# ## Make predictions on data
# p <- predict(fit)
# select(p, X, Y, est:omega_s) %>% as_tibble()

# Import grid to make predictions over
nd.grid <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/user_region_NEFSC-NEAMAP.rds")
nd.grid$Bottemp <- median(menhaden.spring$Bottemp)
nd.grid$Depth <- median(menhaden.spring$Depth) #Need to add correct bathymetry

# # Should I not convert?
# # convert UTM from meters (default) to km
# nd.grid$X <- nd.grid$X*1000
# nd.grid$Y <- nd.grid$Y*1000

# Now predict over this grid
pgrid <- predict(fit, newdata = nd.grid)

# Plot predictions
ggplot(pgrid, aes(X, Y, color = plogis(est))) +
	geom_point() +
  scale_color_viridis_c() +
  coord_fixed() +
  theme_classic()

# Plot main effects contributions
# !!! Because my bathymetry is incorrect in the prediction grid, this isn't going to look correct!!
ggplot(pgrid, aes(X, Y, color = plogis(est_non_rf))) +
  geom_point() +
  scale_color_viridis_c() +
  coord_fixed() +
  theme_classic()

# Plot spatial random effects
ggplot(pgrid, aes(X, Y, color = omega_s)) +
  geom_point() +
  scale_color_viridis_c() +
  coord_fixed() +
  theme_classic()

# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, color = {{ column }})) +
    geom_point() +
    coord_fixed()
}


# Predictions with all fixed and random effects
# Subset a few years
# p2a <- p2 %>% filter(Year > 2010)

ggplot(p2, aes(X, Y, color = exp(est))) +
  geom_point() +
  coord_fixed() +
  facet_wrap(~Year)

plot_map(p2, est) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "grey", limits = c(0, quantile(exp(p2$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(p2$est))))
  )

# Predictions with just fixed effects
plot_map(p2, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")




#----- Spatiotemporal Model  ----------
tic()
st.fit <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  family = tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  spatial = "on",
  spatiotemporal = "iid"
)
toc()
#287.46 sec elapsed
sanity(st.fit)

# Save and then move it to DATA storage
# on mac
# saveRDS(fit2, file = "/Users/janellemorano/MODEL_OUTPUT/_currentrun/fit2-spatiotemporal.rds")
# on Virtual PC
saveRDS(fit2, file = "D:/MODEL_OUTPUT/fit2-spatiotemporal-test.rds" )
fit2 <- readRDS("D:/MODEL_OUTPUT/fit2-spatiotemporal-test.rds")
# fit2 <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/fit2-spatiotemporal.rds")
# fit2
tidy(fit2)
param <- tidy(fit2, effects = "ran_pars", conf.int = TRUE)

## Basic sanity checks on model
sanity(fit2)

## Look at smoother effect in link space with randomized quantile partial residuals
visreg::visreg(fit2, xvar = "Bottemp")

# Or on the response scale
visreg::visreg(fit2, xvar = "Bottemp", scale = "response")

#----- Make predictions

# Prepare prediction grid by repeating info for every year
# on mac
# nd.grid.yrs <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
# on Virtual PC
nd.grid.yrs <- readRDS("D:/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
years <- sort(unique(menhaden.spring$Year)) #collect years

# Prep some dfs to run loop properly
temp <- nd.grid.yrs
temp$Year <- years[1]
new.nd.grid.yrs <- temp
# Loop to amend the newdf2 prepped above
for (i in years[2:length(years)]) {
  temp$Year <- i #add the next year to the original list of lat/lon, bottemp
  new.nd.grid.yrs <- rbind(new.nd.grid.yrs, temp)
}
# After checking, copy back to replace nd.grid
nd.grid.yrs <- new.nd.grid.yrs

# Add Bottemp
mediantemp <- menhaden.spring %>%
  group_by(Year) %>%
  summarise(Bottemp = median(Bottemp))

nd.grid.yrs <- nd.grid.yrs %>%
  left_join(mediantemp, 
            by = c("Year"),relationship = "many-to-many")
saveRDS("D:/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")


# Now predict
tic()
p2 <- predict(fit2, newdata = nd.grid.yrs)
toc()
# on Virtual PC
saveRDS(p2, file = "D:/Atlantic_menhaden_modeling/3-model-output-save/fit2-spatiotemporal-predictions.rds" )
# saveRDS(p2, file = "/Users/janellemorano/MODEL_OUTPUT/_currentrun/predictions.fit2-spatiotemporal.rds")
p2 <- readRDS(p2, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/predictions.fit2-spatiotemporal.rds")
select(p2, Latitude, Longitude, est:epsilon_st) %>%
  as_tibble()

# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, color = {{ column }})) +
    geom_point() +
    coord_fixed()
}


# Predictions with all fixed and random effects
# Subset a few years
# p2a <- p2 %>% filter(Year > 2010)

ggplot(p2, aes(X, Y, color = exp(est))) +
  geom_point() +
  coord_fixed() +
  facet_wrap(~Year)

plot_map(p2, est) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "grey", limits = c(0, quantile(exp(p2$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(p2$est))))
  )

# Predictions with just fixed effects
plot_map(p2, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")



#----- Presence-Absence Model  ----------
tic()
pa.fit <- sdmTMB(
  Presence ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "Year",
  spatiotemporal = "AR1"
)
toc()
#2052.41 sec elapsed
sanity(pa.fit)
pa.fit

tidy(pa.fit)
# Spatiotemporal model fit by ML ['sdmTMB']
# Formula: Presence ~ s(Bottemp) + s(Depth)
# Mesh: mesh.spring (isotropic covariance)
# Time column: Year
# Data: menhaden.spring
# Family: binomial(link = 'logit')
# 
# coef.est coef.se
# (Intercept)   -14.53    2.52
# sBottemp      -31.06   50.45
# sDepth          0.50  186.34
# 
# Smooth terms:
#   Std. Dev.
# sds(Bottemp)     37.55
# sds(Depth)       72.84
# 
# Spatiotemporal AR1 correlation (rho): 0.21
# Matern range: 0.96
# Spatial SD: 8.48
# Spatiotemporal SD: 9.12
# ML criterion at convergence: 3372.987
tidy(pa.fit, effects = "ran_pars")
# term    estimate std.error
# <chr>      <dbl> <lgl>    
#   1 range      0.957 NA       
# 2 sigma_O    8.48  NA       
# 3 sigma_E    9.12  NA       
# 4 rho        0.211 NA 

menhaden.spring$resids <- residuals(pa.fit) # randomized quantile residuals
qqnorm(menhaden.spring$resids)
qqline(menhaden.spring$resids)

saveRDS(pa.fit, "D:/MODEL_OUTPUT/pa-spatiotemporal-fit.rds")

# Now predict
tic()
pa.pred <- predict(pa.fit, newdata = nd.grid.yrs)
toc()
# 152 sec elapsed
saveRDS(pa.pred, file = "D:/MODEL_OUTPUT/pa-spatiotemporal-predictions.rds")
select(pa.pred, Latitude, Longitude, est:epsilon_st) %>%
  as_tibble()

# First check that the prediction grid is as expected
ggplot(pa.pred.sub, aes(X, Y)) +
  geom_point(aes(color = Depth)) + 
  scale_fill_viridis_c() +
  facet_wrap(~Year) +
  ggtitle("Prediction Grid Bottemp")


# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, color = {{ column }})) +
    geom_point() +
    coord_fixed()
}

# Predictions with all fixed and random effects
# Subset a few years
pa.pred.sub <- pa.pred %>% filter(Year > 2010)

plot_map(pa.pred.sub, est) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "grey", limits = c(0, quantile(exp(pa.pred.sub$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(pa.pred.sub$est))))
  )

# Predictions with just fixed effects
plot_map(pa.pred.sub, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")


