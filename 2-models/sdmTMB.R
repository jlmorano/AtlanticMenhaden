# sdmTMB-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Objectives:
# Build spatial model in sdmTMB

# Primarily to compare with VAST model
# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 27 July 2023
###############################################
###############################################

# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

library(dplyr)
library(ggplot2)
library(sdmTMB)
library(sf)


#----- Data Prep ------------------------------------------------------------

menhaden <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230724.csv", header = TRUE)

# Remove NAs
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.)))

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972)
menhaden.fall <- menhaden %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)


#----- Make the mesh

mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("Latitude", "Longitude"), cutoff = 1)
plot(mesh.spring)
mesh.spring$mesh$n # extract number of vertices/knots
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("Latitude", "Longitude"), cutoff = 1)
plot(mesh.fall)

# Fancy mesh plot
# library(inlabru)
# ggplot() +
#   inlabru::gg(mesh.spring$mesh) +
#   geom_point(data = mesh.spring, aes(x = Latitude, y = Longitude, col = Biomass)) +
#   coord_equal()


#----- Model fit  ------------------------------------------------------------

#----- Spatial Model ----------
# for ref: GAM model...Biomass ~ s(Year) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log")
fit <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  family = tweedie(link = "log"), #nbinom2(link = "log")
  spatial = "on"
)
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


## Make predictions
# Make new data
newdf <- data.frame( unique(menhaden.spring[c('Latitude', 'Longitude')]))
newdf$Bottemp <- median(menhaden.spring$Bottemp)
head(newdf)

p <- predict(fit, newdata = newdf)
select(p, Latitude, Longitude, est:omega_s) %>%
  as_tibble()

# Following example
# ggplot(p, aes(Latitude, Longitude, color = exp(est))) + 
#   # geom_raster() +
#   geom_point() +
#   scale_color_viridis_c(trans = "sqrt") +
#   theme_classic() 

ggplot(p, aes(Latitude, Longitude)) + 
  geom_point(aes(color = exp(est))) +
  scale_color_viridis_c() +
  theme_classic() 


# # Import grid to make predictions over
# nd.grid <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
# nd.grid$Bottemp <- median(menhaden.spring$Bottemp)
# # Now predict over this grid
# pA <- predict(fit, newdata = nd.grid)


#----- Spatiotemporal Model  ----------
fit2 <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  family = tweedie(link = "log"),
  spatial = "off",
  spatiotemporal = "ar1"
)
fit2
tidy(fit2)
tidy(fit2, effects = "ran_pars")

## Basic sanity checks on model
sanity(fit2)

## Look at smoother effect in link space with randomized quantile partial residuals
visreg::visreg(fit2, xvar = "Bottemp")

# Or on the response scale
visreg::visreg(fit2, xvar = "Bottemp", scale = "response")

#----- Make predictions
# First build newdata for the predictions
temp <- newdf #copy unique lat/lon and mean bottemp from above
newdf2 <- temp #make a temp copy for loop below
years <- sort(unique(menhaden.spring$Year)) #collect years
newdf2$Year <- years[1] #add 1st year to newdf2 to start collecting

# Loop to amend the newdf2 prepped above
for (i in years[2:length(years)]) {
  temp$Year <- i #add the next year to the original list of lat/lon, bottemp
  newdf2 <- rbind(newdf2, temp)
}

# Now predict
p2 <- predict(fit2, newdata = newdf2)
select(p2, Latitude, Longitude, est:epsilon_st) %>%
  as_tibble()

# Plot predictions
ggplot(p2, aes(Latitude, Longitude)) + 
  geom_point(aes(color = exp(est))) +
  scale_color_viridis_c() +
  theme_classic()


#----- Presence-Absence Model  ----------
fit3 <- sdmTMB(
  presence ~ s(Bottemp),
  data = menhaden.spring,
  mesh = mesh.spring,
  family = binomial(link = "logit")
)
fit3
tidy(fit3)
tidy(fit3, effects = "ran_pars")

