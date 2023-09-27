# sdmTMB-Atlantic-menhaden-server.R
######################################
# Janelle L. Morano

# Objectives:
# Build spatial model in sdmTMB

###########
#### FOR RUNNING ON REMOTE WINDOWS SERVER!!!!
###########

# Primarily to compare with VAST model
# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 26 September 2023
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


#----- Make the mesh

mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("Latitude", "Longitude"), n_knots = 200, type = "cutoff_search") #500
plot(mesh.spring)
mesh.spring$mesh$n # extract number of vertices/knots
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("Latitude", "Longitude"), n_knots = 200, type = "cutoff_search")
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

# ## Make predictions on data
# p <- predict(fit)
# select(p, X, Y, est:omega_s) %>% as_tibble()

# Import grid to make predictions over
nd.grid <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
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




#----- Spatiotemporal Model  ----------
fit2 <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  family = tweedie(link = "log"),
  spatial = "on",
  spatiotemporal = "ar1"
)
saveRDS(fit2, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/sdmTMBoutput/fit2-spatiotemporal.rds")
# fit2 <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/sdmTMBoutput/fit2-spatiotemporal.rds")
fit2
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
nd.grid.yrs <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")
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

# Add Bathymetry depth
# Need to add correct bathymetry, but this cluge with work for now
mediandepth <- menhaden.spring %>%
  group_by(Longitude) %>%
  summarise(Depth = median(Depth)) %>%
  mutate(across(Longitude, round, 0)) %>%
  rename(LongRd = Longitude)

nd.grid.yrs$LongRd <- nd.grid.yrs$Longitude

nd.grid.yrs <- nd.grid.yrs %>%
  mutate(across(LongRd, round, 0)) %>%
  left_join(mediandepth, 
            by = c("LongRd"),relationship = "many-to-many")
nd.grid.yrs <- nd.grid.yrs[, -7]


# Now predict
# p2 <- predict(fit2, newdata = nd.grid.yrs)
# saveRDS(p2, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/sdmTMBoutput/predictions.fit2-spatiotemporal.rds")
p2 <- readRDS(p2, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/sdmTMBoutput/predictions.fit2-spatiotemporal.rds")
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
p2a <- p2 %>% filter(Year > 2010)

ggplot(p2a, aes(X, Y, color = exp(est))) +
  geom_point() +
  coord_fixed() +
  facet_wrap(~Year)

plot_map(p2a, exp(est)) +
  scale_fill_viridis_c(
    # trans = "sqrt",
    # # trim extreme high values to make spatial variation more visible
    # na.value = "yellow", limits = c(0, quantile(exp(p2a$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(p2a$est))))
  )

# Predictions with just fixed effects
plot_map(p2a, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2a, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")

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




############
# Keep for now
# # SKIP- Use Grid Instead- Build newdata for the predictions
# newdf2 <- data.frame( unique(menhaden.spring[c('Latitude', 'Longitude')])) #unique lat/lon from data
# newdf2 <- newdf2 %>%
#   left_join(menhaden.spring, 
#             by = c('Latitude', 'Longitude'))
# newdf2 <- newdf2 %>% select('Latitude', 'Longitude', 'Depth')
# newdf2$Bottemp <- median(menhaden.spring$Bottemp) #median Bottemp
# temp <- newdf2
# years <- sort(unique(menhaden.spring$Year)) #collect years
# newdf2$Year <- years[1] #add 1st year to newdf2 to start collecting
# 
# # Loop to amend the newdf2 prepped above
# for (i in years[2:length(years)]) {
#   temp$Year <- i #add the next year to the original list of lat/lon, bottemp
#   newdf2 <- rbind(newdf2, temp)
# }


