# GAM-Statedata-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Using state surveys, Objectives:
# 1. Determine the affect of temperature and chlorophyll on menhaden abundance
# 2. Determine the spatial areas of highest menhaden density
# 3. Use GAM model of menhaden distribution

# last updated 29 March 2023
###############################################
###############################################

library(tidyverse)
library(janitor)



#----- Load State Data -------------------------------------------------

state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", header = TRUE)



#----- GAM of Count by smooth(Year), Water Temp ----------------------------------------

library(mgcv)

# Menhaden Catch over Years
state.gam = gam(MenhadenTotal ~ s(Year) + SurfTemp, data = statedata)
summary(state.gam)
plot(state.gam, main = "WLI & CTLITS")

# Plot with mgcViz
library(mgcViz)
b <- getViz(state.gam)
plot( sm(b, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  # scale_y_break(c(50, 3600)) +
  theme_classic()


#----- GAM of Count by smooth(Year), Water Temp, Survey (as a proxy for location)-------------

state.gam2 = gam(MenhadenTotal ~ s(Year) + SurfTemp + Survey, data = statedata)
summary(state.gam2)
plot(state.gam2, main = "WLI & CTLITS")

