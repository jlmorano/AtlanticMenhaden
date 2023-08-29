# to-run-models-on-another-computer.R
######################################
# Janelle L. Morano

# Load data, run models, save output

# last updated 26 July 2023
###############################################
###############################################

###########################################################################################
#Instructions
#-----  1. Move the "external-run" folder someplace and then setwd() to that path below.
#-----  2. Run all lines of code (anything commented out will not run and that is expected)
#-----  3. RDS files should be written to that folder when done

###########################################################################################


library(tidyverse)
library(mgcv)

#----- Set working directory path ------------------------------------------------------------
# Copy the path of the "external-run" folder in between the quotes.
setwd("")
#----- Load Data ------------------------------------------------------------
data.list <- readRDS("~/external-run/data.list.rds")



###########################################################################################
#----- Presence/Absence GAM ---------------------------------------------------------------
###########################################################################################


# #----- Run the models  ---------------------------------------------------------------
# # Federal spring and fall
# pa.gam.list <- list()
# pa.gam.summaries <- list()
# for (name in names(data.list[1:2])) {
  # pa.gam.list[[name = name]] = gam(Presence ~ s(Year, by = as.factor(State)) + s(Bottemp) + s(Depth) + as.factor(State), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  # pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
# }

# # State spring and fall
# # Append to previous list
# for (name in names(data.list[3:4])) {
  # pa.gam.list[[name = name]] = gam(Presence ~ s(Year, by = as.factor(State)) + s(SurfTemp) + s(Depth.m) + as.factor(State), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  # pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
# }

# #----- Save model runs as RDS
# saveRDS(pa.gam.list, file = "~/PA-GAM-results.rds")
# saveRDS(pa.gam.summaries, file = "~/PA-GAM-summaries.rds")


###########################################################################################
#----- Biomass GAM ---------------------------------------------------------------
###########################################################################################


#----- Run the models  ---------------------------------------------------------------
# Federal spring and fall
bgam.list <- list()
bgam.summaries <- list()
for (name in names(data.list[1:2])) {
  bgam.list[[name = name]] = gam(Biomass ~ s(Year, by = as.factor(State)) + s(Bottemp) + s(Depth) + as.factor(State), family ="quasipoisson"(link="log"), method = "REML", data = data.list[[name]])
  bgam.summaries[[name = name]] <- summary(bgam.list[[name]])
}

# State spring and fall
# Append to previous list
for (name in names(data.list[3:4])) {
  bgam.list[[name = name]] = gam(Weight.kg ~ s(Year, by = as.factor(State)) + s(SurfTemp) + s(Depth.m) + as.factor(State), family ="quasipoisson"(link="log"), data = data.list[[name]], method = "REML")
  bgam.summaries[[name = name]] <- summary(bgam.list[[name]])
}

saveRDS(bgam.list, file ="~/B-GAM-results.rds")
saveRDS(bgam.summaries, file ="~/B-GAM-summaries.rds")

