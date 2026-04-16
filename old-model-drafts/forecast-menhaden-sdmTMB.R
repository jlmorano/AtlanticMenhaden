# forecast-menhaden-sdmTMB.R
######################################
# Janelle L. Morano

# Objectives:
# forecast (testing) of menhaden

# last updated 11 November 2024

###############################################
###############################################

library(sdmTMB)
packageVersion('sdmTMB') #‘0.6.0’ 
library(tidyverse)
library(janitor)
library(tictoc)

# Full dataset
menhaden <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240819.csv", header = TRUE)

# Remove NAs and amend column headings to lower case to avoid problems (old bug, but keeping for ease)
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.))) %>%
  janitor::clean_names(case = c("lower_camel")) %>%
  # Add vessel column
  mutate(vessel = case_when(survey == "NEAMAP" ~ "NEAMAP",
                            survey == "NEFSC" & year < 2009 ~ "Albatross",
                            survey == "NEFSC" & year >= 2009 ~ "Bigelow"))

# Make "state" and "vessel" a factor
menhaden$state <- as.factor(menhaden$state)
menhaden$vessel <- as.factor(menhaden$vessel)

# add UTM
menhaden <- menhaden[-1]
get_crs(menhaden, c("longitude", "latitude"))
# Suggests UTM zone 19N; CRS = 32619
menhaden <- sdmTMB::add_utm_columns(menhaden, c("longitude", "latitude"))
# Move X, Y columns because there was a bug with cog(), but now keeping for cleanliness
colord <- c( "X", "Y", "survey", "vessel", "cruise", "station", "stratum", "inoffshore", "state", "year", "season", "latitude", "longitude", "areasw", "depth", "surftemp", "surfsalin", "bottemp", "botsalin", "abundance", "biomass", "presence", "centroidLat", "centroidLon") 
menhaden <- menhaden[, colord]

# Create test Fall datasets, 2010-2020
menhaden.fall <- menhaden %>%
  filter(season == "FALL") %>%
  filter(year >=2010)

# Mesh
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("X", "Y"), n_knots = 150, type = "cutoff_search")
plot(mesh.fall)

# Extrapolation grid
nd.grid.yrs.fall <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP.rds")
nd.grid.yrs.fall2 <- nd.grid.yrs.fall %>%
  filter(Year >=2010)
nd.grid.yrs.fall <- nd.grid.yrs.fall %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")


# Zeros out epsilon, keeps omega