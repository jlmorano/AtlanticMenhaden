# Map-surveys-federal-state.R
######################################
# Janelle L. Morano

# Purpose: Map survey locations and regions

# Mapping, with strata shp files
#   NEFSC
#   NEAMAP
#   Connecticut Long Island Sound Trawl Survey (CTLISTS)
# Mapping, with only lat/long for survey locs
#   NYDEC Western Long Island
#   NJ Ocean Trawl
#   Delaware Bay
#   (not yet) Maryland Gill Net (lacks lat/long info)
#   ChesMMAP
#   (not yet) VIMS Shad Gill net
#   Georgia EMTS

# last updated 30 March 2023
###############################################
###############################################

library(sf)
library(tidyverse)
library(viridisLite)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)



##### Load Survey shapefiles and survey locations ------------------------------------------

# NEFSC
nefsc <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# Not sure difference between these shp
# strata2 <- st_read("/Users/janellemorano/DATA/NEFSC_bt_strata/finstr_nad83.shp")
# nefsc <- st_geometry(strata2)
# plot(strata2)

# NEAMAP
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")

# CTLISTS
ct <- st_read("/Users/janellemorano/DATA/CT Long Island Sound survey/LISTS_sitegrid_040822_JanelleMorano/sitegrid_unproj.shp")

# All other surveys with only Lat/Lon coordinates and no .shp
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", header = TRUE)


##### Map It! -----------------------------------------------------------------

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Map surveys
ggplot(data = world) +  
  geom_sf(data = us, fill = "grey") + #CCCC99
  geom_sf(data = canada, fill = "grey") +
  # geom_sf(data = nefsc, color = "black", fill = "#1f78b4") + 
  # geom_sf(data = neamap, color = "black", fill = "#b2df8a") +
  geom_point(data = state, aes(Longitude, Latitude, col = Survey), inherit.aes = FALSE, size = 0.3) + #color = "#253494"
  # coord_sf (xlim = c(-80,-64), ylim = c (32,46), expand = FALSE ) + #Full coast
  coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")




##### Centroids of strata -----------------------------------------------------------

plot(nefsc)
plot(st_centroid(nefsc), add = TRUE, pch = 3, col = 'red')
st_area(nefsc)


