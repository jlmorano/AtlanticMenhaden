# Figure1-surveymap.R
######################################
# Janelle L. Morano

# "Figure 1. Map of surveys" for menhaden distribution manuscript)

# last updated 1 July 2024
###############################################
###############################################

library(sf)
library(tidyverse)
library(viridisLite)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)



#----- Load Survey shapefiles and survey locations ------------------------------------------

### NEFSC
nefsc <- st_read("/Volumes/Eurybia/NEFSC strata/BTS_Strata.shp")
# STR2 are the strata numbers
st_crs(nefsc)
# CRS: NAD83 (EPSG,4269)

### NEAMAP
neamap <- st_read("/Volumes/Eurybia/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
st_crs(neamap)
# CRS: NAD83 (EPSG,4269)

### CTLISTS
ct <- st_read("/Volumes/Eurybia/CT Long Island Sound survey/LISTS_sitegrid_040822_JanelleMorano/sitegrid_unproj.shp")
st_crs(ct)
# CRS: NAD83 (EPSG,4269)

### All other surveys with only Lat/Lon coordinates and no .shp
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240701.csv", header = TRUE)
state.sf <- st_as_sf(state, coords = c("Longitude", "Latitude"), crs = 4269)
st_crs(state.sf)
# sapply(state, function(x) sum(is.na(x)))



#----- Generate Figure 1 -----------------------------------------------------------------
pal8 <- c("#b5de2b", "#6ece58", "#440154", "#482878", "#3e4989", "#31688e", "#26828e", "#1f9e89")
pal8lite <- adjustcolor(pal8, alpha.f =0.5) #make transparent

legendcols <- c(pal8[8], pal8[4])

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Map surveys
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = nefsc, color = "white", fill = pal8[8]) +
  geom_sf(data = neamap, color = pal8[1], fill = pal8lite[1]) +
  geom_sf(data = ct, color = pal8[4]) +
  geom_point(data = subset(state, Survey %in% "DEBay30ft"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "GAEMTS"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "MDGill"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "NCp915"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "SEAMAP"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  coord_sf (xlim = c(-83,-62), ylim = c (28,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  # theme_void() +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  # theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") 
# Legend will have to be added later

