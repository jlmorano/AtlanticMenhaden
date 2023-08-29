# NEFSC-NEAMAP-prediction-grid-for-menhaden-model.R
######################################
# Janelle L. Morano

# Prediction grid for combined extent of NEFSC and NEAMAP surveys
# Use in sdmTMB
# sf() compatible (*sp and rgdal are depreciated)

# last updated 17 July 2023
###############################################
###############################################

library(tidyverse)
library(sf)
packageVersion('sf') 
# 1.0.13



#----- Read in shapefiles
# NEFSC
nefsc.shp <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
colnames(nefsc.shp)

# NEAMAP
neamap.shp <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
colnames(neamap.shp)


#----- Check projections are the same
identical(st_crs(nefsc.shp), st_crs(neamap.shp))
# TRUE
# st_crs(nefsc.shp)
# st_crs(neamap.shp)
# EPSG 4269


#----- Convert shapefile to simple features
nefsc.sf <- st_as_sf(nefsc.shp)
neamap.sf <- st_as_sf(neamap.shp)


#----- Spatial join NEFSC & NEAMAP extent
# This is to deal with the multipolygon issue of the shapefiles. More reading is necessary, but seems to be the only way to get this to work for these particular shapefiles.
sf::sf_use_s2(FALSE)

# This should result in essentially the NEFSC extent, but there is a little near the northern edge that is added by NEAMAP
fedunion <- st_union(nefsc.sf$geometry, neamap.sf$geometry)
plot(fedunion)


#----- Join NEFSC & NEAMAP and then Make grid
# The processing takes longer, but is more accurate than making a grid for each, then joining those
fedgrid <- fedunion %>% 
  st_make_grid(cellsize = 0.15, what = "centers") %>% # grid of points
  st_intersection(fedunion) # only within the polygon
ggplot() +
  geom_sf(data = fedgrid)


#----- Convert grid to dataframe with lat/lon
fedgrid.LL <- fedgrid %>%
  # 1. Project to lon/lat (using the eps of the original shp)
  st_transform(4269) %>%
  # 2 Extract coordinates
  st_coordinates() %>%
  # 3 to table /tibble
  as.data.frame()


#----- Write grid as rds file as a data.frame class for sdmTMB
saveRDS(fedgrid.LL, file = "/Users/janellemorano/Git/AtlanticMenhaden/1-extrapolation-grids/grid_NEFSC-NEAMAP.rds")

#----- Add columns appropriate for VAST
nefsc.gridVAST <- fedgrid.LL %>%
  rename(Lon = X,
         Lat = Y) %>%
  mutate(Area_km2 = ((0.15/1000)^2),
         row = 1:nrow(fedgrid.LL))
### Save it to be read in and passed to VAST later.
saveRDS(nefsc.gridVAST, file = "/Users/janellemorano/Git/AtlanticMenhaden/user_region_NEFSC-NEAMAP.rds")


#----- Check for distance between points
# Specifically, in qcs_grid in sdmTMB
# Remember that UTM is in meters
library(sdmTMB)
sqrt((qcs_grid$X[2:4] - qcs_grid$X[1:4-1]) ^ 2 + (qcs_grid$Y[2:4] - qcs_grid$Y[1:4-1]) ^ 2)
# 2 meters between points?!




