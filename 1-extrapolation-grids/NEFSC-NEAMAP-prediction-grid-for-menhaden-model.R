# NEFSC-NEAMAP-prediction-grid-for-menhaden-model.R
######################################
# Janelle L. Morano

# Prediction grid for combined extent of NEFSC and NEAMAP surveys
# Use in sdmTMB
# sf() compatible (*sp and rgdal are depreciated as of Oct 2023)

# last updated 4 January 2024
###############################################
###############################################

library(tidyverse)
library(sf)
packageVersion('sf') 
# 1.0.13
library(sdmTMB)
packageVersion('sdmTMB')
# ‘0.4.0’
library(terra)
packageVersion('terra')
# ‘1.7.29’



#----- Read in shapefiles
# NEFSC
nefsc.shp <- st_read("/Volumes/Eurybia/NEFSC strata/BTS_Strata.shp")
colnames(nefsc.shp)

# NEAMAP
neamap.shp <- st_read("/Volumes/Eurybia/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
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

# Project fedunion
fedunion <- st_transform(fedunion, crs=st_crs(4269))

# Dissolve interior borders
fedunion.ext <- st_union(fedunion)
plot(fedunion.ext)


#----- Make a 10x10 km grid over the NEFSC & NEAMAP survey extent
# Extract the bounding box coordinates
fedunion.bb <- st_bbox(fedunion.ext)

# Convert to a spatial feature of a grid covering the extend of the bounding box with a single grid cell
fedunion.bb.grid <- st_make_grid(fedunion.bb, n=1)

# Make a new grid of 10x10 km
fedunion.grid <- st_make_grid(fedunion.bb.grid, cellsize = 0.2, what = "centers") #ideally, move to 0.01 but it takes forever to process
plot(fedunion.grid)

# Intersect grid and survey extent
fedgrid <- st_intersection(fedunion.grid, fedunion.ext)
plot(fedgrid)


#----- Convert grid to dataframe with lat/lon
fedgrid.LL <- fedgrid %>%
  # 1. Project to lon/lat (using the eps of the original shp)
  st_transform(4269) %>%
  # 2 Extract coordinates
  st_coordinates() %>%
  # 3 to table /tibble
  as.data.frame()

# Add UTM
names(fedgrid.LL)[1] <- "Longitude"
names(fedgrid.LL)[2] <- "Latitude"
fedgrid.LL <- sdmTMB::add_utm_columns(fedgrid.LL, c("Longitude", "Latitude"))
# fedgrid.LL$X <- round(fedgrid.LL$X)
# fedgrid.LL$Y <- round(fedgrid.LL$Y) 



#----- Check for distance between points
# Specifically, in qcs_grid in sdmTMB
# Remember that UTM is in kilometers
# sqrt((qcs_grid$X[2:4] - qcs_grid$X[1:4-1]) ^ 2 + (qcs_grid$Y[2:4] - qcs_grid$Y[1:4-1]) ^ 2)
# 2 km


#----- Add bathymetry
bathy <- terra::rast("/Volumes/Eurybia/Bathymetry-GEBCO_19_Oct_2023_8946e1573d02 2/gebco_2023_n45.0_s32.0_w-79.11_e-65.0.tif")

## Get depth from raster for each lat/lon point in fedgrid.LL
# Grab just lat/lon from fedgrid.LL
points <- fedgrid.LL %>%
  select(Longitude, Latitude) %>%
  rename(x = Longitude,
         y = Latitude)

# Get depth from bathy for each loc in points
depthpoints <- terra::extract(bathy, points)

# Bind back to fedgrid.LL and clean up
fedgrid.LL <- cbind(fedgrid.LL, depthpoints)

# Check for NAs
sapply(fedgrid.LL, function(x) sum(is.na(x)))
fedgrid.LL %>% filter_all(any_vars(is.na(.)))
# This is a strange point, don't know how it got in, so it's being dropped
fedgrid.LL <- na.omit(fedgrid.LL)

# Drop unneccessary columns
fedgrid.LL$depth <- fedgrid.LL[,6]
fedgrid.LL <- fedgrid.LL[-c(5:6)]

# Change "depth" (ie bathymetry) values from negative to positive, to make them true depth values
fedgrid.LL <- fedgrid.LL %>%
  mutate(Depth = case_when(depth >0 ~ 0,
                           depth <0 ~ depth * -1))
# Delete old depth col
fedgrid.LL <- fedgrid.LL[-5]

#----- Save grid with depth for each X,Y as a data.frame class for sdmTMB --------------------------------
saveRDS(fedgrid.LL, "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP-2.rds")


#----- Add columns appropriate for VAST
nefsc.gridVAST <- fedgrid.LL %>%
  rename(Lon = X,
         Lat = Y) %>%
  mutate(Area_km2 = ((0.15/1000)^2),
         row = 1:nrow(fedgrid.LL))
### Save it to be read in and passed to VAST later.
saveRDS(nefsc.gridVAST, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/user_region_NEFSC-NEAMAP.rds")



#--------------------------------------------------------------------
  
  
#----- Create grid with depth & temp for every year ----------------------------------------         

#----- First, repeat bathymetry grid for each year
# Read in grid with depth and X,Y
nd.grid.yrs <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NEFSC-NEAMAP-2.rds")

# Read in menhaden data with temp to extract the years being used
menhaden <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)

# Keep only 1972-2021
menhaden <- menhaden %>%
  filter(Year >=1972 & Year <=2021)

# Extract years
years <- sort(unique(menhaden$Year)) #collect years

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


#----- Add Bottom Temperature
# # First, use bottom temperature from sample points and add to the extrapolation grid
# 
# # Create separate spring and fall grids
# nd.grid.yrs.spring <- nd.grid.yrs %>% mutate(Season = "SPRING")
# nd.grid.yrs.fall <- nd.grid.yrs %>% mutate(Season = "FALL")
# 
# # Create a subset of menhaden data to make the join easier
# merge.sp <- menhaden |>
#   filter(Season == "SPRING") |>
#   select(Longitude, Latitude, Year, Bottemp)
# # Add X, Y in UTM to menhaden
# merge.sp <- sdmTMB::add_utm_columns(merge.sp, c("Longitude", "Latitude"))
# 
# # Join menhaden to season grids
# test <- nd.grid.yrs.spring |>
#   left_join(merge.sp)
# 
# nd.grid.yrs.fall <- nd.grid.yrs |>
#   left_join(mediantemp.fall |> select(AdjustBottemp))

# Calculate average bottom temp by strata using centriod lat/lon
mediantemp <- menhaden %>%
  group_by(Season, Year, CentroidLat, CentroidLon, Depth) %>%
  summarise(Bottemp = median(Bottemp))
# How many NAs? Quite a few
sapply(mediantemp, function(x) sum(is.na(x)))
# Remove missing CentroidLat & CentroidLon
mediantemp <- mediantemp %>%
  drop_na(CentroidLat, CentroidLon, Depth)

# For missing Bottemp rows, fill with adjacent Bottemp
mediantemp <- mediantemp %>%
  mutate(AdjustBottemp = if_else(is.na(Bottemp), lag(Bottemp, n =1, order_by = CentroidLat), Bottemp))
# How many NAs?
sapply(mediantemp, function(x) sum(is.na(x)))

# Before addressing remaining NA, join temp in mediantemp to nd.grid.yrs data
# First, create new cols in each that round centroid lat/lon to increase matches when joining
mediantemp <- mediantemp %>% mutate(rdLat = round(CentroidLat, 1),
                                      rdLon = round(CentroidLon, 1))
nd.grid.yrs <- nd.grid.yrs %>% mutate(rdLat = round(Latitude, 1),
                                    rdLon = round(Longitude, 1))

# Separate spring and fall
mediantemp.spring <- mediantemp %>% filter(Season == "SPRING")
mediantemp.fall <- mediantemp %>% filter(Season == "FALL")

# Join AdjustBottemp to season grids
nd.grid.yrs.spring <- nd.grid.yrs |>
  left_join(mediantemp.spring |> select(AdjustBottemp))
nd.grid.yrs.fall <- nd.grid.yrs |>
  left_join(mediantemp.fall |> select(AdjustBottemp))

# Now, to address the remaining NAs,
## Spring
nd.grid.yrs.spring %>% summarise(mean(AdjustBottemp, na.rm = TRUE))
# 7.453442
nd.grid.yrs.spring <- nd.grid.yrs.spring |>
  mutate(AdjustBottemp = if_else(is.na(AdjustBottemp), 7.45, AdjustBottemp))
sapply(nd.grid.yrs.spring, function(x) sum(is.na(x)))
# Clean up
nd.grid.yrs.spring <- nd.grid.yrs.spring %>%
  select(Longitude, Latitude, X, Y, Depth, Year, Season, AdjustBottemp) %>%
  rename(Bottemp = AdjustBottemp)

## Fall
nd.grid.yrs.fall %>% summarise(mean(AdjustBottemp, na.rm = TRUE))
# 13.60185
nd.grid.yrs.fall <- nd.grid.yrs.fall |>
  mutate(AdjustBottemp = if_else(is.na(AdjustBottemp), 13.60, AdjustBottemp))
sapply(nd.grid.yrs.fall, function(x) sum(is.na(x)))
# Clean up
nd.grid.yrs.fall <- nd.grid.yrs.fall %>%
  select(Longitude, Latitude, X, Y, Depth, Year, Season, AdjustBottemp) %>%
  rename(Bottemp = AdjustBottemp)


#----- Verify that the prediction grid is as expected
# Spring
# Subset a few years
nd.grid.yrs.spring.sub <- nd.grid.yrs.spring %>% filter(Year > 2010)
ggplot() +
  geom_point( data = nd.grid.yrs.spring.sub, aes(X, Y, color = log(Depth)), size = 0.25) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~Year) +
  ggtitle("Spring Prediction Grid Depth")
ggplot() +
  geom_point( data = nd.grid.yrs.spring.sub, aes(X, Y, color = Bottemp), size = 0.25) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~Year) +
  ggtitle("Spring Prediction Grid Bottemp")

# Fall
# Subset a few years
nd.grid.yrs.fall.sub <- nd.grid.yrs.fall %>% filter(Year > 2017)
ggplot() +
  geom_point( data = nd.grid.yrs.fall.sub, aes(X, Y, color = log(Depth)), size = 0.25) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~Year) +
  ggtitle("Fall Prediction Grid Depth")
ggplot() +
  geom_point( data = nd.grid.yrs.fall.sub, aes(X, Y, color = Bottemp), size = 0.25) +
  scale_color_viridis_c(direction = -1) +
  facet_wrap(~Year) +
  ggtitle("Fall Prediction Grid Bottemp")


#----- Write grid as rds file as a data.frame class for sdmTMB
# Spring
saveRDS(nd.grid.yrs.spring, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP-2.rds")

# Fall
saveRDS(nd.grid.yrs.fall, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP_byyears_fall-2.rds")
