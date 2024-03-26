# Ches-prediction-grid-for-menhaden-model.R
######################################
# Janelle L. Morano

# Prediction grid for combined extent of NEFSC and NEAMAP surveys
# Use in sdmTMB
# sf() compatible (*sp and rgdal are depreciated as of Oct 2023)

# last updated 14 February 2024
###############################################
###############################################

library(tidyverse)
library(sf)
packageVersion('sf') 
# 1.0.13
library(sdmTMB)
packageVersion('sdmTMB')


#----- Read in ChesMap data from already cleaned state data
state <- read.csv("/Volumes/Eurybia/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20230727.csv", header = TRUE)

ch <- state |> filter(Survey == "ChesMMAP") 

plot(ch$Longitude, ch$Latitude)

#----- Draw points around the extent
LL <- locator()
saveRDS(LL, 'extent_LL.rds')
LL <- readRDS('extent_LL.rds')

# Take a data.frame of coordinates in longitude/latitude
df <- data.frame(long=LL$x, lat=LL$y)

# Duplicate the first point to connect
df <- rbind(df, df[1,])

# Convert to a matrix
mat <- data.matrix(df)

# Turn it into a spatial polygon object
pol <- st_polygon(list(mat))
region <- st_sfc(pol, crs = 4269)
plot(region)

#----- Make a grid over the ChesMMAP survey extent
region.grid <- st_make_grid(region, cellsize = 0.03, what = "centers")
plot(region.grid)

# Intersect grid and survey extent
sf_use_s2(FALSE)
chesgrid <- st_intersection(region.grid, region)
plot(chesgrid)

#----- Convert grid to dataframe with lat/lon
chesgrid.LL <- chesgrid %>%
  # 1. Project to lon/lat (using the eps of the original shp)
  st_transform(4269) %>%
  # 2 Extract coordinates
  st_coordinates() %>%
  # 3 to table /tibble
  as.data.frame()

# Add UTM
names(chesgrid.LL)[1] <- "Longitude"
names(chesgrid.LL)[2] <- "Latitude"
chesgrid.LL <- sdmTMB::add_utm_columns(chesgrid.LL, c("Longitude", "Latitude"))


#----- Add bathymetry
bathy <- terra::rast("/Volumes/Eurybia/Bathymetry-GEBCO_19_Oct_2023_8946e1573d02 2/gebco_2023_n45.0_s32.0_w-79.11_e-65.0.tif")

## Get depth from raster for each lat/lon point in fedgrid.LL
# Grab just lat/lon from fedgrid.LL
points <- chesgrid.LL %>%
  select(Longitude, Latitude) %>%
  rename(x = Longitude,
         y = Latitude)

# Get depth from bathy for each loc in points
depthpoints <- terra::extract(bathy, points)

# Bind back to fedgrid.LL and clean up
chesgrid.LL <- cbind(chesgrid.LL, depthpoints)

# Check for NAs
sapply(chesgrid.LL, function(x) sum(is.na(x)))
chesgrid.LL %>% filter_all(any_vars(is.na(.)))
chesgrid.LL <- na.omit(chesgrid.LL)

# Rename and Drop unneccessary columns
chesgrid.LL$depth <- chesgrid.LL[,6]
chesgrid.LL <- chesgrid.LL[-c(5:6)]

# Change "depth" (ie bathymetry) values from negative to positive, to make them true depth values
chesgrid.LL <- chesgrid.LL %>%
  mutate(Depth = case_when(depth >0 ~ 0,
                           depth <0 ~ depth * -1))
# Delete old depth col
chesgrid.LL <- chesgrid.LL[-5]


#----- Save grid with depth for each X,Y as a data.frame class for sdmTMB --------------------------------
saveRDS(chesgrid.LL, "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_Chesmap.rds")

chesgrid <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_Chesmap.rds")
