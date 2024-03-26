# NJOceanTrawl-prediction-grid-for-menhaden-model.R
######################################
# Janelle L. Morano

# Prediction grid for combined extent of NEFSC and NEAMAP surveys
# Use in sdmTMB
# sf() compatible (*sp and rgdal are depreciated as of Oct 2023)

# last updated 9 January 2024
###############################################
###############################################

library(tidyverse)
library(sf)
packageVersion('sf') 
# 1.0.13
library(sdmTMB)
packageVersion('sdmTMB')
# ‘0.4.0’
# library(terra)
# packageVersion('terra')
# ‘1.7.29’

#----- Read in NJ data from already cleaned state data
state <- read.csv("/Volumes/Eurybia/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20230727.csv", header = TRUE)

nj <- state |> filter(Survey == "NJOT") |>
  filter(Longitude <0) #Remove 2 odd points

plot(nj$Longitude, nj$Latitude)

#----- Draw points around the extent
#LL <- locator()
#saveRDS(LL, 'extent_LL.rds')
#LL <- readRDS('extent_LL.rds')

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

#----- Make a grid over the NJ Ocean Trawl survey extent
region.grid <- st_make_grid(region, cellsize = 0.03, what = "centers")
plot(region.grid)

# Intersect grid and survey extent
sf_use_s2(FALSE)
njgrid <- st_intersection(region.grid, region)
plot(njgrid)


#----- Convert grid to dataframe with lat/lon
njgrid.LL <- njgrid %>%
  # 1. Project to lon/lat (using the eps of the original shp)
  st_transform(4269) %>%
  # 2 Extract coordinates
  st_coordinates() %>%
  # 3 to table /tibble
  as.data.frame()

# Add UTM
names(njgrid.LL)[1] <- "Longitude"
names(njgrid.LL)[2] <- "Latitude"
njgrid.LL <- sdmTMB::add_utm_columns(njgrid.LL, c("Longitude", "Latitude"))

#----- Add bathymetry
bathy <- terra::rast("/Volumes/Eurybia/Bathymetry-GEBCO_19_Oct_2023_8946e1573d02 2/gebco_2023_n45.0_s32.0_w-79.11_e-65.0.tif")

## Get depth from raster for each lat/lon point in fedgrid.LL
# Grab just lat/lon from fedgrid.LL
points <- njgrid.LL %>%
  select(Longitude, Latitude) %>%
  rename(x = Longitude,
         y = Latitude)

# Get depth from bathy for each loc in points
depthpoints <- terra::extract(bathy, points)

# Bind back to fedgrid.LL and clean up
njgrid.LL <- cbind(njgrid.LL, depthpoints)

# Check for NAs
sapply(njgrid.LL, function(x) sum(is.na(x)))
njgrid.LL %>% filter_all(any_vars(is.na(.)))
njgrid.LL <- na.omit(njgrid.LL)

# Rename and Drop unneccessary columns
njgrid.LL$depth <- njgrid.LL[,6]
njgrid.LL <- njgrid.LL[-c(5:6)]

# Change "depth" (ie bathymetry) values from negative to positive, to make them true depth values
njgrid.LL <- njgrid.LL %>%
  mutate(Depth = case_when(depth >0 ~ 0,
                           depth <0 ~ depth * -1))
# Delete old depth col
njgrid.LL <- njgrid.LL[-5]



#----- Save grid with depth for each X,Y as a data.frame class for sdmTMB --------------------------------
saveRDS(njgrid.LL, "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NJoceantrawl.rds")

njgrid <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid_NJoceantrawl.rds")
