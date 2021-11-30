##############################
# Basic menhaden model using NEAMAP data
#############################

##########
# Data Prep
##########
data <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
# check for NAs
sapply(data, function(x) sum(is.na(x)))
# yes in the habitat covariates, so ignore for now
data$LAT <- data$latitude
data$LON <- data$longitude

##########
# Make extrapolation grid
##########
library(sp)
packageVersion('sp') 
# 1.4.5
library(sf)
packageVersion('sf') 
# 0.9.6

### Use this to draw points around your data
plot(data$longitude, data$latitude)
LL <- locator()
saveRDS(LL, 'extent_LL.rds')

## Take a data.frame of coordinates in longitude/latitude that
## define the outer limits of the region (the extent).
LL <- readRDS('extent_LL.rds')
region_extent <- data.frame(long=LL$x, lat=LL$y)
str(region_extent)

#### Turn it into a spatial polygon object
## Need to duplicate a point so that it is connected
region_extent <- rbind(region_extent, region_extent[1,])
## https://www.maths.lancs.ac.uk/~rowlings/Teaching/Sheffield2013/cheatsheet.html
poly <- Polygon(region_extent)
polys <- Polygons(list(poly), ID='all')
sps <- SpatialPolygons(list(polys))
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), row.names='all'))
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")
sps <- spTransform(sps, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +datum=WGS84 +no_defs')
sps@proj4string <- crs_LL

## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 2000
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, Id,
                             Area_km2=( (cell_size/1000^2)),
                             row=1:nrow(region_grid_LL)))

## Filter out the grid that does not overlap (outside extent)
# region <- subset(region_df, !is.na(STRATA))
# I think this is only possible when there is a strata shapefile to compare
# so skipping here and just renaming to region
region <- region_df

## This is the final file needed.
str(region)
### Save it to be read in and passed to VAST later.
saveRDS(region, file = "user_region_NEAMAP.rds")

##########
# VAST model
##########

library(VAST)
sessionInfo()
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Mojave 10.14.6
# FishStatsUtils_2.9.1
# VAST_3.7.1           
# TMB_1.7.18 
# Matrix_1.2-8

# Verify data are loaded
data <- read.csv("/Users/janellemorano/DATA/NEAMAP/NEAMAP_Atlantic Menhaden_2007_2021.csv", header = TRUE)
colnames(data)
# "cruise"    "station"   "year"      "season"    "towbegin"  "timezone"  "region"    "dstrat"   
# "latitude"  "longitude" "areasw"    "depth"     "WT"        "SA"        "DO"        "PS"       
# "vimscode"  "count"     "weight"   
# check for NAs
sapply(data, function(x) sum(is.na(x)))
# yes in the habitat covariates, so ignore for now

##-- Model Settings
settings = make_settings( n_x = 250, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid',
                          ObsModel= c(2,0),#, 1st value encounter probabilities = Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc)),
                          bias.correct = FALSE )
#need to look more into next step
settings$FieldConfig[2,] <- 0 ## turn off temporal components with 0

##-- User Defined extrapolation grid
user_region <- readRDS('user_region_NEAMAP.rds')

# Fit the model
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects. This doesn't have any covariates (depth or bottom temp) yet.
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$latitude,
                 "Lon_i" = data$longitude,
                 "t_i" = data$year, #time
                 "b_i" = data$weight, #catch
                 "a_i" = data$areasw, #area swept, This is FAKED and NOT REAL
                 "v_i"= data$cruise ,
                 "input_grid" = user_region)

# Plot the results
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
setwd("/Users/janellemorano/Git/VAST output_DONOTGIT/_currentrun")
plot( fit )

