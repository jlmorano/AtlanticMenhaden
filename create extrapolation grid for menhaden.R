# Extrapolation grid for Atlantic menhaden distribution model
######################################
# Janelle L. Morano
# Joint grid for NEFSC and NEAMAP data
# last updated 10/1/2021

###############################################
###############################################
library(sp)
packageVersion('sp') 
# 1.4.5
library(sf)
packageVersion('sf') 
# 0.9.6
library(rgdal) # '1.5.18'
# I have 1.5-23
library(raster)

#first tried to merge the shp files outright, but the columns are different
#so running through each then merging the final regions created.

# Read in NEFSC bottom trawl strata
shp1 <- readOGR("/Users/janellemorano/DATA/NEFSC_bt_strata/finstr_nad83.shp", layer = "finstr_nad83")
plot(shp1)

# transform the projection
sps <- spTransform(shp1, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1

## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon1 <- spTransform(sps, crs_UTM)
### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 2000
## Create a grid over the geometry of the region_polygon object
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
head (shp1@data)
# AREA PERIMETER FINSTR_G_ FINSTR_G_I STRATA   A2
# 0 0.164     6.122         2       3840   3820 1712
# 1 0.036     2.681         3       3088   3880  375
# In this data, it's STRATA, so replace Id with STRATA
region_grid_sp@data <- over(region_grid, region_polygon)
crs_LL <- CRS('+proj=longlat +datum=WGS84 +no_defs') #good for PROJ6
sps@proj4string <- crs_LL

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, STRATA,
                             Area_km2=( (cell_size/1000^2)),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region1 <- subset(region_df, !is.na(STRATA))
## This is the final file needed.
str(region1)
# 'data.frame':	2992 obs. of  5 variables:
#   $ Lon     : num  -75.4 -75.4 -75.4 -75.4 -75.4 ...
# $ Lat     : num  35.2 35.2 35.2 35.2 35.2 ...
# $ REGION  : chr  "15" "15" "15" "15" ...
# $ Area_km2: num  0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 ...
# $ row     : int  27 244 245 463 464 680 681 682 898 899 ...


# NEAMAP
# read in shapefile
shp2 <- readOGR("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp", layer="NMDepthStrataPolgyons")
# plot it to look at it
plot(shp2)

# transform the projection
sps <- spTransform(shp2, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1

## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon2 <- spTransform(sps, crs_UTM)

#### ******trying to merge region_polygons to see if this works*****
# need these: AREA=Shape_Area, PERIMETER=Shape_Leng, STRATA = REGION
region_polygon1 %>%
  select(AREA, PERIMETER, STRATA) %>%
  rename(Shape_Area = AREA,
         Perimeter = PERIMETER,
         Region = STRATA)
  

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 2000
## Create a grid over the geometry of the region_polygon object
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
head (shp2@data)
# OBJECTID REGION DEPTHGROUP Index Shape_Leng  Shape_Area
# 0        1     01      40-60  0102  0.7499998 0.011874994
# 1        2     02      20-40  0201  0.6999998 0.004999997
# In this data, it's REGION, so replace ID with REGION
region_grid_sp@data <- over(region_grid, region_polygon)
crs_LL <- CRS('+proj=longlat +datum=WGS84 +no_defs') #good for PROJ6
sps@proj4string <- crs_LL

## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, REGION,
                             Area_km2=( (cell_size/1000^2)),
                             row=1:nrow(region_grid_LL)))

## HERE I AM NOT OVERWRITING ABOVE= REGION2
## Filter out the grid that does not overlap (outside extent)
region2 <- subset(region_df, !is.na(REGION))
## This is the final file needed.
str(region2)
# 'data.frame':	2992 obs. of  5 variables:
#   $ Lon     : num  -75.4 -75.4 -75.4 -75.4 -75.4 ...
# $ Lat     : num  35.2 35.2 35.2 35.2 35.2 ...
# $ REGION  : chr  "15" "15" "15" "15" ...
# $ Area_km2: num  0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 0.002 ...
# $ row     : int  27 244 245 463 464 680 681 682 898 899 ...


# Merge region1 and region2 files
# Rename REGION to STRATA in region2
names(region2)[3] <- 'STRATA'
OK <- rbind(region1, region2)
plot(OK$Lat, OK$Lon, cex =.1)

### Save it to be read in and passed to VAST later.
saveRDS(region, file = "/Users/janellemorano/Git/AtlanticMenhaden/user_region_NEAMAP.rds")
