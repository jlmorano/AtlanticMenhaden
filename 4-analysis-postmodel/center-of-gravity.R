# Visualize model output

# Center of gravity
cog_spring <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Spring gamma/CenterofGravity_spring.csv", header = TRUE)
cog_spring <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Fall gamma/CenterofGravity_fall.csv, header = TRUE")

####################
# Center of Gravity
####################
fit.spring[["Report"]][["mean_Z_ctm"]]
# Spatial_axis = E_km is eastings
easting <- fit.spring[["Report"]][["mean_Z_ctm"]][,,1]
# Spatial_axis = N_km is northings
northing <- fit.spring[["Report"]][["mean_Z_ctm"]][,,2]
# pull together
cog <- data.frame(northing, easting, year = c(1972:2021))
write_csv(cog, file = "CenterofGravity_spring.csv")

cogsf <- st_as_sf(x = cog, 
                  coords = c("easting", "northing"),
                  crs = "+proj=utm +zone=18")
cog.spa <-  st_transform(cogsf, crs = "+proj=longlat +datum=WGS84")
cog.spa <- as.data.frame(cog.spa)




library(sf)
cogsp <- st_as_sf(cog, coords = c("longitude", "latitude"), crs = 4326)
cog2 <- SpatialPoints(cog)
yr <- data.frame(Year = c(1972:2021))
cog2<- SpatialPointsDataFrame(cog2, yr)
cog <- drop_units(cog)
ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
                       geom_line()+
                         theme_classic()
                       
                       fit.fall[["Report"]][["mean_Z_ctm"]]
                       # 1 is eastings
                       easting <- fit.fall[["Report"]][["mean_Z_ctm"]][,,1]
                       # 2 is northings
                       northing <- fit.fall[["Report"]][["mean_Z_ctm"]][,,2]
                       # Year, not sure where it is so I'm going to guess it's in order from low to high and create
                       year <- c(2007:2019)
                       
                       cog <- data.frame(year, northing, easting)
                       write_csv(cog, file = "CenterofGravity_fall.csv")
                       cog <- drop_units(cog)
                       ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
                         geom_line()+
                         theme_classic()
                       
                       
                       
                       
                       
                       #convert northing/easting to UTM
                       cord.dec = SpatialPoints(cbind(cog$long, -cog$lat), proj4string=CRS("+proj=longlat"))
                       cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))
                       cord.UTM
                       
                       #northing
                       ggplot(cog, aes(x=year, y=northing)) +
                         geom_line()
                       #easting
                       ggplot(cog, aes(x=year, y=easting)) +
                         geom_line()
                       
                       
                       
                       
