# Figures-cog-ms.R
######################################
# Janelle L. Morano

# Figures for manuscript

# last updated 22 February 2024
###############################################
###############################################

library(tidyverse)
library(ggbreak)
library(viridis)
library(gridExtra)
library(sf)

cog.sp <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/cog-spring-data.csv", header = TRUE)
cog.xy <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/cog-spring-data-XY.csv")

cog.fa <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/cog-fall-data.csv", header = TRUE)

# Create basemap
library(rnaturalearth)
library(rnaturalearthdata)
# Bring in regional layers
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")

# Crop maps to keep less data
us.cr <- st_crop(us, c(xmin = -67, xmax = -82, ymin = 20, ymax = 50))
canada.cr <- st_crop(canada, c(xmin = -67, xmax = -82, ymin = 20, ymax = 50))

# transform to crs = 26918
# try 4326
us.utm <- st_transform(us.cr, crs = 26918)
canada.utm <- st_transform(canada.cr, crs = 26918)

#check if the transformation was successful
st_crs(us.utm)

#convert cog to spatial
cog.xy.sf <- st_as_sf(cog.xy, coords = c("est_y", "est_x"), crs = 26918, remove = FALSE)
cog.xy.sf <- st_transform(cog.xy.sf, crs = 26918)


# # X,Y graph
# ggplot(cog.xy, aes(est_x, est_y, colour = year)) +
#   geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
#   geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
#   scale_colour_viridis_c() +
#   theme_classic()

ggplot() +
  # geom_sf(data = us.cr, color = "grey90", fill = "white") +
  # geom_sf(data = canada.cr, color = "grey90", fill = "white") +
  # geom_sf(data = us.utm, color = "grey50", fill = "white") +
  # geom_sf(data = canada.utm, color = "grey50", fill = "white") +
  # geom_sf(data = cog.xy.sf) +
  geom_point(data = cog.xy, aes(est_y, est_x)) +
  scale_colour_viridis_c() +
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Spring COG") 
##############################################################

# With Lat/Lon:
fa <- ggplot(cog.sp, aes(est_y, est_x, colour = year)) +
  geom_pointrange(aes(xmin = lwr_y, xmax = upr_y)) +
  geom_pointrange(aes(ymin = lwr_x, ymax = upr_x)) +
  scale_colour_viridis_c() +
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Spring COG") 

fb <- ggplot(cog.fa, aes(est_y, est_x, colour = year)) +
  geom_pointrange(aes(xmin = lwr_y, xmax = upr_y)) +
  geom_pointrange(aes(ymin = lwr_x, ymax = upr_x)) +
  scale_colour_viridis_c() +
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Fall COG") 


# But to reference the coastline
fc <- ggplot() +
  geom_sf(data = us.cr, color = "grey90", fill = "white") + 
  geom_sf(data = canada.cr, color = "grey90", fill = "white") +
  geom_pointrange(data = cog.sp, 
                  mapping = aes(x = est_y, y = est_x, 
                                xmin = lwr_y, xmax = upr_y,
                                ymin = lwr_x, ymax = upr_x,
                                col = year)) +
  scale_colour_viridis_c() +
  theme_classic() +
  theme(legend.position="none") +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Spring COG") 

# But to reference the coastline
fd <- ggplot() +
  geom_sf(data = us.cr, color = "grey90", fill = "white") + 
  geom_sf(data = canada.cr, color = "grey90", fill = "white") +
  geom_pointrange(data = cog.fa, 
                  mapping = aes(x = est_y, y = est_x, 
                                xmin = lwr_y, xmax = upr_y,
                                ymin = lwr_x, ymax = upr_x,
                                col = year)) +
  scale_colour_viridis_c() +
  theme_classic() +
  labs(x = "longitude", y = "latitude") +
  ggtitle("Fall COG") 

# grid.arrange(fcog.sp, fcog.fa, ncol = 2)
grid_arrange_shared_legend(fc, fd, ncol = 2, position='right')

library(lemon)
grid_arrange_shared_legend(fa, fb, fc, fd, ncol = 2, nrow = 2, position='right')

    

