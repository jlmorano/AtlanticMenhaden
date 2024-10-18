# Figure6-cog-ms.R
######################################
# Janelle L. Morano

# Figures for manuscript

# last updated 24 August 2024
###############################################
###############################################

library(tidyverse)
library(viridis)
library(gridExtra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggbreak)

cog.sp <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/cog-spring-data.csv", header = TRUE)

# UTM X and Y coordinates need to be multiplied by 1000 to line up with lat/lon
# Probably faster way with mutate all and a function but whatever...
cog.sp <- cog.sp %>% 
  mutate(est_x = est_x*1000,
         lwr_x = lwr_x*1000,
         upr_x = upr_x*1000,
         se_x = se_x*1000,
         est_y = est_y*1000,
         lwr_y = lwr_y*1000,
         upr_y = upr_y*1000,
         se_y = se_y*1000)


cog.fa <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/cog-fall-data.csv", header = TRUE)

# UTM X and Y coordinates need to be multiplied by 1000 to line up with lat/lon
# Probably faster way with mutate all and a function but whatever...
cog.fa <- cog.fa %>% 
  mutate(est_x = est_x*1000,
         lwr_x = lwr_x*1000,
         upr_x = upr_x*1000,
         se_x = se_x*1000,
         est_y = est_y*1000,
         lwr_y = lwr_y*1000,
         upr_y = upr_y*1000,
         se_y = se_y*1000)


# Create basemap
# Bring in regional layers
us <- ne_states(geounit = "United States of America", returnclass = "sf")
canada <- ne_states(geounit = "Canada", returnclass = "sf")

# Crop maps to keep less data
us.cr <- st_crop(us, c(xmin = -67, xmax = -83, ymin = 20, ymax = 48))
canada.cr <- st_crop(canada, c(xmin = -67, xmax = -83, ymin = 20, ymax = 48))

# transform to crs = 26919
us.utm <- st_transform(us.cr, crs = 26919)
canada.utm <- st_transform(canada.cr, crs = 26919)
# 
# #check if the transformation was successful
# st_crs(us.utm)

#generate colors for 52 years
mycols <- viridis(52)

# Spring
a <- ggplot() +
  geom_sf(data = us.utm, color = "grey70", fill = "white") + 
  geom_sf(data = canada.utm, color = "grey70", fill = "white") +
  geom_pointrange(data = cog.sp, aes(x=est_x, y = est_y, 
                                     ymin = lwr_y, ymax = upr_y), 
                  color = mycols) +
  geom_pointrange(data = cog.sp, aes(x=est_x, y = est_y, 
                                     xmin = lwr_x, xmax = upr_x), 
                  color = mycols) +
  scale_colour_viridis_d() +
  theme_classic() +
  theme(text = element_text(size = 14)) + 
  labs(x = "longitude", y = "latitude") +
  ggtitle("Spring")

# Fall
b <- ggplot() +
  geom_sf(data = us.utm, color = "grey70", fill = "white") + 
  geom_sf(data = canada.utm, color = "grey70", fill = "white") +
  geom_pointrange(data = cog.fa, aes(x=est_x, y = est_y, 
                                     ymin = lwr_y, ymax = upr_y), 
                  color = mycols) +
  geom_pointrange(data = cog.fa, aes(x=est_x, y = est_y, 
                                     xmin = lwr_x, xmax = upr_x), 
                  color = mycols) +
  # scale_color_manual("year", values = c(mycols)) + #this isn't forcing the legend
  theme_classic() +
  theme(text = element_text(size = 14)) + 
  labs(x = "longitude", y = "latitude") +
  ggtitle("Fall")


c <- ggplot(data = cog.sp, aes(x = year, y = est_y)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_classic() +
  theme(text = element_text(size = 16)) + 
  labs(x = " ", y = "UTM") +
  ggtitle("Spring")


d <- ggplot(data = cog.fa, aes(x = year, y = est_y)) +
  geom_point() +
  geom_smooth(method='lm') +
  theme_classic() +
  theme(text = element_text(size = 16)) + 
  labs(x = " ", y = "UTM") +
  ggtitle("Fall")


# Not all together because will do layout separately
plot_grid(a, b, c, d, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

# Plot separately and save
plot_grid(a, labels=c("A"))
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig6A-cog.png", width=5, height = 5)
plot_grid(b, labels=c("B"))
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig6B-cog.png", width=5, height = 5)
plot_grid(c, labels=c("C"))
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig6C-cog.png", width=4, height = 3)
plot_grid(d, labels=c("D"))
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig6D-cog.png", width=4, height = 3)