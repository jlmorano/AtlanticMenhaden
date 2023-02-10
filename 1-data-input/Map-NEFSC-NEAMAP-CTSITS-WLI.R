# Map-NEFSC-NEAMAP-CTSITS-WLI.R
######################################
# Janelle L. Morano
# Mapping
#   NEFSC
#   NEAMAP
#   Connecticut Long Island Sound Trawl Survey (CTLISTS)
#   NYDEC Western Long Island

# last updated 31 October 2022
###############################################
###############################################

library(tidyverse)
library(viridisLite)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Add NEFSC and NEAMAP survey areas
strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
nmp.strata <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")

ct <- read.csv("/Users/janellemorano/DATA/CT Long Island Sound survey/CTLISTS_menhaden.csv", header = TRUE)
colnames(ct)
plot(ct$Latitude, ct$Longitude)

wli <- read.csv("/Users/janellemorano/DATA/NY Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Map 'em
ggplot(data = world) +  
  geom_sf(data = us, fill = "grey") + #CCCC99
  geom_sf(data = canada, fill = "grey") +
  geom_sf(data = strata, color = "black", fill = "#1f78b4") + 
  geom_sf(data = nmp.strata, color = "black", fill = "#b2df8a") +
  # geom_point(data = ct, aes(Longitude, Latitude), inherit.aes = FALSE, size = 0.3, color = "#253494") +
  # geom_point(data = wli, aes(Longitutde, Latitude), inherit.aes = FALSE, size = 0.3, color = "#41b6c4") +
  coord_sf (xlim = c(-82,-62), ylim = c (23,47), expand = FALSE ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  coord_sf(xlim = c(-74, -71), ylim = c(40, 42)) 



# strata <- strata %>% st_set_crs()
strata2 <- st_read("/Users/janellemorano/DATA/NEFSC_bt_strata/finstr_nad83.shp")
nefsc <- st_geometry(strata2)
plot(nefsc)
plot(st_centroid(nefsc), add = TRUE, pch = 3, col = 'red')
st_area(nefsc)
