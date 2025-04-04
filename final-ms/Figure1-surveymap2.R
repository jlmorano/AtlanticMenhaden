# Figure1-surveymap.R
######################################
# Janelle L. Morano

# "Figure 1. Map of surveys" for menhaden distribution manuscript
# AND Figure 3/4 A-B map inset for state region reference

# last updated 5 August 2024
###############################################
###############################################

library(sf)
library(tidyverse)
library(viridisLite)
library(viridis)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)



#----- Load Survey shapefiles and survey locations ------------------------------------------

### NEFSC
nefsc <- st_read("/Volumes/Eurybia/NEFSC strata/BTS_Strata.shp")
# STR2 are the strata numbers
st_crs(nefsc)
# CRS: NAD83 (EPSG,4269)

### NEAMAP
neamap <- st_read("/Volumes/Eurybia/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
st_crs(neamap)
# CRS: NAD83 (EPSG,4269)

### CTLISTS
ct <- st_read("/Volumes/Eurybia/CT Long Island Sound survey/LISTS_sitegrid_040822_JanelleMorano/sitegrid_unproj.shp")
st_crs(ct)
# CRS: NAD83 (EPSG,4269)

### GA EMTS
# These are the intended survey points, which may differ from the recorded lat/lon in the data. This is due to some unexplained errors, so for mapping, only using the planned locations.
ga <- read.csv("/Volumes/Eurybia/GA EMTS/GAEMTS Stations 2019.csv", header = TRUE)
ga.sf <- st_as_sf(ga, coords = c("LonBeg", "LatBeg"), crs = 4269)
st_crs(ga.sf)

### All other surveys with only Lat/Lon coordinates and no .shp
# Will need to ignore the GA data in here
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240726.csv", header = TRUE)
state.sf <- st_as_sf(state, coords = c("Longitude", "Latitude"), crs = 4269)
st_crs(state.sf)
# sapply(state, function(x) sum(is.na(x)))



#----- Generate Figure 1 -----------------------------------------------------------------
pal3 <- c("#b5de2b", "#482878", "#1f9e89")

legendcols <- c(pal3[3], pal3[2])

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

#----- Fig.1. Map of surveys
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  # strata lines and differentiation between NEAMAP and NEFSC
  # geom_sf(data = nefsc, color = "white", fill = pal3[3]) +
  # geom_sf(data = neamap, color = pal3[1], fill = pal3lite[1]) +
  geom_sf(data = nefsc, color = pal3[3], fill = pal3[3]) +
  geom_sf(data = neamap, color = pal3[3], fill = pal3[3]) +
  geom_sf(data = ct, color = pal3[2]) +
  geom_sf(data = ga.sf, color = pal3[2]) +
  # geom_sf(data = subset(state.sf, Survey %in% c("DEBay30ft", "MDGill", "NCp915", "NJOT", "ChesMMAP", "SEAMAP")), color = pal3[2]) +
  geom_point(data = subset(state, Survey %in% "DEBay30ft"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "MDGill"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NCp915"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "SEAMAP"), aes(Longitude, Latitude), color = pal3[2], size = 0.25) +
  coord_sf (xlim = c(-83,-62), ylim = c (28,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  # theme_void() +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  # theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") 
# Legend will have to be added later
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig1 survey map/Fig1-federal-state-survey-map.png", width=5.5, height = 6)


#----- Supplement Fig. Map of surveys, color-coded by survey
spal <- brewer.pal(n = 12, name = "Set3")
#skip 9 b/c looks grey 

ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = nefsc, color = "white", fill = spal[1]) +
  geom_sf(data = neamap, color = spal[2]) +
  geom_sf(data = ct, color = spal[3]) +
  geom_sf(data = ga.sf, color = spal[4]) +
  geom_point(data = subset(state, Survey %in% "DEBay30ft"), aes(Longitude, Latitude), color = spal[5], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "MDGill"), aes(Longitude, Latitude), color = spal[6], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NCp915"), aes(Longitude, Latitude), color = spal[7], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = spal[8], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = spal[10], size = 0.25) +
  geom_point(data = subset(state, Survey %in% "SEAMAP"), aes(Longitude, Latitude), color = spal[11], size = 0.25) +
  coord_sf (xlim = c(-83,-62), ylim = c (28,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  # theme_void() +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  # theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") 
# Legend will have to be added later
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig1 survey map/SupFig-federal-state-survey-identify-map.png", width=5.5, height = 6)



#----- Generate Figure 3 A/B Color by State --------------------------------------------

#----- Add state designation to NEFSC shapefile (which will match the NEFSC/NEAMAP data)
# Add inshore/offshore designation
nefsc <- nefsc %>%
  mutate(inoffshore = case_when(STRATA <2000 ~ "offshore",
                                STRATA >3000 & STRATA <5000 ~ "inshore",                              STRATA >3000 & STRATA <5000 ~ "inshore",
                                STRATA >5000 ~ "offshore",
                                STRATA >=7000 & STRATA <8000 ~ "inshore",
                                STRATA >=8000 ~ "offshore")) %>%
  # Add state designation. Strata are assigned to the closest state waters. RI-CT-NY are collectively assigned because of the overlap of the strata with the span of the states and the location of the mouth of Long Island Sound.
  mutate(State = case_when(STRATA >=3570 & STRATA <=3990 ~ "GME", # Inshore GME (north of 42 lat) 
                           STRATA >=1260 & STRATA <=1420 ~ "GME", # Offshore GME
                           STRATA ==1490 ~ "GME", # Offshore GME
                           STRATA >=5430 & STRATA <=5480 ~ "GME", # Offshore GME
                           STRATA ==3460 ~ "MA", #Inshore MA
                           STRATA >=3480 & STRATA <=3560 ~ "MA", #Inshore MA
                           STRATA >=1090 & STRATA <=1250  ~ "MA", #Offshore MA (south of 42 lat)
                           STRATA >=3010 & STRATA <=3140 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3450 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3470 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3490 ~ "RICTNY", #Inshore RICTNY
                           STRATA ==3910 ~ "RICTNY", #Inshore RICTNY
                           STRATA >=1010 & STRATA <=1140 ~ "RICTNY", #Offshore RICTNY
                           STRATA >=3150 & STRATA <=3230 ~ "NJ", #Inshore NJ                
                           STRATA >=1730 & STRATA <=1760 ~ "NJ", #Offshore NJ
                           STRATA >=3240 & STRATA <=3290 ~ "DEMD", #Inshore DEMD
                           STRATA >=1690 & STRATA <=1720 ~ "DEMD", #Offshore DEMD
                           STRATA >=3300 & STRATA <=3380 ~ "VA", #Inshore VA
                           STRATA >=1650 & STRATA <=1680 ~ "VA", #Offshore VA
                           STRATA >=3390 & STRATA <=3580 ~ "NC", #Inshore NC and south
                           STRATA >=1610 & STRATA <=1640 ~ "NC", #Offshore NC and south
                           .default = "NC"))



pal7 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26", "#fde725")
pal7lite <- adjustcolor(pal7, alpha.f = 0.2) #make transparent


#----- Highlight State: Map Federal Data lighter than State data
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  #geom_sf(data = subset(nefsc, State %in% "GME"), color = "white", fill = pal7lite[1]) +
  geom_sf(data = subset(nefsc, State %in% "MA"), color = "white", fill = pal7lite[1]) +
  geom_sf(data = subset(nefsc, State %in% "RICTNY"), color = "white", fill = pal7lite[2]) +
  geom_sf(data = subset(nefsc, State %in% "NJ"), color = "white", fill = pal7lite[3]) +
  geom_sf(data = subset(nefsc, State %in% "DEMD"), color = "white", fill = pal7lite[4]) +
  geom_sf(data = subset(nefsc, State %in% "VA"), color = "white", fill = pal7lite[5]) +
  geom_sf(data = subset(nefsc, State %in% "NC"), color = "white", fill = pal7lite[6]) +
  geom_point(data = subset(state, State %in% "RICTNY"), aes(Longitude, Latitude), color = pal7[2], size = 0.25) +
  geom_point(data = subset(state, State %in% "NJ"), aes(Longitude, Latitude), color = pal7[3], size = 0.25) +
  geom_point(data = subset(state, State %in% "DEMD"), aes(Longitude, Latitude), color = pal7[4], size = 0.25) +
  geom_point(data = subset(state, State %in% "VA"), aes(Longitude, Latitude), color = pal7[5], size = 0.25) +
  geom_point(data = subset(state, State %in% "NC"), aes(Longitude, Latitude), color = pal7[6], size = 0.25) +
  geom_point(data = subset(state, State %in% "SCGAFL"), aes(Longitude, Latitude), color = pal7[7], size = 0.25) +
  # coord_sf (xlim = c(-83,-62), ylim = c (28,46), expand = FALSE ) + #Full coast
  coord_sf (xlim = c(-82.5,-64), ylim = c (28,43), expand = FALSE ) + #tighter crop
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/state-regions-map-STATE-Hilite.png", width=5.5, height = 6)


#----- Highlight Federal: Make Federal data dark, omit state
# State data as transparent points doesn't work, so just omit
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = subset(nefsc, State %in% "MA"), color = "white", fill = pal7[1]) +
  geom_sf(data = subset(nefsc, State %in% "RICTNY"), color = "white", fill = pal7[2]) +
  geom_sf(data = subset(nefsc, State %in% "NJ"), color = "white", fill = pal7[3]) +
  geom_sf(data = subset(nefsc, State %in% "DEMD"), color = "white", fill = pal7[4]) +
  geom_sf(data = subset(nefsc, State %in% "VA"), color = "white", fill = pal7[5]) +
  geom_sf(data = subset(nefsc, State %in% "NC"), color = "white", fill = pal7[6]) +
  coord_sf (xlim = c(-82.5,-64), ylim = c (28,43), expand = FALSE ) + #tighter crop
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/state-regions-map-FEDERAL-Hilite.png", width=5.5, height = 6)

