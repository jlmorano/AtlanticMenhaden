# Map-surveys-federal-state.R
######################################
# Janelle L. Morano

# Purpose: Map survey locations and regions

# Mapping these surveys (have strata shp files)
#   NEFSC
#   NEAMAP
#   Connecticut Long Island Sound Trawl Survey (CTLISTS)
# Mapping these surveys (have only lat/long for survey locs)
#   NYDEC Western Long Island
#   NJ Ocean Trawl
#   Delaware Bay
#   (not yet) Maryland Gill Net (lacks lat/long info)
#   ChesMMAP
#   (not yet) VIMS Shad Gill net
#   Georgia EMTS

# last updated 27 July 2023
###############################################
###############################################

library(sf)
library(tidyverse)
library(viridisLite)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)



#----- Load Survey shapefiles and survey locations ------------------------------------------

### NEFSC
# BTS_Strata.shp: CENTROIDS from these data are CORRECT
nefsc <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
plot(nefsc)
# STR2 are the strata numbers
# finstr_nad83.shp: CENTROIDS from these data are INCORRECT, so only use above
# nefsc.finstr <- st_read("/Users/janellemorano/DATA/NEFSC_bt_strata/finstr_nad83.shp")


### NEAMAP
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")


### CTLISTS
ct <- st_read("/Users/janellemorano/DATA/CT Long Island Sound survey/LISTS_sitegrid_040822_JanelleMorano/sitegrid_unproj.shp")


### All other surveys with only Lat/Lon coordinates and no .shp
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data_20230727.csv", header = TRUE)


#----- Add State Designation to NEFSC
 # Add state designation. Strata are assigned to the closest state waters. RI-CT-NY are collectively assigned because of the overlap of the strata with the span of the states and the location of the mouth of Long Island Sound.
#test <- st_coordinates(nefsc)
#nefsc$latitude <- test[,2]
nefsc <- nefsc %>%
  mutate(State = case_when(.$STRATA >=3570 & .$STRATA <=3990 ~ "GME", # Inshore GME (north of 42 lat) 
                           .$STRATA >=1260 & .$STRATA <=1420 ~ "GME", # Offshore GME
                           .$STRATA ==1490 ~ "GME", # Offshore GME
                           .$STRATA >=5430 & .$STRATA <=5480 ~ "GME", # Offshore GME
                           .$STRATA ==3460 ~ "MA", #Inshore MA
                           .$STRATA >=3480 & .$STRATA <=3560 ~ "MA", #Inshore MA
                           .$STRATA >=1090 & .$STRATA <=1250  ~ "MA", #Offshore MA (south of 42 lat)
                           .$STRATA >=3010 & .$STRATA <=3140 ~ "RICTNY", #Inshore RICTNY
                           .$STRATA ==3450 ~ "RICTNY", #Inshore RICTNY
                           .$STRATA ==3470 ~ "RICTNY", #Inshore RICTNY
                           .$STRATA ==3490 ~ "RICTNY", #Inshore RICTNY
                           .$STRATA ==3910 ~ "RICTNY", #Inshore RICTNY
                           .$STRATA >=1010 & .$STRATA <=1140 ~ "RICTNY", #Offshore RICTNY
                           .$STRATA >=3150 & .$STRATA <=3230 ~ "NJ", #Inshore NJ                           
                           .$STRATA >=1730 & .$STRATA <=1760 ~ "NJ", #Offshore NJ
                           .$STRATA >=3240 & .$STRATA <=3290 ~ "DEMD", #Inshore DEMD
                           .$STRATA >=1690 & .$STRATA <=1720 ~ "DEMD", #Offshore DEMD
                           .$STRATA >=3300 & .$STRATA <=3380 ~ "VA", #Inshore VA
                           .$STRATA >=1650 & .$STRATA <=1680 ~ "VA", #Offshore VA
                           .$STRATA >=3390 & .$STRATA <=3580 ~ "NC", #Inshore NC and south
                           .$STRATA >=1610 & .$STRATA <=1640 ~ "NC", #Offshore NC and south
                           .$STRATA >7000 ~ "NC")) #Offshore NC and south (includes confirmed strata and numbers including 7000+ and 8000+ that don't directly correspond to maps, but follow convention described above)
                           
                           #.$STRATA >7000 & .$latitude >36 ~ "GME",
                           #.$STRATA == 1990 & .$latitude >36 ~ "GME",
                           #.$STRATA == 1990 & .$latitude <36 ~ "NC")) #1990, 7940, 7980 have to be an error assignment for some because it spans different lats


#----- Create US east coast basemap for referencing in presentations -----------------------------------------------------
# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  coord_sf (xlim = c(-83,-62), ylim = c (29,46), expand = FALSE ) + #Full coast
  theme_void() + #  theme_classic() +  
  theme(panel.background = element_rect(fill = "lightcyan")) + #royalblue2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/USeastcoast-map.png", width=5.5, height = 6)




#----- Map It! -----------------------------------------------------------------
pal8 <- c("#b5de2b", "#6ece58", "#440154", "#482878", "#3e4989", "#31688e", "#26828e", "#1f9e89")
pal8lite <- adjustcolor(pal8, alpha.f =0.5) #make transparent

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Map surveys
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  geom_sf(data = nefsc, color = "white", fill = pal8[8]) +
  # geom_sf(data = neamap, color = pal8[1], fill = pal8lite[1]) +
  geom_point(data = subset(state, Survey %in% "CTLISTS"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "WLI"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "DEBay"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "GAEMTS"), aes(Longitude, Latitude), color = pal8[4], size = 0.5) +
  coord_sf (xlim = c(-83,-62), ylim = c (29,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/survey-area-map.png", width=5.5, height = 6)


#----- Color by State
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent
ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") +
  #geom_sf(data = subset(nefsc, State %in% "GME"), color = "white", fill = pal6lite[1]) +
  geom_sf(data = subset(nefsc, State %in% "MA"), color = "white", fill = pal6lite[1]) +
  geom_sf(data = subset(nefsc, State %in% "RICTNY"), color = "white", fill = pal6lite[2]) +
  geom_sf(data = subset(nefsc, State %in% "NJ"), color = "white", fill = pal6lite[3]) +
  geom_sf(data = subset(nefsc, State %in% "DEMD"), color = "white", fill = pal6lite[4]) +
  geom_sf(data = subset(nefsc, State %in% "VA"), color = "white", fill = pal6lite[5]) +
  geom_sf(data = subset(nefsc, State %in% "NC"), color = "white", fill = pal6lite[6]) +
  geom_point(data = subset(state, Survey %in% "CTLISTS"), aes(Longitude, Latitude), color = pal6[2], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "WLI"), aes(Longitude, Latitude), color = pal6[2], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = pal6[3], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "DEBay"), aes(Longitude, Latitude), color = pal6[4], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "ChesMMAP"), aes(Longitude, Latitude), color = pal6[5], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "GAEMTS"), aes(Longitude, Latitude), color = pal6[6], size = 0.5) +
  coord_sf (xlim = c(-83,-62), ylim = c (29,46), expand = FALSE ) + #Full coast
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/state-area-map-federal.png", width=5.5, height = 6)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/state-area-map-federal-state.png", width=5.5, height = 6)


#----- Survey areas of NY -----------------------------------------------------------
# Add survey sites from NEFSC & NEAMAP
federal <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230724.csv", header = TRUE)

ggplot(data = world) +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_point(data = federal, aes(Longitude, Latitude), color = pal6[2], size =0.5) +
  geom_point(data = subset(state, Survey %in% "CTLISTS"), aes(Longitude, Latitude), color = pal6[2], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "WLI"), aes(Longitude, Latitude), color = pal6[2], size = 0.5) +
  geom_point(data = subset(state, Survey %in% "NJOT"), aes(Longitude, Latitude), color = pal6[3], size = 0.5) +
  coord_sf (xlim = c(-74,-70), ylim = c (40.2,41), expand = FALSE ) + #Full coast
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")




#----- Centroids of strata -----------------------------------------------------------

#----- NEAMAP
# Centroids extracted in "create-nefsc-neamap-menhaden-model-data-wchlor-a.R"
nefsc.centroids <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/nefsc-strata-centroids.csv", header = TRUE)

#----- NEAMAP






