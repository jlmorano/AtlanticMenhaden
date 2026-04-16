# Occurrence-over-time-gif.R
######################################
# Janelle L. Morano
# Summary of NEFSC and NEAMAP data
# These data are used in the VAST menhaden model
# These maps and gif of maps were used in A-exam in Nov 2022 and GSA presentation Jan 2023

# last updated 20 April 2023
###############################################
###############################################


library(tidyverse)
library(janitor)
library(lubridate)
library(viridis)


surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl.csv", header = TRUE)
# Remove salinity cols because not working with now
surveydata <- select(surveydata, Survey, Stratum, Year, Season, Lat, Lon, Depth, Bottemp, Abundance, Biomass)
# remove NAs
sapply(surveydata, function(x) sum(is.na(x)))
surveydata <- na.omit(surveydata)
surv.spring <- filter(surveydata, Season == "SPRING") 
surv.fall <- filter(surveydata, Season == "FALL") 


#### Formatting Standards
############################
# Color selection
# Spring: color="#7AD151FF"
# Fall: color="#414487FF"
# theme_classic()


########################################################
#### 4. Map of Occurrence/Abundance at Sample Sites within Statistical survey areas
########################################################
# The following libraries and shapefiles read in at beginning
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(viridis)
# 
# nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# plot(nefsc)
# neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
# plot(neamap)


# # Create basemap
# world <- ne_countries(scale = "medium", returnclass = "sf")  
# us <- ne_states(geounit = "United States of America", returnclass = "sf")  
# canada <- ne_states(geounit = "Canada", returnclass = "sf")  
# state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

library(gganimate)
library(gifski)

# Plot Occurrence at survey locations
# Spring
# surv.spring.sub <- surv.spring %>% filter(Year >= 2007 & Year <= 2019 )
surv.spring.sub <- surv.spring %>% filter(Year >= 1972 )

p <- ggplot(data = world) +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = us, color = "gray", fill = "white") + #CCCC99
  geom_sf(data = canada, color = "gray", fill = "white") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) +
  geom_point(data = subset(surv.spring.sub, Abundance ==0),  #zero sites
             aes (x = Lon, y = Lat),
             size=.5, stroke=0.25,shape=1) +
  geom_point(data = subset(surv.spring.sub, Abundance >0) %>% #non-zero sites
               arrange(Abundance),
             aes (x = Lon, y = Lat), #instead of color =log(Abundance)
             size=4, stroke=0.75,shape=16, color = "red") +
  # scale_color_viridis_c(option = "viridis") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(axis.text = element_blank(),
        plot.title = element_text(size = 40, color = 'darkblue')) +
  # theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
  #       strip.background = element_blank()) + #lines for facet wrapping
  # facet_wrap('Year', ncol = 4) + #lines for facet wrapping
  labs(x= "longitude", 
       y = "latitude") #+
p

anim <- p + 
  transition_states(Year,
                    transition_length = 0.01,
                    state_length = 10,
                    wrap = TRUE) +
  ggtitle('Spring {closest_state}') 

animate(anim, nframes = 300, start_pause = 10, end_pause = 10, width = 680, height = 547)
anim_save('menhaden_presence_animation_1972-2021_Spring.gif', animation = last_animation(), path = '/Users/janellemorano/Documents/Presentations/images-videos')


# Fall
surv.fall.sub <- surv.fall %>% filter(Year > 1972 )

p <- ggplot(data = world) +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = us, color = "gray", fill = "white") + #CCCC99
  geom_sf(data = canada, color = "gray", fill = "white") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) +
  geom_point(data = subset(surv.fall.sub, Abundance ==0),  #zero sites
             aes (x = Lon, y = Lat),
             size=.5, stroke=0.25,shape=1) +
  geom_point(data = subset(surv.fall.sub, Abundance >0) %>% #non-zero sites
               arrange(Abundance),
             aes (x = Lon, y = Lat), #instead of color =log(Abundance)
             size=4, stroke=0.75,shape=16, color = "red") +
  # scale_color_viridis_c(option = "viridis") +
  theme_classic() +
  theme(text = element_text(size = 20)) +
  theme(axis.text = element_blank(),
        plot.title = element_text(size = 40, color = 'darkblue')) +
  # theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
  #       strip.background = element_blank()) + #lines for facet wrapping
  # facet_wrap('Year', ncol = 4) + #lines for facet wrapping
  labs(x= "longitude", 
       y = "latitude") #+
p

anim <- p + 
  transition_states(Year,
                    transition_length = 0.01,
                    state_length = 10,
                    wrap = TRUE) +
  ggtitle('Fall {closest_state}') 

animate(anim, nframes = 300, start_pause = 10, end_pause = 10, width = 680, height = 547)
anim_save('menhaden_presence_animation_1972-2021_Fall.gif', animation = last_animation(), path = '/Users/janellemorano/Documents/Presentations/images-videos')
