# Hotspot Analysis
library(tidyverse)

est_density.spring <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Spring gamma/PredDensity_spring.csv")
est_density.fall <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Fall gamma/PredDensity_fall.csv")

# Chesapeake Bay region: lat 35.8544 to 38.4208
ches.spring <- est_density.spring %>%
  filter(Lat >= 35.8544 & Lat <= 38.4208) 
ches.fall <- est_density.fall %>%
  filter(Lat >= 35.8544 & Lat <= 38.4208) 
  
# NY Bight region: 39.4059 to 40.9723
nyb.spring <- est_density.spring %>%
  filter(Lat >= 39.4059 & Lat <= 40.9723)
nyb.fall <- est_density.fall %>%
  filter(Lat >= 39.4059 & Lat <= 40.9723)

#### Schoener's D
####################
# 1 - 0.5 * sum(|density1 - density 2|)
ches.annualdensity.spring <- ches.spring %>%
  group_by(Year) %>%
  summarise(sum(logD))
ches.vec.spring <- pull(ches.annualdensity.spring[,2])

ches.annualdensity.fall <- ches.fall %>%
  group_by(Year) %>%
  summarise(sum(logD))
ches.vec.fall <- pull(ches.annualdensity.fall[,2])

x <- c()
for (i in 1:(length(ches.vec.spring)-1)) {
  x[i]<- abs( ches.vec.spring[i] - ches.vec.spring[i +1] )
}
ches.diff.spring <- x


for (i in 1:(length(ches.vec.fall)-1)) {
  x[i]<- abs( ches.vec.fall[i] - ches.vec.fall[i +1] )
}
ches.diff.fall <- x

plot(1:length(ches.diff.spring), ches.diff.spring, col = "blue", type = "l" )
lines(1:length(ches.diff.fall), ches.diff.fall, col = "red")

### Hotspot by density
##################


pick <- function(condition){
  function(d) d %>% filter_(condition)
}

hist(est_density.spring$logD)
hist(est_density.spring$logD[est_density.spring$logD>15])

# Create basemap
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

ggplot() +
  # geom_sf(data = us, color = "gray", fill = "white") + #CCCC99
  # geom_sf(data = canada, color = "gray", fill = "white") +
  # coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) +
  geom_point(est_density.spring, aes(x = Lon, y = Lat, logD)) +
  geom_point(size = 4, shape = 4) +
  geom_point(data = pick(~logD > 5), colour = "red") +
  facet_wrap('Year', ncol = 3) +
  ggtitle("Spring") +
  theme_classic()

hist(est_density.fall$logD)
ggplot(est_density.fall, aes(x = Lon, y = Lat, logD)) +
  geom_point(size = 4, shape = 4) +
  geom_point(data = pick(~logD > 5), colour = "red") +
  facet_wrap('Year', ncol = 3) +
  ggtitle("Fall") +
  theme_classic()

## Following from model-input-NEFSC...
# Spring
ggplot(data = world) +
  # geom_sf(data = us, color = "gray", fill = "white") + #CCCC99
  # geom_sf(data = canada, color = "gray", fill = "white") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) +
  geom_point(data = subset(est_density.spring, logD <10),  #zero sites
             aes (x = Lon, y = Lat),
             size=.5, stroke=0.25,shape=1, color="gray30") +
  geom_point(data = subset(est_density.spring, logD >=10) %>% #non-zero sites
               arrange(logD),
             aes (x = Lon, y = Lat), #instead of color =log(Abundance)
             size=.5, stroke=0.75,shape=16, color = "red") +
  # scale_color_viridis_c(option = "viridis") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(strip.text.x = element_text(size = 7),
        strip.background = element_blank()) +
  facet_wrap('Year', ncol = 4) +
  labs(x= "longitude", 
       y = "latitude",
       title = "Spring") 

# Fall
## Following from model-input-NEFSC...
ggplot(data = world) +
  # geom_sf(data = us, color = "gray", fill = "white") + #CCCC99
  # geom_sf(data = canada, color = "gray", fill = "white") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) +
  geom_point(data = subset(est_density.fall, logD <10),  #zero sites
             aes (x = Lon, y = Lat),
             size=.5, stroke=0.25,shape=1, color="gray30") +
  geom_point(data = subset(est_density.fall, logD >=10) %>% #non-zero sites
               arrange(logD),
             aes (x = Lon, y = Lat), #instead of color =log(Abundance)
             size=.5, stroke=0.75,shape=16, color = "red") +
  # scale_color_viridis_c(option = "viridis") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(strip.text.x = element_text(size = 7),
        strip.background = element_blank()) +
  facet_wrap('Year', ncol = 4) +
  labs(x= "longitude", 
       y = "latitude",
       title = "Fall") 
