# model-input-NEFSC-NEAMAP_exploresummary.R
######################################
# Janelle L. Morano
# Summary of NEFSC and NEAMAP data
# These data are used in the VAST menhaden model
# These analyses are for intuition of what is happening with menhaden and temperature

# last updated 9 February 2023
###############################################
###############################################

# Table of Contents
# 1.  Map of survey data
# 2.  Abundance v. Year figure
#     - GAM
# 3.  Abundance v. Temp figure
#     - GAM
# 4.  Map Abundance at Sample Sites within Statistical survey areas (gganimate)
# 5.  Biomass vs Year figure
#     - GLM
# 6.  Average abundance by strata
# 7.  Average abundance by strata over time
#     - GAM of Ave Abundance by Strata over smooth(Years)
# 8.  Average biomass by strata
# 9.  Average biomass by strata over time

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




############################
#### 1. Map of survey data
############################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Add NEFSC and NEAMAP survey areas
nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
plot(nefsc)
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
plot(neamap)


ggplot(data = world) +  
  geom_sf(data = us, fill = "grey") + #CCCC99
  geom_sf(data = canada, fill = "grey") +
  geom_sf(data = nefsc, color = "black", fill = "#1f78b4") + 
  geom_sf(data = neamap, color = "black", fill = "#b2df8a") +
  coord_sf (xlim = c(-82,-62), ylim = c (23,47), expand = FALSE ) + #East coast, Fl to Nova Scotia
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + ##66CCFF, slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")




############################
#### 2. Abundance vs Year
############################
# Spring & Fall on same plot
ggplot(surveydata, aes(x=Year, y=log(Abundance +1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  labs(x= " ", y = "log(Abundance + 1)") 
  # geom_smooth(method = "loess") +
  # stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)
# pscl::hurdle(y ~ x + z, dist = "negbin") 

# #Spring
# ggplot(surv.spring, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(col = "green") +
#   theme_bw() +
#   ggtitle("Spring") 
# 
# #Fall
# ggplot(surv.fall, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(col = "blue") +
#   theme_bw() +
#   ggtitle("Fall")

# #### Linear model of log(Abundance) by Year
# abund.lm <- lm(log(Abundance +1) ~ Year, data = surveydata)
# summary(abund.lm)
# 
# #Spring
# abund.lm.spring <- lm(log(Abundance +1) ~ Year, data = surv.spring)
# summary(abund.lm.spring)
# 
# #Fall
# abund.lm.fall <- lm(log(Abundance +1) ~ Year, data = surv.fall)
# summary(abund.lm.fall)

# # Add coefficients from regression above onto figure
# coeff <- coefficients(abund.lm)
# # Equation of the line : 
# eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
# # Plot
# ggplot(surveydata, aes(x=Year, y=log(Abundance))) +
#   geom_point(shape=19, color="blue") +
#   geom_abline(intercept = -3.56, slope = 0.0018075, color = "red", size = 1.5) +
#   ggtitle(eq) +
#   theme_bw()

# # Alternatively, use geom_smooth
# # Spring
# ggplot(surv.spring, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(color="#22A884FF") +
#   geom_smooth(method=lm)
# 
# # Fall
# ggplot(surv.fall, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(color="#414487FF") +
#   geom_smooth(method=lm)

#### GAM of log(Abundance) by Year
library(mgcv)

#Spring
abun.gam.spring = gam(log(Abundance +1) ~ s(Year), data = surv.spring)
summary(abun.gam.spring)
plot(abun.gam.spring)

preddata <- data.frame(Year = surv.spring$Year) #,
                       # Bottemp = surv.spring$Bottemp,
                       # Depth = surv.spring$Depth,
                       # Lat = surv.spring$Lat)
pred.abun.gam.spring <- predict.gam(abun.gam.spring, newdata = preddata, type="response", se.fit=TRUE)
head(pred.abun.gam.spring)

plot(log(surv.spring$Abundance +1) ~ surv.spring$Year)
lines(preddata$Year, pred.abun.gam.spring$fit, col="red")
lines(preddata$Year, pred.abun.gam.spring$fit+2*pred.abun.gam.spring$se.fit,lty=2, col="red")
lines(preddata$Year, pred.abun.gam.spring$fit-2*pred.abun.gam.spring$se.fit,lty=2, col="red")

#Fall
abun.gam.fall = gam(log(Abundance +1) ~ s(Year), data = surv.fall)
summary(abun.gam.fall)
plot(abun.gam.fall,
     main = "Fall")


#### Plot GAM on data
library(mgcViz)
# convert gam fit to mgcViz object
abun.gam.spring.viz <- getViz(abun.gam.spring)
# Plot the first smoother
plot( sm(abun.gam.spring.viz, 1) ) +
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic() +
  ggtitle("Spring Abundance by Year")

#### Model comparison
AIC(abund.lm)
AIC(abun.gam)


############################
#### 3. Abundance vs Temp
############################
ggplot(surveydata, aes(x=Bottemp, y=log(Abundance +1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  labs(x= "Bottom Temperature (°C)", y = "log(Abundance +1)") 
  # geom_smooth(method = "loess") +
  # stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)

#### Linear model of log(Abundance) by Year
# Spring
temp.lm.spring <- lm(log(Abundance +1) ~ Bottemp, data = surv.spring)
summary(temp.lm.spring)

ggplot(surv.spring, aes(x=Bottemp, y=log(Abundance +1))) +
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm)

# Fall
temp.lm.fall <- lm(log(Abundance +1) ~ Bottemp, data = surv.fall)
summary(temp.lm.fall)

ggplot(surv.fall, aes(x=Bottemp, y=log(Abundance +1))) +
  geom_point(shape=19, color="blue") +
  geom_smooth(method=lm)

#### GAM of log(Abundance) by Year
#Spring
temp.gam.spring <- gam(log(Abundance +1) ~ s(Bottemp), data = surv.spring)
summary(temp.gam.spring)
plot(temp.gam.spring, main = "Spring")

#Fall
temp.gam.fall <- gam(log(Abundance +1) ~ s(Bottemp, bs = "cr"), data = surv.fall)
summary(temp.gam.fall)
plot(temp.gam.fall, main = "Fall")

#### Model comparison
AIC(abund.lm)
AIC(abun.gam)


#### Plot GAM on data
library(mgcViz)
# convert gam fit to mgcViz object
temp.gam.spring.viz <- getViz(temp.gam.spring)
# Plot the first smoother
plot( sm(temp.gam.spring.viz, 1) ) +
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic() +
  ggtitle("Spring Abundance by Temperature")


# Fall
temp.gam.fall.viz <- getViz(temp.gam.fall)
# Plot the first smoother
plot( sm(temp.gam.fall.viz, 1) ) +
  l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic() +
  ggtitle("Fall Abundance by Temperature")




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




# NEFSC: glm of log(Abundance) ~ Bottemp + Stratum + Season
glm1 <- glm(log(Abundance +1) ~ Bottemp + Stratum + Season, data = surveydata)
summary(glm1)

# Map which of these strata are significant
strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
plot(st_geometry(strata))

# # color statistically sig strata
# ggplot() +
#   geom_sf(data = strata, aes(color = factor(STRATA)))
#   # geom_sf_label(aes(label = STRATA))

# Filter out only the ***sig strata
strata.ex <- strata %>%
  filter(STRATA %in% c(11, 14,15,2,3,3180,3270,3360,3420,3430,4,6,7500,7560,7590,7620,7860))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.ex, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none")


# Filter out >.05 sig strata
strata.sig <- strata %>%
  filter(STRATA %in% c(10,12,1240,1270,1280,13,1370,1380,3150,3240,3300,3400,3440,7,7510,7530,7570,7600,7630,7770,7830, 11,14,15,2,3,3180,3270,3360,3420,3430,4,6,7500,7560,7590,7620,7860))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none")




########################################################
#### 5. Biomass at Sample Sites within Statistical survey areas
########################################################

# Biomass v. Year
library(ggbreak) 
library(patchwork)
ggplot(surveydata, aes(x=Year, y=log(Biomass), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  # scale_y_break(c(1000, 3500)) + #don't use when on log scale
  geom_point() +
  theme_classic() +
  labs(x= " ", y = "log(Biomass) (kg/tow)") 

# NEFSC: glm of Biomass ~ Bottemp + Stratum + Season
glm2 <- glm(Biomass ~ Bottemp + Stratum + Season, data = surveydata)
summary(glm2)

# Filter out >.05 sig strata
strata.sig <- strata %>%
  filter(STRATA %in% c(14,3350))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none")



########################################################
#### 6. Average abundance by strata
########################################################

### SPRING
# Average Abundance per Stratum
abun.strat <- surv.spring %>%
  group_by(Survey, Stratum) %>%
  summarise(AveAbun = mean(Abundance)) 

## Map of Average Abundance
# Add NEFSC and NEAMAP survey areas, repeat from beginning above
# nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# plot(nefsc)
# neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
# plot(neamap)

# Add AveAbun to NEFSC shp
colnames(abun.strat)[2] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.ave.spring <- dplyr::left_join(nefsc, abun.strat, by = "STRATA")

# Add AveAbun to neamap shp
colnames(abun.strat)[2] = "REGION"
abun.strat <- abun.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.ave.spring <- dplyr::left_join(neamap, abun.strat, by = "REGION")

ggplot() +
  geom_sf(data = subset(nefsc.ave.spring , AveAbun >0), aes(fill = AveAbun)) +
  geom_sf(data = subset(neamap.ave.spring, AveAbun >0), aes(fill = AveAbun)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Spring")

### FALL
# Average Abundance per Stratum
abun.strat <- surv.fall %>%
  group_by(Survey, Stratum) %>%
  summarise(AveAbun = mean(Abundance)) 

# Add AveAbun to NEFSC shp
colnames(abun.strat)[2] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.ave.fall <- dplyr::left_join(nefsc, abun.strat, by = "STRATA")

# Add AveAbun to neamap shp
colnames(abun.strat)[2] = "REGION"
abun.strat <- abun.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.ave.fall <- dplyr::left_join(neamap, abun.strat, by = "REGION")

ggplot() +
  geom_sf(data = subset(nefsc.ave.fall , AveAbun >0), aes(fill = AveAbun)) +
  geom_sf(data = subset(neamap.ave.fall, AveAbun >0), aes(fill = AveAbun)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Fall")


########################################################
#### 7. Average abundance by strata over time
########################################################

# Average Abundance per Stratum, per Year
# SPRING
ann.abun.strat <- surv.spring %>%
  group_by(Stratum, Year) %>%
  summarise(AveAbun = mean(Abundance)) 

ggplot(ann.abun.strat, aes(x=Year, y=AveAbun, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = " ", y = "Abundance") +
  theme_classic() +
  theme(legend.position = "none")

## Map by Year
# Add ann.abun.strat$AveAbun to NEFSC shp
colnames(ann.abun.strat)[1] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.yrave.spring <- dplyr::right_join(nefsc, ann.abun.strat, by = "STRATA")

# Add AveAbun to neamap shp
colnames(ann.abun.strat)[1] = "REGION"
ann.abun.strat <- ann.abun.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.yrave.spring <- dplyr::right_join(neamap, ann.abun.strat, by = "REGION")

# FALL
ann.abun.strat <- surv.fall %>%
  group_by(Stratum, Year) %>%
  summarise(AveAbun = mean(Abundance)) 

ggplot(ann.abun.strat, aes(x=Year, y=AveAbun, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = " ", y = "Abundance") +
  theme_classic() +
  theme(legend.position = "none")

## Map by Year
# Add ann.abun.strat$AveAbun to NEFSC shp
colnames(ann.abun.strat)[1] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.yrave.spring <- dplyr::right_join(nefsc, ann.abun.strat, by = "STRATA")

# Add AveAbun to neamap shp
colnames(ann.abun.strat)[1] = "REGION"
ann.abun.strat <- ann.abun.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.yrave.spring <- dplyr::right_join(neamap, ann.abun.strat, by = "REGION")

# Map by Year, Season
ggplot() +
  geom_sf(data = subset(neamap.yrave.spring , AveAbun >0), aes(fill = AveAbun)) +
  geom_sf(data = subset(neamap.yrave.spring, AveAbun >0), aes(fill = AveAbun)) +
  scale_fill_viridis_c() +
  theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
        strip.background = element_blank()) + #lines for facet wrapping
  facet_wrap('Year', ncol = 8) + #lines for facet wrapping
  theme_minimal() +
  ggtitle("Spring")


#####
## GAM of Ave Abundance by Strata over Years
#####
library(mgcv)

# Predict Abundance for each Strata over time, with smoother over Year
gam.1 = gam(AveAbun ~ s(Year), data = ann.abun.strat)
summary(gam.1)
plot(gam.1)

preddata <- data.frame(Year = ann.abun.strat$Year) #,
pred.gam.abunstrata.1 <- predict.gam(gam.1, newdata = preddata, type="response", se.fit=TRUE)
head(pred.gam.abunstrata.1)

plot(log(ann.abun.strat$AveAbun) ~ ann.abun.strat$Year)
lines(preddata$Year, pred.gam.abunstrata$fit, col="red")
lines(preddata$Year, pred.gam.abunstrata$fit+2*pred.gam.abunstrata$se.fit,lty=2, col="red")
lines(preddata$Year, pred.gam.abunstrata$fit-2*pred.gam.abunstrata$se.fit,lty=2, col="red")


gam.2 = gam(AveAbun ~ s(Year) + Stratum, data = ann.abun.strat)
summary(gam.2)
plot(gam.2)

preddata <- data.frame(Stratum = ann.abun.strat$Stratum, Year = ann.abun.strat$Year) #,
pred.gam.abunstrata.2 <- predict.gam(gam.2, newdata = preddata, type="response", se.fit=TRUE)
head(pred.gam.abunstrata.2)

plot(log(ann.abun.strat$AveAbun) ~ ann.abun.strat$Year, col = factor(ann.abun.strat$Stratum))
lines(preddata$Year, pred.gam.abunstrata.2$fit, col="red")
lines(preddata$Year, pred.gam.abunstrata.2$fit+2*pred.gam.abunstrata.2$se.fit,lty=2, col="red")
lines(preddata$Year, pred.gam.abunstrata.2$fit-2*pred.gam.abunstrata.2$se.fit,lty=2, col="red")


# Predict Abundance for each Strata over time, with smoother over Year
gam.2 = gam(AveAbun ~ s(Year, by = Year) + Stratum, data = ann.abun.strat)
summary(gam.2)
plot(gam.2)

gam.3 = gam(AveAbun ~ s(Year, by = Stratum) + Stratum, data = ann.abun.strat)
summary(gam.3)
plot(gam.3)


########################################################
#### 8. Average biomass by strata
########################################################

# Average Abundance per Stratum
biom.strat <- surv.spring %>%
  group_by(Survey, Stratum) %>%
  summarise(AveBiom = mean(Biomass)) 

## Map of Average Abundance
# Add NEFSC and NEAMAP survey areas, repeat from beginning above
# nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# plot(nefsc)
# neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
# plot(neamap)

# Add AveAbun to NEFSC shp
colnames(biom.strat)[2] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.aveb <- dplyr::left_join(nefsc, biom.strat, by = "STRATA")

# Add AveAbun to neamap shp
colnames(biom.strat)[2] = "REGION"
biom.strat <- biom.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.aveb <- dplyr::left_join(neamap, biom.strat, by = "REGION")


ggplot() +
  geom_sf(data = subset(nefsc.aveb , AveBiom >0), aes(fill = AveBiom)) +
  geom_sf(data = subset(neamap.aveb, AveBiom >0), aes(fill = AveBiom)) +
  scale_fill_viridis_c() +
  theme_minimal() 



########################################################
#### 9. Average biomass by strata over time
########################################################

# Average Biomass per Stratum
ann.biom.strat <- surv.spring %>%
  group_by(Stratum, Year) %>%
  summarise(AveBiom = mean(Biomass)) 

ggplot(ann.biom.strat, aes(x=Year, y=AveBiom, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = " ", y = "Biomass") +
  theme_classic() +
  theme(legend.position = "none")

## Map by Year
# Add ann.biom.strat$AveBiom to NEFSC shp
colnames(ann.biom.strat)[1] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.yraveb <- dplyr::right_join(nefsc, ann.biom.strat, by = "STRATA")

# Add AveBiom to neamap shp
colnames(ann.biom.strat)[1] = "REGION"
ann.biom.strat <- ann.biom.strat %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.yraveb <- dplyr::right_join(neamap, ann.biom.strat, by = "REGION")

# Map by Year
ggplot() +
  geom_sf(data = subset(nefsc.yraveb , AveBiom >0), aes(fill = AveBiom)) +
  geom_sf(data = subset(neamap.yraveb, AveBiom >0), aes(fill = AveBiom)) +
  scale_fill_viridis_c() +
  theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
        strip.background = element_blank()) + #lines for facet wrapping
  facet_wrap('Year', ncol = 8) + #lines for facet wrapping
  theme_minimal() 
