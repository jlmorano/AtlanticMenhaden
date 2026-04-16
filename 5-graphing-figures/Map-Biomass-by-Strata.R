# Map-Biomass-by-Strata.R
######################################
# Janelle L. Morano

# Map of Biomass by Strata

# last updated 15 May 2023

###############################################
###############################################

# Table of Contents
# A. Setup NMFS & NEAMAP Survey Data
# B. NMFS & NEAMAP Strata
# 1a. Map of Average Biomass by Strata
# 1b. Graph of Average Biomass by Strata over Time
# 2a. Graph of Change in Average Biomass by Strata over Time
# 2b. Map of Change in Average Biomass by Strata (1972-2021)

library(tidyverse)
library(janitor)
library(lubridate)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggbreak)



#----- A. Setup NMFS & NEAMAP Survey Data -------------------------------------------------

surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230504.csv", header = TRUE)
# Remove salinity cols and X because not working with now
surveydata <- surveydata %>% select(-c(X, Surfsalin, Botsalin))

#----- Create Spring and Fall datasets
## surveydata
surveydata.spring <- surveydata[surveydata$Season == "SPRING",]
surveydata.fall <- surveydata[surveydata$Season == "FALL",]

#----- Create 1972-2021 dataset
surveydata.spring <- filter(surveydata.spring, Year >=1972)
surveydata.fall <- filter(surveydata.fall, Year >= 1972)



#----- B. NMFS & NEAMAP Strata -------------------------------------------------
nefsc <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")

# Basemap background
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))


#----- 1a. Average Biomass by Strata ----------------------------------------------

#----- SPRING
# Average Biomass per Stratum
bio.strat.spring <- surveydata.spring %>%
  group_by(Survey, Stratum) %>%
  summarise(AveBio = mean(Biomass)) 

# Range
range(bio.strat.spring$AveBio)
# 0.00000 79.89012

#-- Prep to map Spring
# Add AveBio to NEFSC shp by changing bio.strat.spring to match nefsc shp
colnames(bio.strat.spring)[2] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.ave.spring <- dplyr::left_join(nefsc, bio.strat.spring, by = "STRATA")

# Add AveBio to NEAMAP shp by changing bio.strat.spring to match neamap shp
colnames(bio.strat.spring)[2] = "REGION"
bio.strat.spring <- bio.strat.spring %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09")) 

neamap.ave.spring <- dplyr::left_join(neamap, bio.strat.spring, by = "REGION")

#-- Map Average Biomass for Spring!
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.ave.spring , AveBio >0), aes(fill = AveBio)) +
  geom_sf(data = subset(neamap.ave.spring, AveBio >0), aes(fill = AveBio)) +
  scale_fill_viridis_c() +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  # coord_sf (xlim = c(-85,-60), ylim = c (26,46), expand = FALSE ) + #Full coast
  theme_classic() +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Spring, 1972-2021")


#-- Map the Maximum Strata for Spring
# Max AveBio strata
bio.strat.spring[,2][bio.strat.spring[,3]>1]
# [1] "14"   "15"   "3140" "3290" "3350" "3420" "7560" "7590" "7600" "7630"
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.ave.spring , AveBio >1), aes(fill = "#fde725", color = "#fde725")) +
  geom_sf(data = subset(neamap.ave.spring, AveBio >1), aes(fill = "#fde725", color = "#fde725")) +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  theme_classic() +
  theme(legend.position = "none") +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Spring, 1972-2021")



#----- FALL
# Average Biomass per Stratum
bio.strat.fall <- surveydata.fall %>%
  group_by(Survey, Stratum) %>%
  summarise(AveBio = mean(Biomass)) 

# Range
range(bio.strat.fall$AveBio)
# Max AveBio strata
bio.strat.fall[,2][bio.strat.fall[,3]>0.1]

# Add AveBio to NEFSC shp
colnames(bio.strat.fall)[2] = "STRATA"
nefsc$STRATA <- as.character(nefsc$STRATA)
nefsc.ave.fall <- dplyr::left_join(nefsc, bio.strat.fall, by = "STRATA")

# Add AveBio to neamap shp
colnames(bio.strat.fall)[2] = "REGION"
bio.strat.fall <- bio.strat.fall %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.ave.fall <- dplyr::left_join(neamap, bio.strat.fall, by = "REGION")

#-- Map Average Biomass for Fall!
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.ave.fall , AveBio >0), aes(fill = AveBio)) +
  geom_sf(data = subset(neamap.ave.fall, AveBio >0), aes(fill = AveBio)) +
  scale_fill_viridis_c() +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  # coord_sf (xlim = c(-85,-60), ylim = c (26,46), expand = FALSE ) + #Full coast
  theme_classic() +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Fall, 1972-2021")


# Max AveBio strata
bio.strat.fall[,2][bio.strat.fall[,3]>1]
# "02" "03"
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.ave.fall, AveBio >1), aes(fill = "#fde725", color = "#fde725")) +
  geom_sf(data = subset(neamap.ave.fall, AveBio >1), aes(fill = "#fde725", color = "#fde725")) +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  theme_classic() +
  theme(legend.position = "none") +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Fall, 1972-2021")



#----- 1b. Graph of Average Biomass by Strata over Time ----------------------------------

#----- SPRING
# Average Biomass per Stratum
annual.bio.strat.spring <- surveydata.spring %>%
  group_by(Survey, Stratum, Year) %>%
  summarise(AveBio = mean(Biomass)) 

# Graph Average Biomass by Stratum over Years for Spring, but it's difficult to see what's happening
ggplot(annual.bio.strat.spring, aes(x=Year, y=AveBio, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_y_break(c(100, 1200)) +
  labs(x = " ", y = "Biomass (kg/tow)") +
  theme_classic() +
  theme(legend.position = "none")


# Graph Average Biomass by Stratum over Years for Spring, but it's difficult to see what's happening
ggplot(subset(annual.bio.strat.spring, Stratum %in% c(3, 4, 5, 12, 13,14)), aes(x=Year, y=AveBio, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  # scale_y_break(c(100, 1200)) +
  labs(x = " ", y = "Biomass (kg/tow)") +
  theme_classic() +
  theme(legend.position = "none")


#----- FALL
# Average Biomass per Stratum
annual.bio.strat.fall <- surveydata.fall %>%
  group_by(Survey, Stratum, Year) %>%
  summarise(AveBio = mean(Biomass)) 

# Graph Average Biomass by Stratum over Years for Spring, but it's difficult to see what's happening
ggplot(annual.bio.strat.fall, aes(x=Year, y=AveBio, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = " ", y = "Biomass (kg/tow)") +
  theme_classic() +
  theme(legend.position = "none")



#----- 2a. Graph of Change in Average Biomass by Strata over Time -----------------------

#----- SPRING
# Annual change in Biomass per Stratum (Difference between current and previous year)
change.bio.strat.spring <- annual.bio.strat.spring %>%
  group_by(Stratum) %>%
  mutate(DeltaBio = AveBio - lag(AveBio, default = 0))

# Graph annual change
ggplot(change.bio.strat.spring, aes(x=Year, y=DeltaBio, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  scale_y_break(c(250, 1300)) +
  labs(x = " ", y = "Biomass (kg/tow)") +
  theme_classic() +
  theme(legend.position = "none")


#----- FALL
# Annual change in Biomass per Stratum (Difference between current and previous year)
change.bio.strat.fall <- annual.bio.strat.fall %>%
  group_by(Stratum) %>%
  mutate(DeltaBio = AveBio - lag(AveBio, default = 0))

# Graph annual change
ggplot(change.bio.strat.fall, aes(x=Year, y=DeltaBio, color = Stratum)) +
  geom_point() +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = " ", y = "Biomass (kg/tow)") +
  theme_classic() +
  theme(legend.position = "none")



#----- 2b. Map of Change in Average Biomass by Strata (1972-2021) -----------------------

#----- SPRING
# Average annual change in Biomass per Stratum
totchange.bio.strat.spring <- change.bio.strat.spring %>%
  group_by(Stratum) %>%
  summarise(AveDeltaBio = mean(DeltaBio))

## Map delta biomass by strata
# Add ann.abun.strat$AveAbun to NEFSC shp
colnames(totchange.bio.strat.spring)[1] = "STRATA"
nefsc.deltabio.spring <- dplyr::left_join(nefsc, totchange.bio.strat.spring, by = "STRATA")

# Add delta biomass to neamap shp
colnames(totchange.bio.strat.spring)[1] = "REGION"
totchange.bio.strat.spring <- totchange.bio.strat.spring %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.deltabio.spring <- dplyr::left_join(neamap, totchange.bio.strat.spring, by = "REGION")

# Map SPRING
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.deltabio.spring, AveDeltaBio <0), fill = "#440154") +
  geom_sf(data = subset(neamap.deltabio.spring, AveDeltaBio <0), fill = "#440154") +
  geom_sf(data = subset(nefsc.deltabio.spring, AveDeltaBio >0), fill = "#fde725") +
  geom_sf(data = subset(neamap.deltabio.spring, AveDeltaBio >0), fill = "#fde725") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  # coord_sf (xlim = c(-85,-60), ylim = c (26,46), expand = FALSE ) + #Full coast
  theme_classic() +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Spring, Average Annual Change 1972-2021")


#----- FALL
# Average annual change in Biomass per Stratum
totchange.bio.strat.fall <- change.bio.strat.fall %>%
  group_by(Stratum) %>%
  summarise(AveDeltaBio = mean(DeltaBio))

## Map delta biomass by strata
# Add ann.abun.strat$AveAbun to NEFSC shp
colnames(totchange.bio.strat.fall)[1] = "STRATA"
nefsc.deltabio.fall <- dplyr::left_join(nefsc, totchange.bio.strat.fall, by = "STRATA")

# Add delta biomass to neamap shp
colnames(totchange.bio.strat.fall)[1] = "REGION"
totchange.bio.strat.fall <- totchange.bio.strat.fall %>%
  mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
  mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
  mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
  mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
  mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
  mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
  mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
  mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
  mutate(REGION = replace(REGION, REGION == "9", "09"))

neamap.deltabio.fall <- dplyr::left_join(neamap, totchange.bio.strat.fall, by = "REGION")


# Map FALL
ggplot(data = world) +  
  geom_sf(data = us, color = "gray", fill = "white") +
  geom_sf(data = canada, color = "gray", fill = "white") +
  geom_sf(data = nefsc, aes(), color = "grey50") +
  geom_sf(data = neamap, aes(), color = "grey50") +
  geom_sf(data = subset(nefsc.deltabio.fall, AveDeltaBio <0), fill = "#440154") +
  geom_sf(data = subset(neamap.deltabio.fall, AveDeltaBio <0), fill = "#440154") +
  geom_sf(data = subset(nefsc.deltabio.fall, AveDeltaBio >0), fill = "#fde725") +
  geom_sf(data = subset(neamap.deltabio.fall, AveDeltaBio >0), fill = "#fde725") +
  coord_sf (xlim = c(-81,-63.5), ylim = c (32,45), expand = FALSE ) + #Zoomed in to survey extent
  # coord_sf (xlim = c(-85,-60), ylim = c (26,46), expand = FALSE ) + #Full coast
  theme_classic() +
  xlab("longitude") + 
  ylab("latitude")+
  ggtitle("Fall, Average Annual Change 1972-2021")



#----- 3. Map of Change in Average Biomass by Between Seasons -----------------------




#------------------------------ Not sure if I need this below -------------------
# ## Map by Year
# # Add ann.abun.strat$AveAbun to NEFSC shp
# colnames(ann.abun.strat)[1] = "STRATA"
# nefsc$STRATA <- as.character(nefsc$STRATA)
# nefsc.yrave.spring <- dplyr::right_join(nefsc, ann.abun.strat, by = "STRATA")
# 
# # Add AveAbun to neamap shp
# colnames(ann.abun.strat)[1] = "REGION"
# ann.abun.strat <- ann.abun.strat %>%
#   mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
#   mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
#   mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
#   mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
#   mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
#   mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
#   mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
#   mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
#   mutate(REGION = replace(REGION, REGION == "9", "09"))
# 
# neamap.yrave.spring <- dplyr::right_join(neamap, ann.abun.strat, by = "REGION")
# 
# # Map by Year, Season
# ggplot() +
#   geom_sf(data = subset(neamap.yrave.spring , AveAbun >0), aes(fill = AveAbun)) +
#   geom_sf(data = subset(neamap.yrave.spring, AveAbun >0), aes(fill = AveAbun)) +
#   scale_fill_viridis_c() +
#   theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
#         strip.background = element_blank()) + #lines for facet wrapping
#   facet_wrap('Year', ncol = 8) + #lines for facet wrapping
#   theme_minimal() +
#   ggtitle("Spring")
# 
# 
# #####
# ## GAM of Ave Abundance by Strata over Years
# #####
# library(mgcv)
# 
# # Predict Abundance for each Strata over time, with smoother over Year
# gam.1 = gam(AveAbun ~ s(Year), data = ann.abun.strat)
# summary(gam.1)
# plot(gam.1)
# 
# preddata <- data.frame(Year = ann.abun.strat$Year) #,
# pred.gam.abunstrata.1 <- predict.gam(gam.1, newdata = preddata, type="response", se.fit=TRUE)
# head(pred.gam.abunstrata.1)
# 
# plot(log(ann.abun.strat$AveAbun) ~ ann.abun.strat$Year)
# lines(preddata$Year, pred.gam.abunstrata$fit, col="red")
# lines(preddata$Year, pred.gam.abunstrata$fit+2*pred.gam.abunstrata$se.fit,lty=2, col="red")
# lines(preddata$Year, pred.gam.abunstrata$fit-2*pred.gam.abunstrata$se.fit,lty=2, col="red")
# 
# 
# gam.2 = gam(AveAbun ~ s(Year) + Stratum, data = ann.abun.strat)
# summary(gam.2)
# plot(gam.2)
# 
# preddata <- data.frame(Stratum = ann.abun.strat$Stratum, Year = ann.abun.strat$Year) #,
# pred.gam.abunstrata.2 <- predict.gam(gam.2, newdata = preddata, type="response", se.fit=TRUE)
# head(pred.gam.abunstrata.2)
# 
# plot(log(ann.abun.strat$AveAbun) ~ ann.abun.strat$Year, col = factor(ann.abun.strat$Stratum))
# lines(preddata$Year, pred.gam.abunstrata.2$fit, col="red")
# lines(preddata$Year, pred.gam.abunstrata.2$fit+2*pred.gam.abunstrata.2$se.fit,lty=2, col="red")
# lines(preddata$Year, pred.gam.abunstrata.2$fit-2*pred.gam.abunstrata.2$se.fit,lty=2, col="red")
# 
# 
# # Predict Abundance for each Strata over time, with smoother over Year
# gam.2 = gam(AveAbun ~ s(Year, by = Year) + Stratum, data = ann.abun.strat)
# summary(gam.2)
# plot(gam.2)
# 
# gam.3 = gam(AveAbun ~ s(Year, by = Stratum) + Stratum, data = ann.abun.strat)
# summary(gam.3)
# plot(gam.3)
# 
# 
# ########################################################
# #### 8. Average biomass by strata
# ########################################################
# 
# # Average Abundance per Stratum
# biom.strat <- surv.spring %>%
#   group_by(Survey, Stratum) %>%
#   summarise(AveBiom = mean(Biomass)) 
# 
# ## Map of Average Abundance
# # Add NEFSC and NEAMAP survey areas, repeat from beginning above
# # nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# # plot(nefsc)
# # neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
# # plot(neamap)
# 
# # Add AveAbun to NEFSC shp
# colnames(biom.strat)[2] = "STRATA"
# nefsc$STRATA <- as.character(nefsc$STRATA)
# nefsc.aveb <- dplyr::left_join(nefsc, biom.strat, by = "STRATA")
# 
# # Add AveAbun to neamap shp
# colnames(biom.strat)[2] = "REGION"
# biom.strat <- biom.strat %>%
#   mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
#   mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
#   mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
#   mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
#   mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
#   mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
#   mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
#   mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
#   mutate(REGION = replace(REGION, REGION == "9", "09"))
# 
# neamap.aveb <- dplyr::left_join(neamap, biom.strat, by = "REGION")
# 
# 
# ggplot() +
#   geom_sf(data = subset(nefsc.aveb , AveBiom >0), aes(fill = AveBiom)) +
#   geom_sf(data = subset(neamap.aveb, AveBiom >0), aes(fill = AveBiom)) +
#   scale_fill_viridis_c() +
#   theme_minimal() 
# 
# 
# 
# ########################################################
# #### 9. Average biomass by strata over time
# ########################################################
# 
# # Average Biomass per Stratum
# ann.biom.strat <- surv.spring %>%
#   group_by(Stratum, Year) %>%
#   summarise(AveBiom = mean(Biomass)) 
# 
# ggplot(ann.biom.strat, aes(x=Year, y=AveBiom, color = Stratum)) +
#   geom_point() +
#   geom_line() +
#   scale_color_viridis_d() +
#   labs(x = " ", y = "Biomass") +
#   theme_classic() +
#   theme(legend.position = "none")
# 
# ## Map by Year
# # Add ann.biom.strat$AveBiom to NEFSC shp
# colnames(ann.biom.strat)[1] = "STRATA"
# nefsc$STRATA <- as.character(nefsc$STRATA)
# nefsc.yraveb <- dplyr::right_join(nefsc, ann.biom.strat, by = "STRATA")
# 
# # Add AveBiom to neamap shp
# colnames(ann.biom.strat)[1] = "REGION"
# ann.biom.strat <- ann.biom.strat %>%
#   mutate(REGION = replace(REGION, REGION == "1", "01")) %>%
#   mutate(REGION = replace(REGION, REGION == "2", "02")) %>%
#   mutate(REGION = replace(REGION, REGION == "3", "03")) %>%
#   mutate(REGION = replace(REGION, REGION == "4", "04")) %>%
#   mutate(REGION = replace(REGION, REGION == "5", "05")) %>%
#   mutate(REGION = replace(REGION, REGION == "6", "06")) %>%
#   mutate(REGION = replace(REGION, REGION == "7", "07")) %>%
#   mutate(REGION = replace(REGION, REGION == "8", "08")) %>%
#   mutate(REGION = replace(REGION, REGION == "9", "09"))
# 
# neamap.yraveb <- dplyr::right_join(neamap, ann.biom.strat, by = "REGION")
# 
# # Map by Year
# ggplot() +
#   geom_sf(data = subset(nefsc.yraveb , AveBiom >0), aes(fill = AveBiom)) +
#   geom_sf(data = subset(neamap.yraveb, AveBiom >0), aes(fill = AveBiom)) +
#   scale_fill_viridis_c() +
#   theme(strip.text.x = element_text(size = 12), #lines for facet wrapping
#         strip.background = element_blank()) + #lines for facet wrapping
#   facet_wrap('Year', ncol = 8) + #lines for facet wrapping
#   theme_minimal() 
