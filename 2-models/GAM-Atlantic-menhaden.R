# GAM-Atlantic-menhaden.R
######################################
# Janelle L. Morano
# GAM model of menhaden distribution
# Primarily to compare with VAST model
# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 31 January 2023
###############################################
###############################################

#### 1. VAST mimic: GAM of Biomass by smooth(Year), Stratum, Bottemp, Depth
#### 2. VAST mimic: GAM of log(Abundance +1) by smooth(Year), Stratum, Bottemp, Depth
#### 3. GAM of log(Abundance+) by Year, Season, Bottemp, Depth, Lat
#### 4. GAM of Biomass by smooth(Year), Stratum, Bottemp, Depth

# Average density for each strata, over time
# Average density for each strata, over time, smooth year

library(tidyverse)
library(janitor)
# library(viridisLite)
# library(viridis)


surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl.csv", header = TRUE)
# Remove salinity cols because not working with now
surveydata <- select(surveydata, Survey, Stratum, Year, Season, Lat, Lon, Depth, Bottemp, Abundance, Biomass)
# remove NAs
sapply(surveydata, function(x) sum(is.na(x)))
surveydata <- na.omit(surveydata)


#### VAST mimic: GAM of Biomass by smooth(Year), Stratum, Bottemp, Depth
####################################################################
library(mgcv)

# Spring
springdata <- surveydata[surveydata$Season == "SPRING",]
# springdata <- surveydata[surveydata$Season == "SPRING" & surveydata$Biomass > 0,]

spring.gam = gam(Biomass ~ s(Year) + Stratum + Bottemp + Depth, data = springdata)
options(max.print=5.5E5)
summary(spring.gam)
plot(spring.gam, main = "Spring")

preddata <- data.frame(Year = springdata$Year,
                       Stratum = springdata$Stratum,
                       Bottemp = springdata$Bottemp,
                       Depth = springdata$Depth)
pred.spring.gam <- predict.gam(spring.gam, newdata = preddata, se.fit=TRUE)
head(pred.spring.gam)

plot(springdata$Biomass ~ springdata$Year)
lines(preddata$Year, pred.spring.gam$fit, col="red")
lines(preddata$Year, pred.spring.gam$fit+2*pred.spring.gam$se.fit,lty=2, col="red")
lines(preddata$Year, pred.spring.gam$fit-2*pred.spring.gam$se.fit,lty=2, col="red")

# Plot with y axis break
library(ggbreak) 
ggplot(springdata, aes(x=Year, y=Biomass)) +
  geom_point() +
  scale_y_break(c(700, 3600)) 

# Plot with mgcViz
library(mgcViz)
b <- getViz(spring.gam)
plot( sm(b, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  scale_y_break(c(50, 3600)) +
  theme_classic()


# Map >.05 sig strata
library(sf)
library(tidyverse)
strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
strata.sig <- strata %>%
  filter(STRATA %in% c(12,13,14,15,2,3270,3360,3390,3400,3420,3430,3440,4,7500,7510,7520,7530,7540,7560,7600,7620,7630,7770))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Spring")


## Fall
falldata <- surveydata[surveydata$Season == "FALL",]

fall.gam = gam(log(Abundance +1) ~ s(Year) + Stratum + Bottemp + Depth, data = falldata)
options(max.print=5.5E5)
summary(fall.gam)
plot(fall.gam, main = "Fall")

preddata <- data.frame(Year = falldata$Year,
                       Stratum = falldata$Stratum,
                       Bottemp = falldata$Bottemp,
                       Depth = falldata$Depth)
pred.fall.gam <- predict.gam(fall.gam, newdata = preddata, se.fit=TRUE)
head(pred.fall.gam)

plot(log(falldata$Abundance +1) ~ falldata$Year)
lines(preddata$Year, pred.fall.gam$fit, col="red")
lines(preddata$Year, pred.fall.gam$fit+2*pred.fall.gam$se.fit,lty=2, col="red")
lines(preddata$Year, pred.fall.gam$fit-2*pred.fall.gam$se.fit,lty=2, col="red")

# Map >.05 sig strata
# library(sf)
# library(tidyverse)
# strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
strata.sig <- strata %>%
  filter(STRATA %in% c(10,11,12,2,3,3120,3150,3180,3210,3240,3270,3300,3310,3360,3390,3420,4,6,7,7500,7710,7830,7860))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Fall")




#### VAST mimic: GAM of log(Abundance +1) by smooth(Year), Stratum, Bottemp, Depth
####################################################################

# Spring
################
springdata <- surveydata[surveydata$Season == "SPRING",]

ab.spring.gam = gam(log(Abundance +1) ~ s(Year) + Stratum + Bottemp + Depth, data = springdata)
options(max.print=5.5E5)
summary(ab.spring.gam)
plot(ab.spring.gam, main = "Spring")

preddata.ab <- data.frame(Year = springdata$Year,
                       Stratum = springdata$Stratum,
                       Bottemp = springdata$Bottemp,
                       Depth = springdata$Depth)
ab.pred.spring.gam <- predict.gam(ab.spring.gam, newdata = preddata.ab, se.fit=TRUE)
head(ab.pred.spring.gam)

plot(springdata$Abundance ~ springdata$Year)
lines(preddata.ab$Year, ab.pred.spring.gam$fit, col="red")
lines(preddata.ab$Year, ab.pred.spring.gam$fit+2*ab.pred.spring.gam$se.fit,lty=2, col="red")
lines(preddata.ab$Year, ab.pred.spring.gam$fit-2*ab.pred.spring.gam$se.fit,lty=2, col="red")


# Plot with mgcViz
library(mgcViz)
b <- getViz(ab.spring.gam)
plot( sm(b, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  scale_y_break(c(750, 3600)) +
  theme_classic() +
  ggtitle("Spring: Abundance")


# Map >.05 sig strata
library(sf)
library(tidyverse)
strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
strata.sig <- strata %>%
  filter(STRATA %in% c(14,3350))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Spring")


## Fall
################
falldata <- surveydata[surveydata$Season == "FALL",]

fall.gam = gam(log(Abundance +1) ~ s(Year) + Stratum + Bottemp + Depth, data = falldata)
options(max.print=5.5E5)
summary(fall.gam)
plot(fall.gam, main = "Fall")

preddata <- data.frame(Year = falldata$Year,
                       Stratum = falldata$Stratum,
                       Bottemp = falldata$Bottemp,
                       Depth = falldata$Depth)
pred.fall.gam <- predict.gam(fall.gam, newdata = preddata, se.fit=TRUE)
head(pred.fall.gam)

plot(log(falldata$Abundance +1) ~ falldata$Year)
lines(preddata$Year, pred.fall.gam$fit, col="red")
lines(preddata$Year, pred.fall.gam$fit+2*pred.fall.gam$se.fit,lty=2, col="red")
lines(preddata$Year, pred.fall.gam$fit-2*pred.fall.gam$se.fit,lty=2, col="red")

# Map >.05 sig strata
# library(sf)
# library(tidyverse)
# strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
strata.sig <- strata %>%
  filter(STRATA %in% c(10,11,12,2,3,3120,3150,3180,3210,3240,3270,3300,3310,3360,3390,3420,4,6,7,7500,7710,7830,7860))
ggplot() +
  geom_sf(data = strata) +
  geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Fall")




#### GAM of log(Abundance+) by Year, Season, Bottemp, Depth, Lat
####################################################################

abun.gam = gam(log(Abundance +1) ~ s(Year) + Season + Bottemp + Depth + Lat, data = surveydata)
summary(abun.gam)
plot(abun.gam)

preddata <- data.frame(Year = surveydata$Year,
                       Season = surveydata$Season,
                       Bottemp = surveydata$Bottemp,
                       Depth = surveydata$Depth,
                       Lat = surveydata$Lat)
pred.abun.gam <- predict.gam(abun.gam, newdata = preddata, se.fit=TRUE)
head(pred.abun.gam)

plot(log(surveydata$Abundance +1) ~ surveydata$Year)
lines(preddata$Year, pred.abun.gam$fit, col="red")
lines(preddata$Year, pred.abun.gam$fit+2*pred.abun.gam$se.fit,lty=2, col="red")
lines(preddata$Year, pred.abun.gam$fit-2*pred.abun.gam$se.fit,lty=2, col="red")


#### Average density for each strata, over time
########################################################

# Spring
################
springdata <- surveydata[surveydata$Season == "SPRING",]

ab.spring.gam = gam(log(Abundance +1) ~ s(Year) + Stratum + Bottemp + Depth, data = springdata)
options(max.print=5.5E5)
summary(ab.spring.gam)
plot(ab.spring.gam, main = "Spring")

preddata.ab <- data.frame(Year = springdata$Year,
                          Stratum = springdata$Stratum,
                          Bottemp = springdata$Bottemp,
                          Depth = springdata$Depth)
ab.pred.spring.gam <- predict.gam(ab.spring.gam, newdata = preddata.ab, se.fit=TRUE)
head(ab.pred.spring.gam)

plot(springdata$Abundance ~ springdata$Year)
lines(preddata.ab$Year, ab.pred.spring.gam$fit, col="red")
lines(preddata.ab$Year, ab.pred.spring.gam$fit+2*ab.pred.spring.gam$se.fit,lty=2, col="red")
lines(preddata.ab$Year, ab.pred.spring.gam$fit-2*ab.pred.spring.gam$se.fit,lty=2, col="red")

