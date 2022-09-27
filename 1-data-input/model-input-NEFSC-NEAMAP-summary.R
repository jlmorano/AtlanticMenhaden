# NEFSC-NEAMAP_data_summary.R
######################################
# Janelle L. Morano
# Summary of NEFSC and NEAMAP data
# These data are used in the VAST menhaden model
# These analyses are for intuition of what is happening with menhaden and temperature

# last updated 15 August 2022
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


#### Abundance vs Year
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

#### Model compariison
AIC(abund.lm)
AIC(abun.gam)


############################
#### Abundance vs Temp
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

#### Abundance at Sample Sites within Statistical survey areas
############################
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

nefsc<- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
plot(nefsc)
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")
plot(neamap)

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

library(gganimate)
library(gifski)
# Plot Abundance at survey locations
# Spring
# surv.spring.sub <- surv.spring %>% filter(Year >= 2007 & Year <= 2019 )
surv.spring.sub <- surv.spring %>% filter(Year == 2019 )

ggplot(data = world) +
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
             size=3, stroke=0.75,shape=16, color = "red") +
  # scale_color_viridis_c(option = "viridis") +
  theme_classic() +
  theme(axis.text = element_blank()) +
  theme(strip.text.x = element_text(size = 12),
        strip.background = element_blank()) +
  facet_wrap('Year', ncol = 4) +
  labs(x= "longitude", 
       y = "latitude",
       title = "Spring") 
  # transition_states(YEAR,
  #                 transition_length = 2,
  #                 state_length = 1) +
  # enter_fade() +
  # exit_shrink() + 
  # save_gif("menhaden_animation_2007-2019.gif")

# Fall
ggplot(data = world) +
  geom_sf(data = nefsc, aes()) +
  coord_sf (xlim = c(-81,-63), ylim = c (32,46), expand = FALSE ) +
  geom_point(data = subset(surv.fall, Year %in% c(2009)) %>%
               arrange(Abundance),
             aes (x = Lon, y = Lat, color = log(Abundance)),
             size=.5, stroke=0.75,shape=16) +
  scale_color_viridis_c(option = "viridis") +
  theme_bw() +
  theme(axis.text = element_blank()) +
  theme(strip.text.x = element_text(size = 15),
        strip.background = element_blank()) +
  facet_wrap('Year', ncol = 4) +
  labs(x= "longitude", 
       y = "latitude",
       title = "Fall")
