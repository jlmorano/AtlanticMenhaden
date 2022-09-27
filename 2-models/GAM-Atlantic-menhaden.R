# GAM-Atlantic-menhaden.R
######################################
# Janelle L. Morano
# Using NEFSC and NEAMAP data that goes into VAST menhaden model
# Built a GAM to try it out

# last updated 16 August 2022
###############################################
###############################################

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


#### Abundance vs Year by Season
############################
ggplot(surveydata, aes(x=Year, y=log(Abundance +1), color = Season)) +
  geom_point() +
  theme_bw() 


#### GAM of log(Abundance+) by Year, Season, Bottemp, Depth, Lat
library(mgcv)

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
