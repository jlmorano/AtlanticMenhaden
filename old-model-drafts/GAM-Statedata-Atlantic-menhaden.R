# GAM-Statedata-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Using state surveys, Objectives:
# 1. Determine the affect of temperature and chlorophyll on menhaden BIOMASS abundance
# 2. Determine the spatial areas of highest menhaden density
# 3. Use GAM model of menhaden distribution

# last updated 7 June 2023
###############################################
###############################################

#----- Table of Contents ---------------------------------------------------------------------------------

# Load State Data
# 1. GAM: Biomass (weight CPUE: kg/tow) ~ as.factor(Year) + s(SurfTemp) + s(Depth) + Survey, family ="quasipoisson"(link="log")
# 2. GAM: Biomass (weight CPUE: kg/tow) ~ s(Year) + s(SurfTemp) + s(Depth) + Survey, family ="quasipoisson"(link="log")


library(tidyverse)
library(janitor)
library(mgcv)



#----- Load State Data -----------------------------------------------------------------------------------

statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data_20230607.csv", header = TRUE)

sapply(statedata, function(x) sum(is.na(x)))

#----- Create Spring and Fall datasets
statedata.spring <- statedata[statedata$Month > 2 & statedata$Month < 5,]
statedata.fall <- statedata[statedata$Month > 8 & statedata$Month <11 ,]


# Create list of datasets
state.data.list <- list(state.spring = statedata.spring, state.fall= statedata.fall)
# Save list as object
saveRDS(state.data.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/state.data.list.rds")




#----- 1. GAM of Biomass by as.factor(Year), smooth(SurfTemp), smooth(Depth), as.factor(Survey) ------------------------------------------------------------------------------------------------------------------

#----- Run the model
state.gam.list <- list()
for (name in names(state.data.list[1:2])) {
  state.gam.list[[name = name]] = gam(Weight.kg ~ as.factor(Year) + s(SurfTemp) + s(Depth.m) + as.factor(Survey), family ="quasipoisson"(link="log"), data = state.data.list[[name]], method = "REML")
}

summary(state.gam.list[[1]])
summary(state.gam.list[[2]])
flextable::as_flextable(state.gam.list[[1]])
flextable::as_flextable(state.gam.list[[2]])
par(mfrow = c(2, 2))
plot(state.gam.list[[1]], all.terms = TRUE)
par(mfrow = c(2, 2))
plot(state.gam.list[[2]], all.terms = TRUE)


#----- Predictions 
newstatedata.spring <- data.frame()
for (i in unique(statedata.spring$Survey)) {
	new <- data.frame(Year = sort(unique(statedata.spring$Year[statedata.spring$Survey == "ChesMMAP"])),
						Survey = i,
						SurfTemp = median(statedata.spring$SurfTemp, na.rm=TRUE),
						Depth.m = median(statedata.spring$Depth, na.rm=TRUE))
	newstatedata.spring <- rbind(newstatedata.spring, new)
}
#newstatedata.spring$Year[which(!(newstatedata.spring$Year %in% unique(statedata.spring$Year)))] <- NA


# Just one survey at a time then!
newstatedata.spring <- data.frame(Year = sort(unique(statedata.spring$Year)),
									Survey = "ChesMMAP",
									SurfTemp = median(statedata.spring$SurfTemp, na.rm=TRUE),
									Depth.m = median(statedata.spring$Depth, na.rm=TRUE))


predictions.state.spring <- predict(state.gam.list[[1]], se.fit=TRUE, newdata= newstatedata.spring, type = "response")
range(log(statedata.spring$Weight.kg))
range(predictions.state.spring$fit)
plot(newstatedata.spring$Year, predictions.state.spring$fit,type="b")  



# Menhaden Catch over Years
state.gam = gam(MenhadenTotal ~ s(Year) + SurfTemp, data = statedata)
summary(state.gam)
plot(state.gam, main = "State Surveys")




#----- 2. GAM of Biomass by smooth(Year, by = as.factor(Survey)), smooth(SurfTemp), smooth(Depth), as.factor(Survey) ------------------------------------------------------------------------------------------------------------------

#----- Run the model
state.gam2.list <- list()
for (name in names(state.data.list[1:2])) {
  state.gam2.list[[name = name]] = gam(Weight.kg ~ s(Year, by = as.factor(Survey)) + s(SurfTemp) + s(Depth.m) + as.factor(Survey), family ="quasipoisson"(link="log"), data = state.data.list[[name]], method = "REML")
}

gam.check(state.gam2.list[[1]])
gam.check(state.gam2.list[[2]])
summary(state.gam2.list[[1]])
summary(state.gam2.list[[2]])

par(mfrow = c(2, 2))
plot(state.gam2.list[[1]], all.terms = TRUE)
par(mfrow = c(2, 2))
plot(state.gam2.list[[2]], all.terms = TRUE)

#----- Predictions Spring
newstatedata2.spring <- data.frame()
for (i in unique(statedata.spring$Survey)) {
	new <- data.frame(Year = sort(unique(statedata.spring$Year)),
						Survey = i,
						SurfTemp = median(statedata.spring$SurfTemp, na.rm=TRUE),
						Depth.m = median(statedata.spring$Depth, na.rm=TRUE))
	newstatedata2.spring <- rbind(newstatedata2.spring, new)
}
# No WLI, so drop from newstatedata.2.spring
newstatedata2.spring <- newstatedata2.spring[newstatedata2.spring$Survey != "WLI",]
newstatedata2.spring <- newstatedata2.spring[newstatedata2.spring$Survey != "SEAMAP",]


predictions2.state.spring <- predict(state.gam2.list[[1]], se.fit=TRUE, newdata= newstatedata2.spring, type = "response")
range(log(statedata.spring$Weight.kg))
range(predictions2.state.spring$fit)
plot(newstatedata2.spring$Year, predictions2.state.spring$fit,type="b")   

#----- Create Dataset of Predictions from GAM2 spring
predictions.gam2.state.spring <- cbind(newstatedata2.spring, data.frame(predictions2.state.spring))
# Save df as csv
write.csv(predictions.gam2.state.spring, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam2.state.spring_20230612.csv", row.names = TRUE)  


#----- Predictions Fall
newstatedata2.fall <- data.frame()
for (i in unique(statedata.fall$Survey)) {
	new <- data.frame(Year = sort(unique(statedata.fall$Year)),
						Survey = i,
						SurfTemp = median(statedata.fall$SurfTemp, na.rm=TRUE),
						Depth.m = median(statedata.fall$Depth, na.rm=TRUE))
	newstatedata2.fall <- rbind(newstatedata2.fall, new)
}
# No WLI, so drop from newstatedata.2.spring
newstatedata2.fall <- newstatedata2.fall[newstatedata2.fall$Survey != "WLI",]
newstatedata2.fall <- newstatedata2.fall[newstatedata2.fall$Survey != "SEAMAP",]

predictions2.state.fall <- predict(state.gam2.list[[1]], se.fit=TRUE, newdata= newstatedata2.fall, type = "response")
range(log(statedata.fall$Weight.kg))
range(predictions2.state.fall$fit)
plot(newstatedata2.fall$Year, predictions2.state.fall$fit,type="b")   

#----- Create Dataset of Predictions from GAM2 fall
predictions.gam2.state.fall <- cbind(newstatedata2.fall, data.frame(predictions2.state.fall))
# Save df as csv
write.csv(predictions.gam2.state.fall, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam2.state.fall_20230612.csv", row.names = TRUE)  



#----- Map >.05 sig surveys ----------------------------------------------------------------------------

library(sf)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)

# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

nefsc <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
neamap <- st_read("/Users/janellemorano/DATA/NEAMAP/NEAMAP Strata/NMDepthStrataPolgyons.shp")

statedata.sub.spring <- statedata.spring %>%
  filter(Survey == "ChesMMAP" &
         Survey == "GAEMTS")

# Map surveys
ggplot(data = world) +  
  geom_sf(data = us, fill = "grey") + #CCCC99
  geom_sf(data = canada, fill = "grey") +  
  # geom_sf(data = nefsc, color = "lightgrey", fill = "white") + #This adds the NEFSC strata as an option
  # geom_sf(data = neamap, color = "lightgrey", fill = "white") + #This adds the NEAMAP strata as an option
  geom_point(data = statedata.sub.spring, aes(Longitude, Latitude, col = Survey), inherit.aes = FALSE, size = 0.3) + #color = "#253494"
  coord_sf (xlim = c(-85,-60), ylim = c (26,46), expand = FALSE ) + #Full coast
  # coord_sf (xlim = c(-77,-69), ylim = c (35,42), expand = FALSE ) + #Zoomed in
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + # slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  theme(legend.position = "none") +
  ggtitle("1972-2021, Spring")
