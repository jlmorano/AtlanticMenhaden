# GAM-LongIsland-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Objectives:
#  1. What is the relationship between menhaden abundance/biomass and temperature and spatial location?
#  2. Are there more menhaden in certain regions, latitudinally or inshore/offshore?

# Using state data from "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv"

# GAM model
# This is for starting to build up state data analysis as I get more state data

# last updated 28 March 2023
###############################################
###############################################

state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", header = TRUE)

####################################################################
#### GAM of Count by smooth(Year), Water Temp
####################################################################
library(mgcv)

# Menhaden Catch over Years
state.gam = gam(MenhadenTotal ~ s(Year) + SurfTemp, data = statedata)
summary(state.gam)
plot(state.gam, main = "CTLISTS, WLI, NJOT, DEBay, ChesMMAP, GAEMTS")

# Plot with mgcViz
# library(mgcViz)
# b <- getViz(state.gam)
# plot( sm(b, 1)) +
#   l_fitLine(colour = "red") +
#   l_ciLine(colour = "blue", linetype = 2) +
#   l_points(shape = 19, size = 1, alpha = 0.1) +
#   # scale_y_break(c(50, 3600)) +
#   theme_classic()



####################################################################
#### GAM of Count by smooth(Year), Water Temp, Survey (as a proxy for location)
####################################################################

state.gam2 = gam(MenhadenTotal ~ s(Year) + SurfTemp + Survey, data = statedata)
summary(state.gam2)
plot(state.gam2, main = "CTLISTS, WLI, NJOT, DEBay, ChesMMAP, GAEMTS")



####################################################################
#### GAM of Count by smooth(Year), Water Temp, Latitude block (as a proxy for location)
####################################################################

range(statedata$Latitude, na.rm = TRUE)
statedata = mutate(statedata, LatRange = round(Latitude, 0))
unique(statedata$LatRange)

ggplot(statedata, aes(x = LatRange, y = MenhadenTotal)) +
  geom_point() +
  coord_flip() +
  theme_classic()

plot(statedata$MenhadenTotal, statedata$Latitude)

state.gam3 = gam(MenhadenTotal ~ s(Year) + SurfTemp + round(Latitude,1), data = statedata)
summary(state.gam3)
boxplot(state.gam3, main = "CTLISTS, WLI, NJOT, DEBay, ChesMMAP, GAEMTS")

