# Visualize-GAMmodel-output.R
######################################
# Janelle L. Morano

# Description: Take the output (objects and dataframes) from GAM models run in "GAM-Atlantic-menhaden.R" and "GAM-Statedata-Atlantic-menhaden.R" and visualize the fit, etc for inspection, analysis and presentation graphics.

# last updated 10 May 2023
###############################################
###############################################

#----- Table of Contents -------------------------------------------------

# Setup survey data and GAM model output
# 1. Graphing Prediction on Biomass v Year (by State)
# 2. Graphing Prediction on Biomass v Year (by Strata)

library(tidyverse)
library(viridis)
library(ggbreak)
library(geomtextpath)

#----- Setup survey data and GAM model output -------------------------------------------------

# # Data with State Code for GAM3
# predictions.gam3.spring <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam3.spring.csv", header = TRUE)
# predictions.gam3.fall <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam3.fall.csv", header = TRUE)
# 
# # Data with State Code for GAM4
# predictions.gam4.spring <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4.spring.csv", header = TRUE)
# predictions.gam4.fall <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4.fall.csv", header = TRUE)

# Data with State Code for GAM4b
predictions.gam4b.spring <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4b.spring.csv", header = TRUE)
predictions.gam4b.fall <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4b.fall.csv", header = TRUE)



#----- 1. FEDERAL Graphing Prediction on Biomass v Year (by State) --------------------------------------------------------

# Color palette for North to South States AKA Dark to Light
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent


#----- Spring
ggplot() +
  # FIT LINES
  # # RI&MA
  # geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("RI&MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  # geom_line(data = subset(predictions.gam4b.spring, State %in% c("RI&MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  # # NYBLI
  # geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("NYBLI")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  # geom_line(data = subset(predictions.gam4b.spring, State %in% c("NYBLI")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  # # NJ
  # geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  # geom_line(data = subset(predictions.gam4b.spring, State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  # # DEMD
  # geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  # geom_line(data = subset(predictions.gam4b.spring, State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  # VANC
  geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("VANC")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.gam4b.spring, State %in% c("VANC")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  # NCGA
  geom_ribbon(data = subset(predictions.gam4b.spring, State %in% c("NCGA")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.gam4b.spring, State %in% c("NCGA")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  # ylim(0,5) +
  # scale_y_break(c(0,1000), scales = 100) + scale_y_break(c(1000,1000), scales = 1000) +
  # scale_y_break(c(1000,1000), scales = 1000) +
  # scale_y_continuous(limits = c(0, 100), breaks = c(0, 100,1000, 2000)) +
  # geom_textline(label = "State", size = 6, linewidth = 2, hjust = 1) + #with geomtextpath()
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  # ylim(0, 0.15) + #For RI, NY, NJ
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Spring")
# plot just RI, NY, NJ, and VANC first 500x500
# then plot DE, NCGA


#----- FALL
ggplot() +
  #FIT LINES
  # RI&MA
  geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("RI&MA")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.gam4b.fall, State %in% c("RI&MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  # # NYBLI
  geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("NYBLI")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.gam4b.fall, State %in% c("NYBLI")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  # NJ
geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("NJ")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[3]) +
geom_line(data = subset(predictions.gam4b.fall, State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
# DEMD
geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("DEMD")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[4]) +
geom_line(data = subset(predictions.gam4b.fall, State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
# VANC
geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("VANC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[5]) +
geom_line(data = subset(predictions.gam4b.fall, State %in% c("VANC")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
# NCGA
geom_ribbon(data = subset(predictions.gam4b.fall, State %in% c("NCGA")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.gam4b.fall, State %in% c("NCGA")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
# ylim(0,0.25) +
  # scale_y_break(c(0,0.4), scales = 0.01) + scale_y_break(c(0.05, 0.4), scales = 0.1)
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Fall")



#----- 1B. STATE Graphing Prediction on Biomass v Year (by Survey) --------------------------------------------------------
# Data with State Code for GAM4b
predictions.gam2.state.spring <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam2.state.spring_20230612.csv", header = TRUE)
predictions.gam2.state.fall <- read.csv("/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam2.state.fall_20230612.csv", header = TRUE)
unique(predictions.gam2.state.spring$Survey)
# [1] "CTLISTS"  "NJOT"     "DEBay"    "ChesMMAP" "GAEMTS" 

# # Color palette for North to South States AKA Dark to Light
# pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
# pal6lite <- adjustcolor(pal6, alpha.f =0.5) #make transparent


#----- Spring
ggplot() +
  geom_ribbon(data = predictions.gam2.state.spring, aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = Survey, color = Survey)) +
  geom_line(data = predictions.gam2.state.spring, aes(x=Year, y=fit, group = Survey, color = Survey), linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Biomass (kg/tow)") +
  xlim(2000, 2021) +
  ylim(0, 400) +
  ggtitle("Spring")



#----- Fall
ggplot() +
  geom_ribbon(data = predictions.gam2.state.fall, aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = Survey), color = "grey70") +
  geom_line(data = predictions.gam2.state.fall, aes(x=Year, y=fit, group = Survey, color = Survey), linewidth = 1.2) +
  # geom_textline(aes(label = Survey), size = 6, linewidth = 2, hjust = 1) + #with geomtextpath()
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Biomass (kg/tow)") +
  xlim(2000, 2021) +
  ylim(0, 200) +
  ggtitle("Fall")




#----- 2. Graphing Prediction on Biomass v Year (by Strata) --------------------------------------------------------

library(ggbreak)
library(gt) #adjusting color luminance

# Color palette for North to South Regions AKA Dark to Light
pal <- c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725")
pal2 <- adjustcolor(pal, alpha.f =0.2) #make transparent


#----- Spring
ggplot() +
  #DATA POINTS
  # #cc
  # geom_point(data = subset(surveydata.spring, Stratum %in% ccstrata.spring), aes(x=Year, y=Biomass), color = pal[1]) +
  # #nyli 
  # geom_point(data = subset(surveydata.spring, Stratum %in% nylistrata.spring), aes(x=Year, y=Biomass), color = pal[2]) +
  # #de
    # geom_point(data = subset(surveydata.spring, Stratum %in% destrata.spring), aes(x=Year, y=Biomass), color = pal[3]) +
  #va
  # geom_point(data = subset(surveydata.spring, Stratum %in% vastrata.spring), aes(x=Year, y=Biomass), color = pal[4]) +
  # #nc
  # geom_point(data = subset(surveydata.spring, Stratum %in% ncstrata.spring), aes(x=Year, y=Biomass), color = pal[5]) +
  #FIT LINES
  # #cc
  # geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% ccstrata.spring), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[1]) +
  # geom_line(data = subset(predictions.gam2.spring, Stratum %in% ccstrata.spring), aes(x=Year, y=fit, group = Stratum), color = pal[1]) +
  # #nyli
  geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% nylistrata.spring), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[2]) +
  geom_line(data = subset(predictions.gam2.spring, Stratum %in% nylistrata.spring), aes(x=Year, y=fit, group = Stratum), color = pal[2]) +
  # #de
  # geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% destrata.spring), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[3]) +
  # geom_line(data = subset(predictions.gam2.spring, Stratum %in% destrata.spring), aes(x=Year, y=fit, group = Stratum), color = pal[3]) +
  #va
  geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% vastrata.spring), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[4]) +
  geom_line(data = subset(predictions.gam2.spring, Stratum %in% vastrata.spring), aes(x=Year, y=fit, group = Stratum), color = pal[4]) +
  # #nc
  # geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% ncstrata.spring), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[5]) +
  # geom_line(data = subset(predictions.gam2.spring, Stratum %in% ncstrata.spring), aes(x=Year, y=fit, group = Stratum), color = pal[5]) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Spring")


#----- Fall
ggplot() +
  #DATA POINTS
  # #cc
  # geom_point(data = subset(surveydata.fall, Stratum %in% ccstrata.fall), aes(x=Year, y=Biomass), color = pal[1]) +
  #nyli 
  geom_point(data = subset(surveydata.fall, Stratum %in% nylistrata.fall), aes(x=Year, y=Biomass), color = pal[2]) +
  # #de
    # geom_point(data = subset(surveydata.fall, Stratum %in% destrata.fall), aes(x=Year, y=Biomass), color = pal[3]) +
  #va
  geom_point(data = subset(surveydata.fall, Stratum %in% vastrata.fall), aes(x=Year, y=Biomass), color = pal[4]) +
  # #nc
  # geom_point(data = subset(surveydata.fall, Stratum %in% ncstrata.fall), aes(x=Year, y=Biomass), color = pal[5]) +
  #FIT LINES
  # #cc
  # geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% ccstrata.fall), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[1]) +
  # geom_line(data = subset(predictions.gam2.fall, Stratum %in% ccstrata.fall), aes(x=Year, y=fit, group = Stratum), color = pal[1]) +
  #nyli
  geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% nylistrata.fall), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[2]) +
  geom_line(data = subset(predictions.gam2.fall, Stratum %in% nylistrata.fall), aes(x=Year, y=fit, group = Stratum), color = pal[2]) +  
  # #de
  # geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% destrata.fall), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[3]) +
  # geom_line(data = subset(predictions.gam2.fall, Stratum %in% destrata.fall), aes(x=Year, y=fit, group = Stratum), color = pal[3]) +
  #va
  geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% vastrata.fall), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[4]) +
  geom_line(data = subset(predictions.gam2.fall, Stratum %in% vastrata.fall), aes(x=Year, y=fit, group = Stratum), color = pal[4]) +
  # #nc
  # geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% ncstrata.fall), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[5]) +
  # geom_line(data = subset(predictions.gam2.fall, Stratum %in% ncstrata.fall), aes(x=Year, y=fit, group = Stratum), color = pal[5]) +
  ylim(0, 1) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Fall")
  
  
#----- Predicted Fitted Means for Strata with Highest Biomass
# Get strata from
predictedmeans.gam2.spring
#12(NY), 11(NY), 24(DE), 26(DE), 63(NC)
predictedmeans.gam2.fall
# 26(DE), 24(DE), BIN(NY), 12(NY), 2(NY)

ggplot() +
  #DATA POINTS
  geom_point(data = subset(surveydata.fall, Stratum %in% c("24", "26")), aes(x=Year, y=Biomass), color = pal[3]) +  
  geom_point(data = subset(surveydata.fall, Stratum %in% c("BIN", "2", "12")), aes(x=Year, y=Biomass), color = pal[4]) +
  #FIT LINES
  #nyli
  geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% c("BIN", "2", "12")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[2]) +
  geom_line(data = subset(predictions.gam2.fall, Stratum %in% c("BIN", "2", "12")), aes(x=Year, y=fit, group = Stratum), color = pal[2]) +  
  #de
  geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% c("24", "26")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[3]) +
  geom_line(data = subset(predictions.gam2.fall, Stratum %in% c("24", "26")), aes(x=Year, y=fit, group = Stratum), color = pal[3]) +
  ylim(0, 10) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Predicted Fitted Means for Strata with Highest Biomass")



#----- Single Strata
# Created graphs for #26, 65 
par(mfrow = c(2,2))
ggplot() +
  #DATA POINTS
  geom_point(data = subset(surveydata.spring, Stratum %in% c("65")), aes(x=Year, y=Biomass), color = "#7AD151FF") +    
  #FIT LINES
  geom_ribbon(data = subset(predictions.gam2.spring, Stratum %in% c("65")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[4]) +
  geom_line(data = subset(predictions.gam2.spring, Stratum %in% c("65")), aes(x=Year, y=fit, group = Stratum), color = "#7AD151FF") +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Predicted Fitted Means for Strata 65 (VA) Spring")
ggplot() +
  #DATA POINTS
  geom_point(data = subset(surveydata.fall, Stratum %in% c("65")), aes(x=Year, y=Biomass), color = "#414487FF") + 
  #FIT LINES
  geom_ribbon(data = subset(predictions.gam2.fall, Stratum %in% c("65")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = Stratum), fill = pal2[4]) +
  geom_line(data = subset(predictions.gam2.fall, Stratum %in% c("65")), aes(x=Year, y=fit, group = Stratum), color = "414487FF") +  
  theme_classic() +
  theme(legend.position = "none") +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Predicted Fitted Means for Strata 65 (VA) Fall")
  
  

# # Map >.05 sig strata 
# library(sf)
# 
# strata <- st_read("/Users/janellemorano/DATA/NEFSC strata/BTS_Strata.shp")
# 
# # Grab SPRING strata names
# print(sig.coef.names.list$all.spring)
# strata.sig <- strata %>%
#   filter(STRATA %in% c(14, 3350))
# ggplot() +
#   geom_sf(data = strata) +
#   geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("1972-2021, Spring")
# 
# # Grab Fall strata names
# print(sig.coef.names.list$all.fall)
# strata.sig <- strata %>%
#   filter(STRATA %in% c(10,2,3,3080,3110,3230,3260,4,6,7,9))
# ggplot() +
#   geom_sf(data = strata) +
#   geom_sf(data = strata.sig, aes(fill = "red", colour = "red")) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("1972-2021, Fall")
# 


