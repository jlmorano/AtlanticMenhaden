# Visualizing-Final-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Visualizations (graphs and maps) from the models run in "Final-Models-Atlantic-menhaden-ms.R" for the Atlantic menhaden manuscript

# last updated 26 July 2023
###############################################
###############################################

###########################################################################################
#-Table of Contents ------------------------------------------------------------
#----- Data Prep (federal and state surveys)
#       (Line 26)
#----- Presence-Absence GAM: Presence ~ s(Year, by = State) + s(Depth) + s(Bottemp) + State 
#       (Line 71)
#----- Biomass GAM: Biomass ~ s(Year, by = State) + s(Depth) + s(Bottemp) + State
#----- sdmTMB: 
###########################################################################################


library(tidyverse)
library(mgcv)


###########################################################################################
#----- Load Model Results ------------------------------------------------------------
###########################################################################################

# Survey Data
data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/data.list.rds")
# PA-GAM model output
pa.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-results.rds")
# PA-GAM model output summaries only
pa.gam.summaries <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-summaries.rds")
# Prediction data (for PA or Biomass GAM)
preddata.pa.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-preddata.rds")
# Predictions from PA GAM
predictions.pa.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-predictions.rds")
# Biomass GAM model output
bgam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-results.rds")
# Biomass GAM model output summaries only
bgam.summaries <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-summaries.rds")
# Biomasss GAM model predictions
predictions.bgam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-predictions.rds")


###########################################################################################
#----- Visualize Data------------------------------------------------------------
###########################################################################################

# Color palette for North to South States AKA Dark to Light
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent


#----- Federal Spring
ggplot() + 
  geom_point(data = subset(data.list[[1]], State %in% c("MA")), aes(x=Year, y=Presence, group = State), color = pal6[1]) +
  geom_point(data = subset(data.list[[1]], State %in% c("RICTNY")), aes(x=Year, y= Presence, group = State), color = pal6[2]) +
  geom_point(data = subset(data.list[[1]], State %in% c("NJ")), aes(x=Year, y= Presence, group = State), color = pal6[3]) +
  geom_point(data = subset(data.list[[1]], State %in% c("DEMD")), aes(x=Year, y= Presence, group = State), color = pal6[4]) +
  geom_point(data = subset(data.list[[1]], State %in% c("VA")), aes(x=Year, y= Presence, group = State), color = pal6[5]) +
  geom_point(data = subset(data.list[[1]], State %in% c("NC")), aes(x=Year, y= Presence, group = State), color = pal6[6]) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring")

ggplot() + 
  geom_point(data = subset(data.list[[1]], State %in% c("MA")), aes(x=Year, y=Biomass, group = State), color = pal6[1]) +
  geom_point(data = subset(data.list[[1]], State %in% c("RICTNY")), aes(x=Year, y= Biomass, group = State), color = pal6[2]) +
  geom_point(data = subset(data.list[[1]], State %in% c("NJ")), aes(x=Year, y= Biomass, group = State), color = pal6[3]) +
  geom_point(data = subset(data.list[[1]], State %in% c("DEMD")), aes(x=Year, y= Biomass, group = State), color = pal6[4]) +
  geom_point(data = subset(data.list[[1]], State %in% c("VA")), aes(x=Year, y= Biomass, group = State), color = pal6[5]) +
  geom_point(data = subset(data.list[[1]], State %in% c("NC")), aes(x=Year, y= Biomass, group = State), color = pal6[6]) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Biomass (kg/tow)") +
  ggtitle("Spring")


###########################################################################################
#----- Visualize predictions: PA GAM------------------------------------------------------------
###########################################################################################

# Color palette for North to South States AKA Dark to Light
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent


#----- Federal Spring
ggplot() + 
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.pa.gam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  ylim(c(0,0.25)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/PA-GAM-federal-spring.png", width=5.5, height = 6)

#----- Federal Fall
ggplot() +  
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.pa.gam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +  
  ylim(c(0,0.25)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Fall")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/PA-GAM-federal-fall.png", width=5.5, height = 6)

#----- State Spring
ggplot() + 
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[3]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.pa.gam.list[[3]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  ylim(c(0,1)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring (State)")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/PA-GAM-state-spring.png", width=5.5, height = 6)

#----- State Fall
ggplot() +  
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pa.gam.list[[4]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.pa.gam.list[[4]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +  
  ylim(c(0,1)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Fall (State)")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/PA-GAM-state-fall.png", width=5.5, height = 6)




###########################################################################################
#----- Visualize predictions: Biomass GAM------------------------------------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent
library(ggbreak)

#----- Federal Spring
ggplot() + 
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  ylim(c(0, 150)) +
  scale_y_break(c(2.5, 10), scales=c(0.5,40), space = 0.7) + 
  labs(x= " ", y = " ") #Biomass (kg/tow)
  #ggtitle("Spring (Biomass GAM)")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/B-GAM-federal-spring.png", width=6, height = 6) 

#----- Federal Fall
ggplot() +  
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +  
  ylim(c(0,0.25)) +
  labs(x= " ", y = "Biomass (kg/tow)") 
  # ggtitle("Fall")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/B-GAM-federal-fall.png", width=6, height = 6.5)

#----- State Spring
ggplot() + 
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  ylim(c(0,800)) +
  scale_y_break(c(10, 60, 800, 799), scales=c(1,50,50, .01)) + #This creates a weird break and a double axis but it's the best I can do and can be cropped
  labs(x= " ", y = "Biomass (kg/tow)") 
  #ggtitle("Spring (State)")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/B-GAM-state-spring.png", width=6, height = 9)

#----- State Fall
ggplot() +  
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[4]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[4]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +  
  ylim(c(0,200)) +
  labs(x= " ", y = "Biomass (kg/tow)") 
  #ggtitle("Fall (State)")
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/figures/B-GAM-state-fall.png", width=6, height = 6.5)




