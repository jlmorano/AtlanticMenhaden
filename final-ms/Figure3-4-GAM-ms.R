# Figure3-4-GAM-ms.R
######################################
# Janelle L. Morano

# Visualizations (graphs and maps) from the models run in "Final-Models-Atlantic-menhaden-ms.R" for the Atlantic menhaden manuscript

# last updated 25 July 2024
###############################################
###############################################

###########################################################################################
#-Table of Contents ------------------------------------------------------------
#----- Data Prep (GAM model results of federal and state surveys)
#       (Line 26)
#----- Biomass GAM: Biomass ~ s(Year, by = State) + s(Depth) + s(Bottemp) + State
#       (Line 159)
###########################################################################################


library(tidyverse)
library(mgcv)
library(cowplot)


###########################################################################################
#----- Load Model Results ------------------------------------------------------------
###########################################################################################

# Survey Data
data.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/data.list.rds")
# Prediction data
preddata.bgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/B-GAM-summaries.rds")
# Biomass GAM model output
bgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/B-GAM-results.rds")
# Biomass GAM model output summaries only
bgam.summaries <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/B-GAM-summaries.rds")
# Biomasss GAM model predictions
predictions.bgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/B-GAM-predictions.rds")




###########################################################################################
#----- Visualize predictions: Biomass GAM------------------------------------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.3) #make transparent
library(ggbreak)


#----- Federal Spring
f4a <- ggplot() + 
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  # ylim(c(0,120)) +
  xlim(c(1972, 2021)) +
  scale_y_break(c(1.5, 20), scales=c(0.25,60), space = 0.5) + 
  labs(x= " ", y = "Biomass CPUE (kg)") + #Biomass CPUE (kg)
  ggtitle("Spring (Federal)")


#----- Federal Fall
f4b <- ggplot() +  
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +  
  # ylim(c(0,0.4)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = " ")+
  ggtitle("Fall (Federal)")


#----- State Spring
f4c <- ggplot() + 
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[1]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal6[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[2]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal6[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[3]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal6[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[4]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal6[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[5]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal6[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.bgam.list[[3]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal6lite[6]) +
  geom_line(data = subset(predictions.bgam.list[[3]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal6[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  # ylim(c(0,400)) +
  xlim(c(1972, 2021)) +
  # scale_y_break(c(10, 100), scales=c(0.25,100), space = 1) + 
  labs(x= " ", y = "Biomass CPUE (kg)") +
  ggtitle("Spring (State)")


#----- State Fall
f4d <- ggplot() +  
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
  theme(text = element_text(size = 12)) +  
  # ylim(c(0,200)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = " ") +
  ggtitle("Fall (State)")


plot_grid(f4a, f4b, f4c, f4d, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)



