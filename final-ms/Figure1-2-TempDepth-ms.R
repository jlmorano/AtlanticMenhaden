# Figures-ms.R
######################################
# Janelle L. Morano

# Figures for manuscript

# last updated 5 February 2024
###############################################
###############################################

library(tidyverse)
library(ggbreak)
library(viridis)
library(gridExtra)

fed <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)

state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20230727.csv")


#----- Figure 1. Biomass vs Temperature for Spring & Fall, Federal & State -------------


#### A. Federal, BotTemp
# Spring & Fall on same plot
f1a <- ggplot(fed, aes(x=Bottemp, y=log(Biomass+1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)") +
  ggtitle("A. Federal")
# stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)

#### B. State, BotTemp
# Spring & Fall on same plot
f1b <- ggplot(subset(state, Season %in% c("SPRING", "FALL")), aes(x=BotTemp, y=log(Weight.kg+1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)") +
  ggtitle("B. State")

#### C. Federal, Depth
# Spring & Fall on same plot
f1c <- ggplot(fed, aes(x=Depth, y=log(Biomass+1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
  ggtitle("C. Federal")

#### D. State, Depth
f1d <- ggplot(subset(state, Season %in% c("SPRING", "FALL")), aes(x=Depth.m, y=log(Weight.kg+1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
  ggtitle("D. State")

grid.arrange(f1a, f1b, f1c, f1d, nrow = 2)


#----- Figure 2. Depth vs Temperature ------------------------------------------
#### Federal
# Spring & Fall on same plot
f2a <- ggplot(fed, aes(x=Bottemp, y=Depth, color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Bottom Temperature (°C)", y = "Depth (m)") +
  ggtitle("A. Federal") +
  stat_smooth(method = lm, formula = y ~ x)


#### State
f2b <- ggplot(subset(state, Season %in% c("SPRING", "FALL")), aes(x=BotTemp, y=Depth.m, color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x= "Bottom Temperature (°C)", y = "Depth (m)") +
  ggtitle("B. State") +
  stat_smooth(method = lm, formula = y ~ x)

grid.arrange(f2a, f2b, nrow = 1)

#----- 4. NJ & Chesapeake Biomass vs Time ------------------------------------------
ggplot(subset(state, Survey %in% "ChesMMAP"), aes(x=Month, y=log(Weight.kg+1))) +
  # scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
  ggtitle("State")

ggplot(subset(state, Survey %in% "NJOT"), aes(x=Month, y=log(Weight.kg+1))) +
  # scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
  ggtitle("State")
