# Figure-2-TempDepth.R
######################################
# Janelle L. Morano

# Figures for manuscript

# last updated 6 August 2024
###############################################
###############################################

library(tidyverse)
library(ggbreak)
library(viridis)
library(gridExtra)

fed <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240617.csv", header = TRUE)

state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240726.csv")



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
  theme(plot.title.position = "plot") +
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
  theme(plot.title.position = "plot") +
  stat_smooth(method = lm, formula = y ~ x)

grid.arrange(f2a, f2b, nrow = 1)
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/Fig2-Depth-v-BottomTemp.png", width=6.5, height = 5.5)


#----- Biomass vs Temperature for Spring & Fall, Federal & State -------------
# ggplot(fed, aes(x=Presence, y=Bottemp, color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   theme_classic() +
#   # scale_y_break(c(100, 3640)) + #150-250, 450-3550
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)") +
#   ggtitle("A. Federal")
# 
# 
# #### A. Federal, BotTemp
# # Spring & Fall on same plot
# f1a <- ggplot(fed, aes(x=Bottemp, y=log(Biomass+1), color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   theme_classic() +
#   # scale_y_break(c(100, 3640)) + #150-250, 450-3550
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)") +
#   ggtitle("A. Federal")
# # stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)
# 
# #### B. State, BotTemp
# # Spring & Fall on same plot
# f1b <- ggplot(subset(state, Season %in% c("SPRING", "FALL")), aes(x=BotTemp, y=log(Weight.kg+1), color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   theme_classic() +
#   # scale_y_break(c(100, 3640)) + #150-250, 450-3550
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)") +
#   ggtitle("B. State")
# 
# #### C. Federal, Depth
# # Spring & Fall on same plot
# f1c <- ggplot(fed, aes(x=Depth, y=log(Biomass+1), color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   theme_classic() +
#   # scale_y_break(c(100, 3640)) + #150-250, 450-3550
#   # scale_y_break(c(150, 250)) +
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
#   ggtitle("C. Federal")
# 
# #### D. State, Depth
# f1d <- ggplot(subset(state, Season %in% c("SPRING", "FALL")), aes(x=Depth.m, y=log(Weight.kg+1), color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   theme_classic() +
#   # scale_y_break(c(100, 3640)) + #150-250, 450-3550
#   # scale_y_break(c(150, 250)) +
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 12)) +
#   labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") +
#   ggtitle("D. State")
# 
# grid.arrange(f1a, f1b, f1c, f1d, nrow = 2)
# 
