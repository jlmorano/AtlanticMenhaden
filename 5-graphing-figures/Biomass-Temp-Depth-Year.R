# Biomass-Temp-Depth-Year.R
######################################
# Janelle L. Morano

# Figures of NMFS/NEAMAP data (primarily Biomass, but could use log(Abundance +1))
# 1. Biomass vs Temperature 
# 2. Biomass vs Depth
# 3. Biomass vs Year
# 4. Biomass vs Latitude

# last updated 11 May 2023
###############################################
###############################################

library(tidyverse)
library(ggbreak)
library(viridis)

#----- Load Survey data ------------------------------------------

# NEFSC & NEAMAP, full dataset 1963-2021
surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230504.csv", header = TRUE)

#----- Create Spring and Fall datasets, 1972-2021 dataset
## surveydata
surveydata.spring <- surveydata[surveydata$Season == "SPRING",]
surveydata.fall <- surveydata[surveydata$Season == "FALL",]

surveydata.spring <- filter(surveydata.spring, Year >=1972)
surveydata.fall <- filter(surveydata.fall, Year >= 1972)

#----- All other State surveys
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", header = TRUE)


#----- 1. Biomass vs Temperature ------------------------------------------

#### Federal
# Spring & Fall on same plot
ggplot(surveydata, aes(x=Bottemp, y=log(Biomass+1), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x= "Bottom Temperature (°C)", y = "log(Biomass+1) (kg/tow)")
  # stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)


#----- 2. Biomass vs Depth ------------------------------------------
#### Federal
# Spring & Fall on same plot
ggplot(surveydata, aes(x=Depth, y=log(Biomass), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  # scale_y_break(c(150, 250)) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x= "Depth (m)", y = "log(Biomass) (kg/tow)") 
  # stat_smooth(method = mgcv::gam, formula = y ~ s(x), size = 2)



# #----- 3. Biomass vs Year --------------------------------------------------------
# 
# #### Federal
# # Spring & Fall on same plot
# ggplot(surveydata, aes(x=Year, y=log(Biomass+1), color = Season)) +
#   scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
#   geom_point() +
#   # scale_y_break(c(100, 600)) +
#   # scale_y_break(c(600, 3500)) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   labs(x= " ", y = "Biomass (kg/tow)") 
#   # stat_smooth(method = mgcv::gam, formula = y ~ s(x), linewidth = 2, color = "red")
# 
# # Spring only
# ggplot(surveydata.spring, aes(x=Year, y=log(Biomass), group = Year)) +
#   geom_boxplot(fill = "#7AD151FF") +
#   scale_fill_viridis(discrete = FALSE, direction = -1) +
#   xlim(1972, 2022) +
#   ylim(-7.5, 7.5) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 14)) +
#   labs(x= " ", y = "log(Biomass) (kg/tow)") +
#   ggtitle("Spring")
# # Fall only
# ggplot(surveydata.fall, aes(x=Year, y=log(Biomass), group = Year)) +
#   geom_boxplot(fill = "#414487FF") +
#   scale_fill_viridis(discrete = FALSE, direction = -1) +
#   xlim(1972, 2022) +
#   ylim(-7.5, 7.5) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   theme(text = element_text(size = 14)) +
#   labs(x= " ", y = "log(Biomass) (kg/tow)") +
#   ggtitle("Fall")


#----- 4. Biomass vs Chlorophyll ------------------------------------------

#### Federal
# Spring & Fall on same plot
ggplot(surveydata, aes(x=Avechlor, y=log(Biomass), color = Season)) +
  scale_colour_manual(values = c("#414487FF","#7AD151FF")) +
  geom_point() +
  theme_classic() +
  # scale_y_break(c(100, 3640)) + #150-250, 450-3550
  theme(legend.position = "none") +
  theme(text = element_text(size = 16)) +
  labs(x = bquote(Chlorophyll (mg/m^3)), y= "log(Biomass) (kg/tow)")


#----- 5. Biomass vs Latitude --------------------------------------------------------

latbio.spring <- surveydata %>%
  mutate(LatCat = round(LatCat, 0)) %>%
  filter(Season == "SPRING")

latbio.fall <- surveydata %>%
  mutate(LatCat = round(LatCat, 0)) %>%
  filter(Season == "FALL")

#### Federal Boxplot
# Spring
ggplot(latbio.spring, aes(x=log(Biomass), y=LatCat, fill = LatCat, group = LatCat)) +
  geom_boxplot() +
  # geom_jitter(color="grey80", size=0.4, alpha=0.9) +
  scale_fill_viridis(discrete = FALSE, direction = -1) +
  ylim(29, 45) +
  xlim(-5, 5) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= "log(Biomass) (kg/tow)", y = " ") +
  ggtitle("Spring")

# Fall
ggplot(latbio.fall, aes(x=log(Biomass), y=LatCat, fill = LatCat, group = LatCat)) +
  geom_boxplot() +
  # geom_jitter(color="grey80", size=0.4, alpha=0.9) +
  scale_fill_viridis(discrete = FALSE, direction = -1) +
  ylim(29, 45) +
  xlim(-5, 5) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= "log(Biomass) (kg/tow)", y = " ") +
  ggtitle("Fall")
