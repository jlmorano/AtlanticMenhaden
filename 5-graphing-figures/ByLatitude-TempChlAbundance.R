# ByLatitude-TempChlBiomass.R
######################################
# Janelle L. Morano

# Figure of NEFSC, NEAMAP, and state data
# Where...
# - Temperature
# - Chlorophyll
# - Biomass
# Are graphed over Years by latitude along the USCoast

# last updated 27 April 2023
###############################################
###############################################

library(tidyverse)
library(viridis)
library(gridExtra)
library(directlabels)
library(geomtextpath)



############################
#### 1. Map of survey data
############################

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)



#----- Create map of US East Coast ------------------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf")  
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

ggplot(data = world) +  
  geom_sf(data = us, fill = "grey") + #CCCC99
  geom_sf(data = canada, fill = "grey") +
  coord_sf (xlim = c(-82,-62), ylim = c (23,47), expand = FALSE ) + #East coast, Fl to Nova Scotia
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) + ##66CCFF, slategray2
  theme (axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude")



#----- Load Survey shapefiles and survey locations ------------------------------------------

# NEFSC & NEAMAP, full dataset 1963-2021
surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230504.csv", header = TRUE)
# Remove salinity cols and X because not working with now
surveydata <- surveydata %>% select(-c(X, Surfsalin, Botsalin))

#----- Create Spring and Fall datasets
## surveydata
surveydata.spring <- surveydata[surveydata$Season == "SPRING",]
surveydata.fall <- surveydata[surveydata$Season == "FALL",]

#----- Create 1972-2021 dataset
surveydata.spring <- filter(surveydata.spring, Year >=1972)
surveydata.fall <- filter(surveydata.fall, Year >= 1972)

#----- All other State surveys
state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", header = TRUE)


#----- Modify LatCat & LonCat to drop decimals

surveydata.spring <- surveydata.spring %>%
  mutate(across(c("LatCat", "LonCat"), round, 0)) 
surveydata.fall <- surveydata.fall %>%
  mutate(across(c("LatCat", "LonCat"), round, 0)) 

allstate <- state %>%
  mutate(LatCat = Latitude,
         LonCat = Longitude) %>%
  mutate(across(c("LatCat", "LonCat"), round, 0)) 


#----- Create color pallette

pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#fde725")
pal6lite <- adjustcolor(pal6, alpha.f =0.7) #make transparent




#----- 1. Temperature vs Year ------------------------------------------------------

#-----  Federal

# Spring Temp over time
ggplot() +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(35)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[6], color = pal6lite[6], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(38)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[5], color = pal6lite[5], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(40)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[4], color = pal6lite[4], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(42)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[2], color = pal6lite[2], linewidth = 0.4) +
  scale_y_continuous(limits = c(0,30)) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(x= " ", y = "Temperature (°C)")  +
  ggtitle("Spring")
#Save 600x400


# Fall Temp over time
ggplot() +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(35)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[6], color = pal6lite[6], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(38)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[5], color = pal6lite[5], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(40)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[4], color = pal6lite[4], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(42)), aes(x=Year, y=Bottemp, group = Year), fill = pal6[2], color = pal6lite[2], linewidth = 0.4) +
  scale_y_continuous(limits = c(0,30)) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  labs(x= " ", y = "Temperature (°C)")  +
  ggtitle("Fall")
#Save 600x400


#----- Other plotting options to keep for now
# geom_dl(aes(label = Lat), method = list(dl.combine("first.points", "last.points")), cex = 0.8)  #with directlabels()

# https://waldyrious.net/viridis-palette-generator/
# mycolors <- c("#fde725", "#d2e21b", "#a5db36", "#7ad151", "#54c568", "#35b779", "#22a884", "#1f988b", "#23888e", "#2a788e", "#31688e", "#39568c", "#414487", "#472f7d", "#481a6c", "#440154")
# mycolors <- rev(mycolors)

# grid.arrange(gg1, gg2, gg3, gg4, ncol=1)


#-----  State
# Calculate average annual temp by latitude and season
st.temp.sp <- allstate %>%
  filter(Season == "SPRING") %>% 
  group_by(Survey, Year) %>%
  summarise(Avetemp = mean(SurfTemp))
st.temp.fa <- allstate %>%
  filter(Season == "FALL") %>% 
  group_by(Survey, Year) %>%
  summarise(Avetemp = mean(SurfTemp))

# Plot Spring
ggplot(st.temp.sp, aes(x=Year, y=Avetemp, color = Survey)) +
  # scale_color_viridis_c(option = "viridis") +
  # geom_textline(aes(label = Survey), size = 6, linewidth = 2, hjust = 1) +
  # theme(legend.position = "none") +
  geom_line(linewidth = 2) +
  theme_classic() +
  labs(x= " ", y = "Temperature (°C)") +
  ggtitle("Spring")

# Plot Fall
ggplot(st.temp.fa, aes(x=Year, y=Avetemp, color = Survey)) +
  # scale_color_viridis_c(option = "viridis") +
  # geom_textline(aes(label = Survey), size = 6, linewidth = 2, hjust = 1) +
  # theme(legend.position = "none") +
  geom_line(linewidth = 2) +
  theme_classic() +
  labs(x= " ", y = "Temperature (°C)") +
  ggtitle("Fall")




#----- 2. Chlorophyll vs Year ------------------------------------------------------

#----- Federal
# Spring Chlor over time
ggplot() +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(38)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[5], color = pal6lite[5], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(40)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[4], color = pal6lite[4], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(42)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[2], color = pal6lite[2], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.spring, LatCat %in% c(35)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[6], color = pal6lite[6], linewidth = 0.4) +
  theme_classic() +
  scale_x_continuous(limits = c(1970,2021)) +
  scale_y_continuous(limits = c(0,100)) +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(bquote(Chlorophyll (mg/m^3))) +
  ggtitle("Spring")
#Save 600x400


# Fall Chlor over time
ggplot() +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(35)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[6], color = pal6lite[6], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(38)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[5], color = pal6lite[5], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(40)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[4], color = pal6lite[4], linewidth = 0.4) +
  geom_boxplot(data = subset(surveydata.fall, LatCat %in% c(42)), aes(x=Year, y=Avechlor, group = Year), fill = pal6[2], color = pal6lite[2], linewidth = 0.4) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(1970,2021)) +
  scale_y_continuous(limits = c(0,100)) +
  xlab(" ") +
  ylab(bquote(Chlorophyll (mg/m^3))) +
  ggtitle("Fall")
#Save 600x400

# Calculate average annual chlorophyll by latitude and season
fed.chl.sp <- surveydata.spring %>%
  group_by(LatCat, Year) %>%
  summarise(Avechlor = mean(Avechlor))
fed.chl.fa <- surveydata.fall %>%
  group_by(LatCat, Year) %>%
  summarise(Avechlor = mean(Avechlor))

library(ggbreak)
# Plot
ggplot(fed.chl.sp, aes(x=Year, y=Avechlor, color = LatCat, group = LatCat)) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  geom_textline(aes(label = LatCat), size = 6, linewidth = 1.5, hjust = 1) + #with geomtextpath()
  scale_x_break(c(1972, 2012)) + #150-250, 450-3550
  theme_classic() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(bquote(Chlorophyll (mg/m^3))) +
  ggtitle("Spring")
#Save 600x500

ggplot(fed.chl.fa, aes(x=Year, y=Avechlor, color = LatCat, group = LatCat)) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  geom_textline(aes(label = LatCat), size = 6, linewidth = 2, hjust = 1) + #with geomtextpath()
  scale_x_break(c(1972, 2012)) + #150-250, 450-3550
  theme_classic() +
  theme(legend.position = "none") +
  xlab(" ") +
  ylab(bquote(Chlorophyll (mg/m^3))) +
  ggtitle("Fall")
#Save 600x500

