# sdmTMB-Atlantic-menhaden-ms-figures.R
######################################
# Janelle L. Morano

# Objectives:
# Use predictions already generated to visualize
# results for use in manuscript


# last updated 27 August 2024
###############################################
###############################################

# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
#used (Mb) gc trigger (Mb) max used (Mb)
#Ncells 283597 15.2     660462 35.3   459180 24.6
#Vcells 569026  4.4    8388608 64.0  1785649 13.7
# ACTION: Restart R now


library(sdmTMB)
packageVersion('sdmTMB') #‘0.6.0'
library(tidyverse)
library(janitor)
library(tictoc)
library(ggplot2)
library(sf)
library(viridisLite)

sessionInfo()
#R version 4.4.1 (2024-06-14 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows Server 2022 x64 (build 20348)


#----- Read in Data & Prep ------------------------------------------------------------
# Full dataset
menhaden <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240819.csv", header = TRUE)
# menhaden <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240819.csv", header = TRUE)

# Remove NAs and amend column headings to lower case to avoid problems (old bug, but keeping for ease)
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.))) %>%
  janitor::clean_names(case = c("lower_camel")) %>%
  # Add vessel column
  mutate(vessel = case_when(survey == "NEAMAP" ~ "NEAMAP",
                            survey == "NEFSC" & year < 2009 ~ "Albatross",
                            survey == "NEFSC" & year >= 2009 ~ "Bigelow"))

# Make "state" and "vessel" a factor
menhaden$state <- as.factor(menhaden$state)
menhaden$vessel <- as.factor(menhaden$vessel)

# add UTM
menhaden <- menhaden[-1]
get_crs(menhaden, c("longitude", "latitude"))
# Suggests UTM zone 19N; CRS = 32619
menhaden <- sdmTMB::add_utm_columns(menhaden, c("longitude", "latitude"))
# Move X, Y columns because there was a bug with cog(), but now keeping for cleanliness
colord <- c( "X", "Y", "survey", "vessel", "cruise", "station", "stratum", "inoffshore", "state", "year", "season", "latitude", "longitude", "areasw", "depth", "surftemp", "surfsalin", "bottemp", "botsalin", "abundance", "biomass", "presence", "centroidLat", "centroidLon") 
menhaden <- menhaden[, colord]

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(season == "SPRING") %>%
  filter(year >=1972)

menhaden.fall <- menhaden %>%
  filter(season == "FALL") %>%
  filter(year >=1972)



#----- Make the mesh 

# SPRING
mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("latitude", "longitude"), n_knots = 150, type = "cutoff_search") #500

# mesh.spring$mesh$n # extract number of vertices/knots

# FALL
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("latitude", "longitude"), n_knots = 150, type = "cutoff_search")

par(mfrow = c(1, 2))
plot(mesh.spring)
title(main = "Spring", cex.main = 1.75)
plot(mesh.fall)
title(main = "Fall", cex.main = 1.75)
par(mfrow = c(1, 1))

dev.new(width = 8, height =4, unit ="in")
png(file="D:/MODEL_OUTPUT/mesh150.spring-fall.png"#, width=600, height=350)


#----- Read in Extrapolation Grid  

# Grid has bathymetry and bottom temp data for each X,Y
nd.grid.yrs.spring <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP.rds")
nd.grid.yrs.fall <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP.rds")

# Change column names to match lowercase of dataset now
nd.grid.yrs.spring <- nd.grid.yrs.spring %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")
nd.grid.yrs.fall <- nd.grid.yrs.fall %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")


#----- Read in Model fit & Predictions  

# Spring
fit.sp <- readRDS("D:/MODEL_OUTPUT/twar.fit.spring.rds")
p.sp <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.spring.rds")
pu.sp <- readRDS(file = "D:/MODEL_OUTPUT/predictions-uncertainty-spring-data.rds")

# Fall
fit.fa <- readRDS("D:/MODEL_OUTPUT/twar.fit.fall.rds")
p.fa <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.fall.rds")
pu.fa <- readRDS(file = "D:/MODEL_OUTPUT/predictions-uncertainty-fall-data.rds")


#----- Amend Predictions to Keep Fewer Years for Mapping  

# Spring
# Decades, but with uncertainty
p.sp.d <- pu.sp %>%
  filter(year == 1980 | 
         year == 1990 |
         year == 2000 |
         year == 2010 |
	   year == 2020)

# Decades (w/o uncertainty)
#p.sp.d <- p.sp$data %>%
#  select(longitude:epsilon_st) %>%
#  filter(year == 1980 | 
#         year == 1990 |
#         year == 2000 |
#         year == 2010 |
#	   year == 2020)

# Years (2019-2021, but better 2021-2023)
p.sp.y <- pu.sp %>%
  filter(year == 2021 |
         year == 2022 |
         year == 2023 )


# Fall
# Decades
p.fa.d <- pu.fa %>%
  filter(year == 1980 | 
         year == 1990 |
         year == 2000 |
         year == 2010 |
	   year == 2020)

# Years (2019-2021, but better 2021-2023)
p.fa.y <- pu.fa %>%
  filter(year == 2021 |
         year == 2022 |
         year == 2023 )


#----- Prepare Basemap  
# Create basemap
library(rnaturalearth)
library(rnaturalearthdata)
# Bring in regional layers
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")

# Crop maps to keep less data
us <- st_crop(us, c(xmin = -64, xmax = -81, ymin = 32, ymax = 46))
canada <- st_crop(canada, c(xmin = -64, xmax = -81, ymin = 32, ymax = 46))

#ggplot() +
#  geom_sf(data = us) + 
#  geom_sf(data = canada) +
#  coord_sf (xlim = c(-81,-64), ylim = c (32,46), expand = FALSE )



#----- Predicted Density by Decades Figure  ------------------------------------------------------------

# Convert density to low, med, high

#### Spring
p.sp.d <- p.sp.d %>%
  mutate(densityCat = case_when(exp(est)<1 ~ "VeryLow",
                                exp(est) >=1 & exp(est) <10 ~ "Low",
                                exp(est) >10 & exp(est) <100 ~ "Medium",
					  exp(est) >100 & exp(est) <500 ~ "High",	
					  exp(est) >500 ~ "VeryHigh",	
                                exp(est) == NA ~ NA)) %>%
  mutate(densityCat = factor(densityCat, levels=c("VeryHigh", "High", "Medium", "Low", "VeryLow"))) %>%
  mutate(denUncert = case_when(densityCat == "VeryLow" & se <2.5 ~ "VeryLow1",
                               densityCat == "VeryLow" & se >=2.5 & se < 5 ~ "VeryLow2",
                               densityCat == "VeryLow" & se >=5 & se <7.5 ~ "VeryLow3",
                               densityCat == "VeryLow" & se >=7.5 & se <10 ~ "VeryLow4",
                               densityCat == "VeryLow" & se >=10 ~ "VeryLow5",
                               densityCat == "Low" & se <2.5 ~ "Low1",
                               densityCat == "Low" & se >=2.5 & se < 5  ~ "Low2",
                               densityCat == "Low" & se >=5 & se <7.5 ~ "Low3",
                               densityCat == "Low" & se >=7.5 & se <10 ~ "Low4",
                               densityCat == "Low" & se >=10 ~ "Low5",
                               densityCat == "Medium" & se <2.5 ~ "Medium1",
                               densityCat == "Medium" & se >=2.5 & se < 5  ~ "Medium2",
                               densityCat == "Medium" & se >=5 & se <7.5 ~ "Medium3",
                               densityCat == "Medium" & se >=7.5 & se <10 ~ "Medium4",
                               densityCat == "Medium" & se >=10 ~ "Medium5",
                               densityCat == "High" & se <2.5 ~ "High1",
                               densityCat == "High" & se >=2.5 & se < 5  ~ "High2",
                               densityCat == "High" & se >=5 & se <7.5 ~ "High3",
                               densityCat == "High" & se >=7.5 & se <10 ~ "High4",
                               densityCat == "High" & se >=10 ~ "High5",
                               densityCat == "VeryHigh" & se <2.5 ~ "VeryHigh1",
                               densityCat == "VeryHigh" & se >=2.5 & se < 5  ~ "VeryHigh2",
                               densityCat == "VeryHigh" & se >=5 & se <7.5 ~ "VeryHigh3",
                               densityCat == "VeryHigh" & se >=7.5 & se <10 ~ "VeryHigh4",
                               densityCat == "VeryHigh" & se >=10 ~ "VeryHigh5")) %>%
  mutate(denUncert = factor(denUncert, levels=c("VeryHigh1", "High1", "Medium1", "Low1", "VeryLow1",
                                                "VeryHigh2", "High2", "Medium2", "Low2", "VeryLow2",
                                                "VeryHigh3", "High3", "Medium3", "Low3", "VeryLow3",
                                                "VeryHigh4", "High4", "Medium4", "Low4", "VeryLow4",
                                                "VeryHigh5", "High5", "Medium5", "Low5", "VeryLow5")))
  

unique(p.sp.d$densityCat)
unique(p.sp.d$denUncert)
 [1] VeryLow3  VeryLow4  VeryLow5  VeryLow2  VeryLow1  Low2      Low1     
 [8] Medium1   Low3      High1     VeryHigh1
25 Levels: VeryHigh1 High1 Medium1 Low1 VeryLow1 VeryHigh2 High2 ... VeryLow5


myc1 <- c("#fde725", "#b5de2b", "#6ece58", "#31688e", "grey70") #level 1: very high, high, medium, low, very low 

myc2 <- adjustcolor(myc1, alpha.f = 0.7) #level2
myc2 <- myc2[c(4:5)] #keep only used levels

myc3 <- adjustcolor(myc1, alpha.f = 0.5) #level3
myc3 <- myc3[c(4:5)] #keep only used levels

myc4 <- adjustcolor(myc1, alpha.f = 0.3) #level4
myc4 <- myc4[5] #keep only used levels

myc5 <- adjustcolor(myc1, alpha.f = 0.1) #level5
myc5 <- myc5[5] #keep only used levels

mycols <- c(myc1, myc2, myc3, myc4, myc5)

p.sp.d <- p.sp.d[order(p.sp.d$densityCat, decreasing = TRUE), ]

# Plot predictions WITHOUT uncertainty
fig.spd <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.sp.d, 
             aes(longitude, latitude, color=densityCat), size = 1.25) + 
  scale_color_manual(values = mycols,
                     labels = c("Very High (>500)", "High (100-500)", "Medium (10-100)", "Low (1-10)", "Very Low 1 (<1)")) + 
  labs( color = "Density (kg/km^2)") +
  guides(color = guide_legend(override.aes = list(size = 5) )) +
  theme_classic() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Spring Density Prediction")

# Plot predictions WITH uncertainty
# Getting closer, but essentially the uncertainty doesn't really show up as it's only for the really low densities
library(ggnewscale)
fig.spd2 <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.sp.d, 
             aes(longitude, latitude, color=denUncert), size = 0.75) + 
  scale_color_manual(values = mycols,
                     labels = c("Very High 1 (>500)", "High 1 (100-500)", "Medium 1 (10-100)", "Low 1 (1-10)", "Very Low 1 (<1)", "VH2", "H2", "M2", "L2", "VL2", "VH3", "H3", "M3", "L3", "VL3", "VH4", "H4", "M4", "L4", "VL4", "VH5", "H5", "M5", "L5", "VL5")) + 
  labs( color = "Density (kg/km^2)") +
  guides(color = guide_legend(override.aes = list(size = 5) )) +
  theme_classic() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Spring Density Prediction")


#### Fall
p.fa.d <- p.fa.d %>%
  mutate(densityCat = case_when(exp(est)<1 ~ "VeryLow",
                                exp(est) >=1 & exp(est) <10 ~ "Low",
                                exp(est) >10 & exp(est) <100 ~ "Medium",
					  exp(est) >100 & exp(est) <500 ~ "High",	
					  exp(est) >500 ~ "VeryHigh",	
                                exp(est) == NA ~ NA)) %>%
  mutate(densityCat = factor(densityCat, levels=c("VeryHigh", "High", "Medium", "Low", "VeryLow")))

unique(p.fa.d$densityCat)
#There's no Very High and High in fall, so adjust colors
mycols2 <- c("#6ece58", "#31688e", "grey70") #medium, low, very low 

p.fa.d <- p.fa.d[order(p.fa.d$densityCat, decreasing = TRUE), ]

fig.fad <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.fa.d, 
             aes(longitude, latitude, color=densityCat), size = 1.25) + 
  scale_color_manual(values = mycols2,
                     labels = c("Medium (10-200)", "Low (1-10)", "Very Low (< 1)")) + 
  labs( color = "Density (kg/km^2)") +
  guides(color = guide_legend(override.aes = list(size = 5) )) +
  theme_classic() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Fall Density Prediction")

#### Final Output
dev.new(width = 16, height = 8, unit ="in")
library(gridExtra)
grid.arrange(fig.spd, fig.fad, nrow = 2)



#----- Predicted Density by Select Years Figure  ------------------------------------------------------------

#### Spring
p.sp.y <- p.sp.y %>%
  mutate(densityCat = case_when(exp(est)<1 ~ "VeryLow",
                                exp(est) >=1 & exp(est) <10 ~ "Low",
                                exp(est) >10 & exp(est) <100 ~ "Medium",
					  exp(est) >100 & exp(est) <500 ~ "High",	
					  exp(est) >500 ~ "VeryHigh",	
                                exp(est) == NA ~ NA)) %>%
  mutate(densityCat = factor(densityCat, levels=c("VeryHigh", "High", "Medium", "Low", "VeryLow")))

unique(p.sp.y$densityCat)

# use "mycols" from above

p.sp.y <- p.sp.y[order(p.sp.y$densityCat, decreasing = TRUE), ]

fig.spy <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.sp.y, 
             aes(longitude, latitude, color=densityCat), size = 1.25) + 
  scale_color_manual(values = mycols,
                     labels = c("Very High (>500)", "High (100-500)", "Medium (10-100)", "Low (1-10)", "Very Low (< 1)")) + 
  labs( color = "Density (kg/km^2)") +
  guides(color = guide_legend(override.aes = list(size = 5) )) +
  theme_classic() +
  facet_wrap(~year, ncol = 3) +
  ggtitle("Spring Density Prediction")

#### Fall
p.fa.y <- p.fa.y %>%
  mutate(densityCat = case_when(exp(est)<1 ~ "VeryLow",
                                exp(est) >=1 & exp(est) <10 ~ "Low",
                                exp(est) >10 & exp(est) <100 ~ "Medium",
					  exp(est) >100 & exp(est) <500 ~ "High",	
					  exp(est) >500 ~ "VeryHigh",	
                                exp(est) == NA ~ NA)) %>%
  mutate(densityCat = factor(densityCat, levels=c("VeryHigh", "High", "Medium", "Low", "VeryLow")))

unique(p.fa.y$densityCat)

# use "mycols2" from above

p.fa.y <- p.fa.y[order(p.fa.y$densityCat, decreasing = TRUE), ]

fig.fay <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.fa.y, 
             aes(longitude, latitude, color=densityCat), size = 1.25) + 
  scale_color_manual(values = mycols2,
                     labels = c("Medium (10-100)", "Low (1-10)", "Very Low (< 1)")) + 
  labs( color = "Density (kg/km^2)") +
  guides(color = guide_legend(override.aes = list(size = 5) )) +
  theme_classic() +
  facet_wrap(~year, ncol = 3) +
  ggtitle("Fall Density Prediction")


#### Final Output
dev.new(width = 10, height = 8, unit ="in")
grid.arrange(fig.spy, fig.fay, nrow = 2)




#----- Fixed Effects (Depth & Temp)  ------------------------------------------------------------

#### Spring
fe.sp <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.sp.d, 
             aes(longitude, latitude, color=est_non_rf), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 5) +
  theme_classic() +
  ggtitle("Spring, Fixed effects only")

#### Fall
fe.fa <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.fa.d, 
             aes(longitude, latitude, color=est_non_rf), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 5) +
  theme_classic() +
  ggtitle("Fall, Fixed effects only")

#### Final Output
dev.new(width = 16, height = 8, unit ="in")
grid.arrange(fe.sp, fe.fa, nrow = 2)


#----- Spatial Random Effects (Latent Factors)  ------------------------------------------------------------

#### Spring
se.sp <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = subset(p.sp.y, year == 2020), 
             aes(longitude, latitude, color= omega_s), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  theme_classic() +
  ggtitle("Spring, Spatial random effects only")

#### Fall
se.fa <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = subset(p.fa.y, year == 2020), 
             aes(longitude, latitude, color= omega_s), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  theme_classic() +
  ggtitle("Fall, Spatial random effects only")

#### Final Output
dev.new(width = 4, height = 8, unit ="in")
grid.arrange(se.sp, se.fa, nrow = 2)


#----- Spatio-Temporal Random Effects (Latent Factors)  ------------------------------------------------------------

#### Spring
st.sp <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.sp.d, 
             aes(longitude, latitude, color= epsilon_st), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 5) +
  theme_classic() +
  ggtitle("Spring, Spatio-temporal random effects")

#### Fall
st.fa <- ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p.fa.d, 
             aes(longitude, latitude, color= epsilon_st), size = 1.25) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 5) +
  theme_classic() +
  ggtitle("Fall, Spatio-temporal random effects")

#### Final Output
dev.new(width = 16, height = 8, unit ="in")
grid.arrange(st.sp, st.fa, nrow = 2)

