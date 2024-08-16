# MSmodel-sdmTMB-Atlantic-menhaden-Biomass.R
######################################
# Janelle L. Morano

# Objectives:
# Spatio-temporal model for menhaden biomass in sdmTMB
# This is run on the VirtualPC (RedCloud)

# last updated 15 August 2024
##### NEED TO UPDATE R and SDMTMB on VPC

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
packageVersion('sdmTMB') #‘0.6.0’ #Need to re-run models; Previously Updated 9 Feb 2024
library(tidyverse)
library(janitor)
library(tictoc)
library(sf)

sessionInfo()
#R version 4.3.2 (2023-10-31 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows Server 2022 x64 (build 20348)




#----- Data Prep ------------------------------------------------------------

# Full dataset
menhaden <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240617.csv", header = TRUE)
menhaden <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240617.csv", header = TRUE)

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
# Suggests UTM zone 19N; CRS = 32619, but 
# choosing to use UTM zone 18N; CRS = 32618.
menhaden <- sdmTMB::add_utm_columns(menhaden, ll_crs = 32618, utm_crs = 32618, c("longitude", "latitude"))
# Move X, Y columns because there was a bug with cog(), but now keeping for cleanliness
colord <- c( "X", "Y", "survey", "vessel", "cruise", "station", "stratum", "inoffshore", "state", "year", "season", "latitude", "longitude", "areasw", "depth", "surftemp", "surfsalin", "bottemp", "botsalin", "abundance", "biomass", "presence", "centroidLat", "centroidLon", "latCat", "lonCat", "avechlor") 
menhaden <- menhaden[, colord]

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(season == "SPRING") %>%
  filter(year >=1972)

menhaden.fall <- menhaden %>%
  filter(season == "FALL") %>%
  filter(year >=1972)

# Verify map
# Create basemap
world <- ne_countries(scale = "medium", returnclass = "sf") 
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")  
state_prov <- rnaturalearth::ne_states(c("united states of america", "canada", returnclass = "sf"))

# Crop above to US east coast
world <- sf::st_crop(world, c(xmin = -83, xmax = -62, ymin = 28, ymax = 46))
us <- sf::st_crop(us, c(xmin = -83, xmax = -62, ymin = 28, ymax = 46))
canada <- sf::st_crop(canada, c(xmin = -83, xmax = -62, ymin = 28, ymax = 46))

# Map coastline
ggplot() +  
  geom_sf(data = us, color = "gray60", fill = "gray95") + #CCCC99
  geom_sf(data = canada, color = "gray60", fill = "gray95") 

# transform into UTM zone 18N; CRS = 32618
useastcoast <- sf::st_transform(us, crs = 32618)
canadaeastcoast <- sf::st_transform(canada, crs = 32618)
ggplot() +
  geom_sf(data = useastcoast, color = "gray60", fill = "gray95") +
  geom_sf(data = canadaeastcoast, color = "gray60", fill = "gray95") +
  geom_point(data = menhaden, aes(longitude, latitude))
# I don't know why this isn't co-mapping well but I'm moving on because this shouldn't affect the calculations.


#----- Make the mesh  ------------------------------------------------------------

# SPRING
mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("X", "Y"), n_knots = 150, type = "cutoff_search") #500
#pdf(file="D:/MODEL_OUTPUT/mesh150.spring.png") #, width=600, height=350)
plot(mesh.spring)
#dev.off()
# mesh.spring$mesh$n # extract number of vertices/knots

# FALL
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("X", "Y"), n_knots = 150, type = "cutoff_search")
plot(mesh.fall)
#png(file="D:/MODEL_OUTPUT/mesh300.fall.png") #, width=600, height=350)
#dev.off()



#----- Read in Extrapolation Grid  ------------------------------------------------------------

# Grid has bathymetry and bottom temp data for each X,Y
# Read in SPRING grid by years
#nd.grid.yrs.spring <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP.rds")
nd.grid.yrs.spring <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP-2.rds")

#nd.grid.yrs.fall <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP.rds")
nd.grid.yrs.fall <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP-2.rds")

# Change column names to match lowercase of dataset now
nd.grid.yrs.spring <- nd.grid.yrs.spring %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")
nd.grid.yrs.fall <- nd.grid.yrs.fall %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")


#----- Model fit  ------------------------------------------------------------


#----- Spring ------------------------------------------------
tic()
fit.sp <- sdmTMB(
  biomass ~ s(bottemp),# try with + (1| vessel)
  family = tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "year",
  spatial = "on", 
  spatiotemporal = "AR1"
)
a <- toc()

#764.05 sec elapsed


# Save and read back in
saveRDS(fit.sp, file = "D:/MODEL_OUTPUT/twar.fit.spring.rds" )
fit.sp <- readRDS("D:/MODEL_OUTPUT/twar.fit.spring.rds")

sanity(fit.sp)
fit.sp
tidy(fit.sp, effects = "ran_pars", conf.int = TRUE)


# QQ plot
menhaden.spring$resids <- residuals(fit.sp) # randomized quantile residuals
qqnorm(menhaden.spring$resids) #, ylim=c(-5,5))
qqline(menhaden.spring$resids)

# Residuals
dev.new(width = 8, height =4, unit ="in")
ggplot(subset(menhaden.spring, year %in% c(1980, 1990, 2000, 2010, 2020)), aes(longitude, latitude, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Spring Residuals") +
  theme_classic()


#----- Make predictions
tic()
p.sp <- predict(fit.sp, newdata = nd.grid.yrs.spring, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
b <- toc()
#681.76 sec elapsed

# Save and then move it to DATA storage on Virtual PC
saveRDS(p.sp, file = "D:/MODEL_OUTPUT/twar.predictions.spring.rds" )

# Read in
p.sp <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.spring.rds")



#----- Uncertainty on spatial predictions
# Create newdata from predictions
uncertdata.p.sp <- p.sp$data %>%
  select(longitude:epsilon_st)

tic()
unc.sp <- predict(fit.sp, newdata = uncertdata.p.sp, nsim = 100)
e <- toc()
#793.94 sec elapsed
uncertdata.p.sp$se <- apply(unc.sp, 1, sd)
saveRDS(uncertdata.p.sp, file="D:/MODEL_OUTPUT/predictions-uncertainty-spring-data.rds")


ggplot() +
  geom_point(data = uncertdata.p.sp, 
             aes(longitude, latitude, color = se)) +
  scale_color_viridis_c(option ="magma",
                        name = "S.E.") +
  theme_classic() +
  ggtitle("Uncertainty of Spatial Predictions")



#----- Center of gravity
tic()
cog <- get_cog(p.sp, format = "wide")
c <- toc()
#17174.78 sec elapsed
write.csv(cog, file="D:/MODEL_OUTPUT/cog-spring-data.csv")

ggplot(cog, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()




#----- Area-weighted standardization population index
# Need to do the prediction with return_tmb_object = TRUE
tic()
index <- get_index(p.sp)
d <- toc()
#9368.27 sec elapsed

write.csv(index, file="D:/MODEL_OUTPUT/index-spring-data.csv")

ggplot(index, aes(year, est)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
	geom_line(lwd = 1, colour = "grey30") +
	labs(x = "year", y = "Biomass (kg)") +
	theme_classic()






################
#----- Fall ------------------------------------------------
tic()
fit.fa <- sdmTMB(
  biomass ~ s(bottemp),# 
  family = tweedie(link = "log"),
  data = menhaden.fall,
  mesh = mesh.fall,
  time = "year",
  spatial = "on", 
  spatiotemporal = "AR1"
)
c <- toc()
#684.13 sec elapsed sec elapsed

# bottemp & depth doesn't converge
# depth doesn't converge

sanity(fit.fa)

# Save and then move it to DATA storage
#saveRDS(fit.fa, file = "D:/MODEL_OUTPUT/twar.fit.fall.rds" )
fit.fa <- readRDS("D:/MODEL_OUTPUT/twar.fit.fall.rds")

fit.fa
tidy(fit.fa, effects = "ran_pars", conf.int = TRUE)
summary(fit.fa$sd_report, select =("fixed"))

# QQ plot
menhaden.fall$resids <- residuals(fit.fa) # randomized quantile residuals
qqnorm(menhaden.fall$resids) #, ylim=c(-5,5))
qqline(menhaden.fall$resids)

# Residuals
dev.new(width = 8, height = 4, unit ="in")
ggplot(subset(menhaden.fall, year %in% c(1980, 1990, 2000, 2010, 2020)), aes(longitude, latitude, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Fall Residuals") +
  theme_bw()


#----- Make predictions
tic()
p.fa <- predict(fit.fa, newdata = nd.grid.yrs.fall, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
d <- toc()
#835.4 sec elapsed

# Save and then move it to DATA storage on Virtual PC
saveRDS(p.fa, file = "D:/MODEL_OUTPUT/twar.predictions.fall.rds" )

# Read in
p.fa <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.fall.rds")




#----- Area-weighted standardization population index
# Need to do the prediction with return_tmb_object = TRUE
tic()
index <- get_index(p.fa)
toc()
#9536.53 sec elapsed
write.csv(index, file="D:/MODEL_OUTPUT/index-fall-data.csv")

ggplot(index, aes(year, est)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
	geom_line(lwd = 1, colour = "grey30") +
	labs(x = "year", y = "Biomass (kg)") +
	theme_classic()




#----- Center of gravity
tic()
cog.fa <- get_cog(p.fa, format = "wide")
toc()
#47695.46 sec elapsed
write.csv(cog.fa, file="D:/MODEL_OUTPUT/cog-fall-data.csv")

ggplot(cog.fa, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()
