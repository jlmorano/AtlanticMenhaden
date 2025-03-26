# sdmTMB-Atlantic-menhaden-Biomass-finalmsmodels.R
######################################
# Janelle L. Morano

# Objectives:
# Build Biomass spatio-temporal model in sdmTMB


# last updated 6 February 2024
###############################################
###############################################

### Full dataset
###-------------------------
# This is the final menhaden MS sdmTMB models with the full dataset
###-------------------------

# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
#used (Mb) gc trigger (Mb) max used (Mb)
#Ncells 283597 15.2     660462 35.3   459180 24.6
#Vcells 569026  4.4    8388608 64.0  1785649 13.7
# ACTION: Restart R now


library(sdmTMB)
#packageVersion('sdmTMB') #'0.4.1'
library(tidyverse)
library(ggplot2)
library(sf)
library(tictoc)
library(viridisLite)
library(janitor)

sessionInfo()



#----- Data Prep ------------------------------------------------------------

# Full dataset
menhaden <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)


# Remove NAs and amend column headings to lower case to avoid problems with COG
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.))) %>%
  janitor::clean_names(case = c("lower_camel"))

# Make "state" a factor
menhaden$state <- as.factor(menhaden$state)

# add UTM
menhaden <- menhaden[-1]
get_crs(menhaden, c("longitude", "latitude"))
# Proceeding with UTM zone 18N; CRS = 32618.
menhaden <- add_utm_columns(menhaden, c("longitude", "latitude"))


# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(season == "SPRING") %>%
  filter(year >=1972)

menhaden.fall <- menhaden %>%
  filter(season == "FALL") %>%
  filter(year >=1972)



#----- Make the mesh  ------------------------------------------------------------

# SPRING
mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("latitude", "longitude"), n_knots = 150, type = "cutoff_search") #500
#pdf(file="D:/MODEL_OUTPUT/mesh150.spring.png") #, width=600, height=350)
plot(mesh.spring)
#dev.off()
# mesh.spring$mesh$n # extract number of vertices/knots

# FALL
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("latitude", "longitude"), n_knots = 150, type = "cutoff_search")
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
  biomass ~ s(bottemp),# 
  family = tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "year",
  spatial = "on", 
  spatiotemporal = "AR1"
)
toc()
#800.95 sec elapsed


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


ggplot(subset(menhaden.spring, year %in% c(1980, 1990, 2000, 2010, 2020)), aes(longitude, latitude, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Spring Residuals") +
  theme_bw()


#----- Make predictions
tic()
p.sp <- predict(fit.sp, newdata = nd.grid.yrs.spring, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
toc()
#781 sec elapsed

# Save and then move it to DATA storage on Virtual PC
saveRDS(p.sp, file = "D:/MODEL_OUTPUT/twar.predictions.spring.rds" )

# Read in
p.sp <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.spring.rds")

# Keep select years for creating figures
p <- p.sp$data %>%
  select(longitude:epsilon_st) %>%
  filter(year == 1980 | 
         year == 1990 |
         year == 2000 |
         year >= 2010)
# Save and then move it to DATA storage on Virtual PC
saveRDS(p, file = "D:/MODEL_OUTPUT/st.fit.spring.predictions-selectyears.rds" )
p <- readRDS(file = "D:/MODEL_OUTPUT/st.fit.spring.predictions-selectyears.rds" )



#----- Make figures
# Create basemap
library(rnaturalearth)
library(rnaturalearthdata)
# Bring in regional layers
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")

# Crop maps to keep less data
us <- st_crop(us, c(xmin = -64, xmax = -81, ymin = 32, ymax = 46))
canada <- st_crop(canada, c(xmin = -64, xmax = -81, ymin = 32, ymax = 46))

ggplot() +
  geom_sf(data = us) + 
  geom_sf(data = canada) +
  coord_sf (xlim = c(-81,-64), ylim = c (32,46), expand = FALSE )

# Transform into UTM 18N; CRS = 32618
utm_zone18N <- 32618
us.utm <- sf::st_transform(us, crs = utm_zone18N)
canada.utm <- sf::st_transform(canada, crs = utm_zone18N)
ggplot() + geom_sf(data = us.utm) + geom_sf(data = canada.utm)

p <- filter(p, year == 2021)


# Plot predictions

# Decades, with low, med, high
hist(exp(p$est<0.5))
min(exp(p$est))
p <- p %>%
  mutate(densityCat = case_when(exp(est)<0.5 ~ "Low",
                                exp(est) >=0.5 & exp(est) <2000 ~ "Medium",
                                exp(est) >2000 ~ "High",
                                exp(est) == NA ~ NA))

mycols <- c("#31688e", "35b779", "#fde725") #low, medium, high

ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = subset(p, year %in% c(1970,1980,1990,2000,2010,2020)), 
             aes(longitude, latitude, color=densityCat), size = 0.1) + 
  scale_color_manual(values = mycols) + 
  theme_classic() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Spring Density Prediction")


# All years
#ggplot() +
#  geom_sf(data = us, color = "grey90", fill = "white") + 
#  geom_sf(data = canada, color = "grey90", fill = "white") +
#  geom_point(data = p, 
#             aes(longitude, latitude, color=exp(est))) + 
#  scale_color_viridis_c(trans = "log",
#                        limits = c(0.001, 120000),
#                        na.value = "grey80",
#                        values = c(0, 0.5, 0.8, 1),
#                        breaks =c(10, 100, 1000, 10000, 100000),
#                        begin = 0.2, end =1,
#                        #direction = -1,
#                        name = "Density") +
#  theme_classic() +
#  facet_wrap(~year, ncol = 2) +
#  ggtitle("Spring Density Prediction")


# Predictions with just fixed effects (effect of depth and temp)
ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p, 
             aes(longitude, latitude, color=est_non_rf)) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 3) +
  theme_classic() +
  ggtitle("Prediction (fixed effects only)")


# Spatial random effects (latent factors)
ggplot() +
  geom_point(data = subset(p, year == 2020), 
             aes(longitude, latitude, color= omega_s), size = 3) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  theme_classic() +
  ggtitle("Spatial random effects only")


# Spatiotemporal random effects (latent factors)
ggplot() +
  geom_point(data = p, 
             aes(longitude, latitude, color= epsilon_st)) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 3) +
  theme_classic() +
  ggtitle("Spatiotemporal random effects only")


# Area-weighted standardization population index
# Need to do the prediction with return_tmb_object = TRUE
tic()
index <- get_index(p.sp)
toc()
#didn't get duration, but >8hrs likely
write.csv(index, file="D:/MODEL_OUTPUT/index-spring-data.csv")

ggplot(index, aes(year, est)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
	geom_line(lwd = 1, colour = "grey30") +
	labs(x = "year", y = "Biomass (kg)") +
	theme_classic()



# Center of gravity
tic()
cog <- get_cog(p.sp, format = "wide")
toc()
#48172.47 sec elapsed
write.csv(cog, file="D:/MODEL_OUTPUT/cog-spring-data.csv")

ggplot(cog, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()


# Uncertainty on spatial predictions
# Create newdata from predictions
uncertdata.p.sp <- p.sp$data %>%
  select(longitude:epsilon_st)


tic()
unc.sp.1 <- predict(fit.sp, newdata = menhaden.spring, nsim = 5)
toc()
#793.94 sec elapsed
menhaden.spring$se <- apply(unc.sp.1, 1, sd)

ggplot() +
  geom_point(data = menhaden.spring, 
             aes(longitude, latitude, color = se)) +
  scale_color_viridis_c(option ="magma",
                        name = "S.E.") +
  theme_classic() +
  ggtitle("Uncertainty of Spatial Predictions")

tic()
unc.sp <- predict(fit.sp, newdata = uncertdata.p.sp, nsim = 5)
toc()
#793.94 sec elapsed
uncertdata.p.sp$se <- apply(unc.sp, 1, sd)

ggplot() +
  geom_point(data = uncertdata.p.sp, 
             aes(longitude, latitude, color = se)) +
  scale_color_viridis_c(option ="magma",
                        name = "S.E.") +
  theme_classic() +
  ggtitle("Uncertainty of Spatial Predictions")





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
toc()
#86.67 sec elapsed
#714.65 sec elapsed

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
ggplot(subset(menhaden.fall, year %in% c(1980, 1990, 2000, 2010, 2020)), aes(longitude, latitude, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Fall Residuals") +
  theme_bw()


#----- Make predictions
tic()
p.fa <- predict(fit.fa, newdata = nd.grid.yrs.fall, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
toc()
#943.92 sec elapsed

# Save and then move it to DATA storage on Virtual PC
saveRDS(p.fa, file = "D:/MODEL_OUTPUT/twar.predictions.fall.rds" )

# Read in
p.fa <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.fall.rds")


# Keep select years for creating figures
p2 <- p.fa$data %>%
  select(longitude:epsilon_st) %>%
  filter(year == 1980 | 
         year == 1990 |
         year == 2000 |
         year == 2010 |
	   year == 2020)
# Save and then move it to DATA storage on Virtual PC
saveRDS(p2, file = "D:/MODEL_OUTPUT/st.fit.fall.predictions-selectyears.rds" )
p2 <- readRDS(file = "D:/MODEL_OUTPUT/st.fit.fall.predictions-selectyears.rds" )


#----- Make figures
# Assumes basemap created above
# Plot predictions

# Add Low, Medium, High categories
p2 <- p2 %>%
  mutate(densityCat = case_when(exp(est)<10 ~ "Low",
                                exp(est) >=10 & exp(est) <2000 ~ "Medium",
                                exp(est) >2000 ~ "High"))

mycols <- c("#fde725", "21918c", "#440154")

ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p2, 
             aes(longitude, latitude, color=densityCat), size = 0.1) + 
  scale_color_manual(values = c("grey80", "21918c", "#440154")) +
  theme_classic() +
  facet_wrap(~year, ncol = 5) +
  ggtitle("Fall Density Prediction")


#ggplot() +
#  geom_sf(data = us, color = "grey90", fill = "white") + 
#  geom_sf(data = canada, color = "grey90", fill = "white") +
#  geom_point(data = p2, 
#             aes(longitude, latitude, color=exp(est)), size = 0.1) + 
#  scale_color_viridis_c(trans = "log",
#                        limits = c(0.001, 120000),
#                        na.value = "grey80",
#                        values = c(0, 0.5, 0.8, 1),
#                       begin = 0.2, end =1,
#                        #direction = -1,
#                        name = "Density") +
#  theme_classic() +
#  facet_wrap(~year, ncol = 5) +
#  ggtitle("Fall Density Prediction")


# Predictions with just fixed effects (effect of depth and temp)
ggplot() +
  geom_sf(data = us, color = "grey90", fill = "white") + 
  geom_sf(data = canada, color = "grey90", fill = "white") +
  geom_point(data = p2, 
             aes(longitude, latitude, color=est_non_rf)) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 3) +
  theme_classic() +
  ggtitle("Prediction (fixed effects only)")


# Spatial random effects (latent factors)
ggplot() +
  geom_point(data = subset(p2, year == 2020), 
             aes(longitude, latitude, color= omega_s), size = 3) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  theme_classic() +
  ggtitle("Spatial random effects only")


# Spatiotemporal random effects (latent factors)
ggplot() +
  geom_point(data = p2, 
             aes(longitude, latitude, color= epsilon_st)) + 
  scale_color_viridis_c(option = "viridis",
                        name = "Estimate") +
  facet_wrap(~year, ncol = 3) +
  theme_classic() +
  ggtitle("Spatiotemporal random effects only")


# Area-weighted standardization population index
# Need to do the prediction with return_tmb_object = TRUE
tic()
index <- get_index(p.fa)
toc()
#27147.48 sec elapsed
write.csv(index, file="D:/MODEL_OUTPUT/index-fall-data.csv")

ggplot(index, aes(year, est)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
	geom_line(lwd = 1, colour = "grey30") +
	labs(x = "year", y = "Biomass (kg)") +
	theme_classic()




# Center of gravity
tic()
cog.fa <- get_cog(p.fa, format = "wide")
toc()
#47695.46 sec elapsed
write.csv(cog.fa, file="D:/MODEL_OUTPUT/cog-fall-data.csv")

ggplot(cog.fa, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()
