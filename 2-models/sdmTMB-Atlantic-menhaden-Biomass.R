# sdmTMB-Atlantic-menhaden-Biomass.R
######################################
# Janelle L. Morano

# Objectives:
# Build Biomass spatio-temporal model in sdmTMB


# last updated 11 January 2024
###############################################
###############################################


###-------------------------
# Note that code has been saved as written after variations of models have been tested.
# Notes have been left about what has been tried and the results, but not all model data has been saved
###-------------------------



# Best practice to clean up and then restart R
rm(list = ls(all.names = TRUE)) #will clear all objects including hidden objects.
gc() #free up memory and report the memory usage
# ACTION: Restart R now

# Load sdmTMB and verify that I'm running the correct version of dependencies
# install.packages("sdmTMB", dependencies = TRUE)
library(sdmTMB)

# If any warnings or errors come up, be sure to install or update what is noted
# Conflicts with Matrix will silently kill your models from compiling (i.e. sdmTMB will load but your models will fail)
# If there is a conflict between versions of Matrix, etc. in sdmTMB, you can't load a fit model previously run. This may be related to glmmTMB. 
# If different versions are needed of a package,
# require(devtools)
# install_version("Matrix", version = "1.6.1.1", repos = "http://cran.us.r-project.org")
packageVersion('sdmTMB')
# ‘0.4.1’ on Mac
# '0.4.1' on VirtualPC


library(tidyverse)
library(ggplot2)
library(sf)
library(tictoc)
library(viridisLite)
library(janitor)


sessionInfo()
###### Mac
# R version 4.3.0 (2023-04-21)
# Platform: x86_64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.5.2
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] viridisLite_0.4.2 tictoc_1.2        sf_1.0-13         lubridate_1.9.2  
# [5] forcats_1.0.0     stringr_1.5.0     dplyr_1.1.2       purrr_1.0.1      
# [9] readr_2.1.4       tidyr_1.3.0       tibble_3.2.1      ggplot2_3.4.2    
# [13] tidyverse_2.0.0   sdmTMB_0.4.1     
# 
# loaded via a namespace (and not attached):
#   [1] sandwich_3.0-2     utf8_1.2.3         generics_0.1.3     class_7.3-22      
# [5] KernSmooth_2.23-21 stringi_1.7.12     lattice_0.21-8     hms_1.1.3         
# [9] magrittr_2.0.3     grid_4.3.0         estimability_1.4.1 timechange_0.2.0  
# [13] mvtnorm_1.2-1      Matrix_1.6-1.1     e1071_1.7-13       DBI_1.1.3         
# [17] survival_3.5-5     multcomp_1.4-25    mgcv_1.8-42        fansi_1.0.4       
# [21] scales_1.2.1       TH.data_1.1-2      codetools_0.2-19   cli_3.6.1         
# [25] rlang_1.1.1        units_0.8-2        munsell_0.5.0      splines_4.3.0     
# [29] withr_2.5.0        visreg_2.7.0       tools_4.3.0        tzdb_0.4.0        
# [33] coda_0.19-4        colorspace_2.1-0   assertthat_0.2.1   vctrs_0.6.2       
# [37] R6_2.5.1           proxy_0.4-27       classInt_0.4-9     zoo_1.8-12        
# [41] lifecycle_1.0.3    emmeans_1.8.6      MASS_7.3-60        pkgconfig_2.0.3   
# [45] pillar_1.9.0       gtable_0.3.3       Rcpp_1.0.10        glue_1.6.2        
# [49] tidyselect_1.2.0   rstudioapi_0.14    xtable_1.8-4       nlme_3.1-162      
# [53] TMB_1.9.6          compiler_4.3.0


###### Virtual PC
# R  version 4.3.2 (2023-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server 2022 x64 (build 20348)
# 
# Matrix products: default
# 
# 
# locale:
#   [1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8   
# [3] LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.utf8    
# 
# time zone: America/New_York
# tzcode source: internal
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] viridisLite_0.4.2 tictoc_1.2        sf_1.0-14         ggplot2_3.4.4     tidyverse_2.0.0   sdmTMB_0.4.1     
# 
# loaded via a namespace (and not attached):
#   [1] Matrix_1.6-1.1     gtable_0.3.4       dplyr_1.1.4        compiler_4.3.2     tidyselect_1.2.0  
# [6] Rcpp_1.0.11        assertthat_0.2.1   splines_4.3.2      scales_1.2.1       lattice_0.22-5    
# [11] R6_2.5.1           generics_0.1.3     classInt_0.4-10    tibble_3.2.1       units_0.8-4       
# [16] munsell_0.5.0      DBI_1.1.3          pillar_1.9.0       rlang_1.1.2        utf8_1.2.4        
# [21] cli_3.6.1          withr_2.5.2        magrittr_2.0.3     mgcv_1.9-0         class_7.3-22      
# [26] grid_4.3.2         lifecycle_1.0.4    nlme_3.1-164       vctrs_0.6.4        KernSmooth_2.23-22
# [31] proxy_0.4-27       glue_1.6.2         fansi_1.0.5        e1071_1.7-13       colorspace_2.1-0  
# [36] tools_4.3.2        pkgconfig_2.0.3    visreg_2.7.0



# Set working directory on Virtual Windows Server
# Not clear this is worth it
# setwd("D:/")
# getwd()



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

# ALTERNATIVE, Create smaller set for testing, 2013-2021
menhaden.spring <- menhaden %>%
  filter(season == "SPRING") %>%
  filter(year >=2013 & year <=2021)

menhaden.fall <- menhaden %>%
  filter(season == "FALL") %>%
  filter(year >=2013 & year <=2021) 



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

# ALTERNATIVE, Create smaller set for testing, 2013-2021
nd.grid.yrs.spring <- nd.grid.yrs.spring %>%
  filter(year >=2013 & year <=2021)  
nd.grid.yrs.fall <- nd.grid.yrs.fall %>%
  filter(year >=2013 & year <=2021) 



#----- Model fit  ------------------------------------------------------------
tic()
fit.spring <- sdmTMB(
  1+Biomass ~ s(Bottemp), 
  family = Gamma(link = "log"),  #tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  spatial = "off", 
)
toc()
# 11.87 sec elapsed
sanity(fit.spring)
fit.spring
tidy(fit.spring, effects = "ran_pars", conf.int = TRUE)
hist(log(menhaden.spring$Depth))




#----- Spatiotemporal Model  ----------

#----- Spring
#Test dataset won't converge with Biomass ~ s(Bottemp)+s(Depth) & Tweedie, IID or AR1
#Test dataset won't converge with Biomass ~ s(Bottemp) & Tweedie, IID or AR1
#Test dataset won't converge with Biomass ~ s(Depth) & Tweedie, IID or AR1
#Test dataset converges, but problems with ln_tau_O and sigma_O with 1+Biomass ~ s(Bottemp)+s(Depth) & Gamma, IID
#Test dataset converges, but problems with ln_tau_O and sigma_O with 1+Biomass ~ s(Depth) & Gamma, IID


tic()
st.fit.spring <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth), 
  family = tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data` #Turn off if not using spatiotemporal
  spatial = "on", 
  spatiotemporal = "IID"   #"AR1" is slower than IID
)
toc()

#Test dataset: 41.5 with 1 + Biomass ~ s(Bottemp), Gamma and AR1
#Test dataset: 13.31 with 1 + Biomass ~ s(Bottemp), Gamma and IID
#Test dataset: 12.86 with 1 + Biomass ~ s(Bottemp), Gamma and NO spatiotemporal


#Full dataset: 3483.81 sec elapsed with AR1 and spatial off
#Full dataset: 339.49 sec elapsed    with IID and spatia,l on


## Basic sanity checks on model
sanity(st.fit.spring)
#✔ Non-linear minimizer suggests successful convergence
#✔ Hessian matrix is positive definite
#✔ No extreme or very small eigenvalues detected
#✔ No gradients with respect to fixed effects are >= 0.001
#✔ No fixed-effect standard errors are NA
#✔ No standard errors look unreasonably large
#✔ No sigma parameters are < 0.01
#✔ No sigma parameters are > 100
#✔ Range parameter doesn't look unreasonably large

# Save and then move it to DATA storage
# on Virtual PC
# saveRDS(st.fit.spring, file = "D:/MODEL_OUTPUT/st.fit.spring.iid-spatialoff.rds" )

# Read in
# st.fit.spring <- readRDS("D:/MODEL_OUTPUT/D:/MODEL_OUTPUT/st.fit.spring-spatialon.rds")


## Basic sanity checks on model
# st.fit.spring
tidy(st.fit.spring, conf.int=TRUE)
param <- tidy(st.fit.spring, effects = "ran_pars", conf.int = TRUE)


# Residuals
st.fit.spring$resids <- residuals(st.fit.spring) # randomized quantile residuals
qqnorm(st.fit.spring$resids, ylim=c(-1,1))
qqline(st.fit.spring$resids)
png(file="D:/MODEL_OUTPUT/qqAR1gamma.spring.png") 
dev.off()

ggplot(st.fit.spring, aes(X, Y, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~Year, ncol = 4) +
  ggtitle("Residuals")



#----- Make predictions
tic()
st.p <- predict(st.fit.spring, newdata = nd.grid.yrs.spring, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
toc()
# 196.36 sec elapsed Gamma, IID

#1630.09 sec elapsed AR1
# 1537.18 sec elapsed IID


# Save and then move it to DATA storage on Virtual PC
saveRDS(st.p, file = "D:/MODEL_OUTPUT/st.fit.spring.predictions.rds" )


# Read in
st.p <- readRDS(file = "D:/MODEL_OUTPUT/st.fit.pred-spatiotemporal-predictions.rds")


p <- select(st.p$data, Longitude:epsilon_st) %>%
  as_tibble()
# Save as a csv on VPC
# write.table(p, file = "D:/MODEL_OUTPUT/st.fit.pred-spatiotemporal-predictions-byloc.csv")


#----- Make figures ----------------------------------------

# Predictions with all fixed and random effects
# Subset a few years
psub <- p %>% filter(Year >= 2000) 



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
us <- sf::st_transform(us, crs = utm_zone18N)
canada <- sf::st_transform(canada, crs = utm_zone18N)
ggplot() + geom_sf(data = us) + geom_sf(data = canada)

# Predictions of full model (all fixed and random effects)
png(file="D:/MODEL_OUTPUT/density.spring.png") 

ggplot() +
  #geom_sf(data = ecoastus) + 
  #geom_sf(data = ecoastca) +
  geom_point(data = p, 
             aes(X *1000, Y*1000, color=est)) + #size may need to be 0.5 or 0.25
  #coord_fixed() +
  scale_color_viridis_c(option = "viridis") +
  #scale_fill_viridis_c(trans = "sqrt", 
  #                     na.value = "grey", 
  #                     limits = c(0.8, quantile(exp(psub$est), 0.995))) + # trim extreme high values to make spatial variation more visible
  theme_classic() +
  facet_wrap(~Year, ncol = 4) +
  ggtitle("Density Prediction")

dev.off()   


# Predictions with just fixed effects (effect of depth and temp)
png(file="D:/MODEL_OUTPUT/fixedeffects.spring.png") 
ggplot() +
  geom_point(data = p, 
             aes(X * 1000, Y*1000, color=est_non_rf)) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~Year, ncol = 2) +
  ggtitle("Prediction (fixed effects only)")
dev.off()  

# Spatial random effects (latent factors)
# Save as png, but pdf is high quality
png(file="D:/MODEL_OUTPUT/spatialeffects.spring.png") 
ggplot() +
  geom_point(data = p, 
             aes(X, Y, color= omega_s), size = 0.25) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~Year, ncol = 2) +
  ggtitle("Spatial random effects only")
dev.off()





#----- Fall ------------------------------------------------
tic()
st.fit.fall.iid <- sdmTMB(
  1+biomass ~ s(bottemp),# + s(depth),
  family = Gamma(link = "log"), #tweedie(link = "log"),
  data = menhaden.fall,
  mesh = mesh.fall,
  time = "year", # column in `data`
  spatial = "on", 
  spatiotemporal = "IID" #AR1
)
toc()

tic()
st.fit.fall.twar <- sdmTMB(
  biomass ~ s(bottemp),# + s(depth),
  family = tweedie(link = "log"),
  data = menhaden.fall,
  mesh = mesh.fall,
  time = "year", # column in `data`
  spatial = "on", 
  spatiotemporal = "IID"
)
toc()

#250.2 sec elapsed sec elapsed #with IID
#11.47 sec elapsed for test set with IID
#60.89 sec elapsed for test set with AR1
sanity(st.fit.fall.iid)


# Save and then move it to DATA storage
#saveRDS(st.fit.fall, file = "D:/MODEL_OUTPUT/st.fit.fall-spatialoff.rds" )
#saveRDS(st.fit.fall, file = "D:/MODEL_OUTPUT/st.fit.fall-spatialon.rds" )
#saveRDS(st.fit.fall, file = "D:/MODEL_OUTPUT/st.fit.fall.iid.rds" )
#st.fit.fall.iid <- readRDS("D:/MODEL_OUTPUT/st.fit.fall.iid.rds")

## Basic sanity checks on model
tidy(st.fit.fall.iid)
param <- tidy(st.fit.fall.iid, effects = "ran_pars", conf.int = TRUE)
st.fit.fall.iid

#png(file="D:/MODEL_OUTPUT/qqGamma.fall.png") 
menhaden.fall$resids <- residuals(st.fit.fall.iid) # randomized quantile residuals
qqnorm(menhaden.fall$resids, ylim=c(-1,1))
qqline(menhaden.fall$resids)
#dev.off()

#Residuals
#png(file="D:/MODEL_OUTPUT/residualsGamma.fall.png") 
ggplot(menhaden.fall, aes(X, Y, color = resids)) +
  geom_point() +
  scale_color_gradient2() +
  facet_wrap(~year, ncol = 4) +
  ggtitle("Residuals")
#dev.off()


#----- Make predictions
tic()
st.fit.fall.iid.pred <- predict(st.fit.fall.iid, newdata = nd.grid.yrs.fall, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
toc()
# 207.72 sec elapsed
# 132.88 sec elapsed

#st.fit.fall.iid.pred <- readRDS("D:MODEL_OUTPUT/st.fit.fall.iid.pred.rds")

p <- select(st.fit.fall.iid.pred$data, longitude:epsilon_st) %>%
  as_tibble()

# Save test p
saveRDS(p, file="D:/MODEL_OUTPUT/st.fit.fall.iid.predictionoutput.rds")



#----- Make figures ----------------------------------------

# Predictions with all fixed and random effects
# Subset a few years
psub <- p %>% filter(Year >= 2000) 



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
us <- sf::st_transform(us, crs = utm_zone18N)
canada <- sf::st_transform(canada, crs = utm_zone18N)
ggplot() + geom_sf(data = us) + geom_sf(data = canada)


# Predictions of full model (all fixed and random effects)
#png(file="D:/MODEL_OUTPUT/densityGammaiid.fall-2.png") 
p <- arrange(p, est)
ggplot() +
  #geom_sf(data = ecoastus) + 
  #geom_sf(data = ecoastca) +
  geom_point(data = p, 
             aes(X, Y, color=exp(est)), size = 0.25) + #size may need to be 0.5 or 0.25
  #coord_fixed() +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  scale_fill_viridis_c(trans = "sqrt", 
                       na.value = "grey", 
                       limits = c(0.8, quantile(exp(p$est), 0.995))) + # trim extreme high values to make spatial variation more visible
  theme_classic() +
  facet_wrap(~year, ncol = 4) +
  ggtitle("Density Prediction")
#dev.off()
        

# Predictions with just fixed effects (effect of depth and temp)
#png(file="D:/MODEL_OUTPUT/fixedeffects-Gammaiid.fall-2.png") 
ggplot() +
  geom_point(data = p, 
             aes(X * 1000, Y*1000, color=exp(est_non_rf))) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~year, ncol = 2) +
  ggtitle("Prediction (fixed effects only)")
dev.off()


# Spatial random effects (latent factors)
#png(file="D:/MODEL_OUTPUT/spatialeffects-Gammaiid.fall-2.png") 
ggplot() +
  geom_point(data = p, 
             aes(X, Y, color= omega_s)) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~year, ncol = 2) +
  ggtitle("Spatial random effects only")
#dev.off()


# Spatiotemporal random effects (latent factors)
#png(file="D:/MODEL_OUTPUT/spatiotemporaleffects-Gammaiid.fall-2.png") 
ggplot() +
  geom_point(data = p, 
             aes(X, Y, color= epsilon_st)) + 
  scale_color_viridis_c(option = "viridis") +
  facet_wrap(~year, ncol = 2) +
  ggtitle("Spatiotemporal random effects only")
#dev.off()


# Area-weighted standardization population index
#png(file="D:/MODEL_OUTPUT/index-Gammaiid.fall-2.png") 
index <- get_index(st.fit.fall.iid.pred)
# Need to do the prediction with return_tmb_object = TRUE
ggplot(index, aes(year, est)) +
	geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey90") +
	geom_line(lwd = 1, colour = "grey30") +
	labs(x = "year", y = "Biomass (kg)") +
	theme_classic()
dev.off()


# Center of gravity
#st.fit.fall.iid.pred$data$year <- st.fit.fall.iid.pred$data$Year

tic()
cog <- get_cog(st.fit.fall.iid.pred, format = "wide")
toc()
# 88.53 sec elapsed
write.csv(cog, file="D:/MODEL_OUTPUT/cog-gammaiid.fall.csv")

ggplot(cog, aes(est_x, est_y, colour = year)) +
  geom_pointrange(aes(xmin = lwr_x, xmax = upr_x)) +
  geom_pointrange(aes(ymin = lwr_y, ymax = upr_y)) +
  scale_colour_viridis_c()


# Get uncertainty
# sample from the joint precision matrix
samps <- gather_sims(st.fit.fall.iid, nsim = 1000)
ggplot(samps, aes(.value)) + geom_histogram() +
  facet_wrap(~.variable, scales = "free_x")

# calculate uncertainty on spatial predictions
pu <- predict(st.fit.fall.iid, newdata = predictor_dat, nsim = 500)
predictor_dat$se <- apply(pu, 1, sd)
ggplot(predictor_dat, aes(X, Y, fill = se)) +
  geom_raster() +
  scale_fill_viridis_c(option = "A") +
  coord_cartesian(expand = FALSE)


#----- Delta Spatio-Temporal Model ----------------------
# Model Type        |	Built-in delta function	  | Presence-absence model		| Positive catch model
# Delta-gamma	      | delta_gamma()		          | binomial(link = "logit")	| Gamma(link = "log")
# Delta-lognormal		| delta_lognormal()		      | binomial(link = "logit")	| lognormal(link = "log")
# Delta-NB1	        | delta_truncated_nbinom1()	| binomial(link = "logit")	| truncated_nbinom1(link = "log")
# Delta-NB2	        |delta_truncated_nbinom2()	| binomial(link = "logit")	| truncated_nbinom2(link = "log")

tic()
delta.fit <- sdmTMB(
  biomass ~ 1 + s(bottemp), # + s(depth),
  data = menhaden.fall,
  mesh = mesh.fall,
  time = "year", # column in `data`
  family = delta_gamma()
)
toc()
# 
# 475.06
#Warning messages:
#1: In sqrt(diag(cov)) : NaNs produced
#2: The model may not have converged: non-positive-definite Hessian matrix. 
## Basic sanity checks on model
sanity(delta.fit)
#✔ Non-linear minimizer suggests successful convergence
# ✖ Non-positive-definite Hessian matrix: model may not have converged
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✔ No extreme or very small eigenvalues detected
# ✖ `ln_tau_E` gradient > 0.001
# ℹ See ?run_extra_optimization(), standardize covariates, and/or simplify the model

# ✖ `ln_kappa` gradient > 0.001
# ℹ See ?run_extra_optimization(), standardize covariates, and/or simplify the model

# ✖ `ln_phi` gradient > 0.001
# ℹ See ?run_extra_optimization(), standardize covariates, and/or simplify the model

# ✖ `ln_tau_O` standard error is NA
# ℹ `ln_tau_O` is an internal parameter affecting `sigma_O`
# ℹ `sigma_O` is the spatial standard deviation
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `ln_tau_O` standard error is NA
# ℹ `ln_tau_O` is an internal parameter affecting `sigma_O`
# ℹ `sigma_O` is the spatial standard deviation
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `sigma_O` standard error is NA
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `sigma_O` standard error is NA
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `log_sigma_O` standard error is NA
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `log_sigma_O` standard error is NA
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `ln_smooth_sigma` standard error may be large
# ℹ Try simplifying the model, adjusting the mesh, or adding priors

# ✖ `sigma_O` is smaller than 0.01
# ℹ Consider omitting this part of the model

# ✖ `sigma_E` is larger than 100
# ℹ Consider simplifying the model or adding priors

# ✔ Range parameters don't look unreasonably large
# There were 42 warnings (use warnings() to see them)
