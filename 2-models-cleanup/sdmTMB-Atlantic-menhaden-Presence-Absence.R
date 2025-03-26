# sdmTMB-Atlantic-menhaden-Presence-Absence.R
######################################
# Janelle L. Morano

# Objectives:
# Build Presence-Absence spatio-temporal model in sdmTMB

# last updated 27 November 2023
###############################################
###############################################

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
#   [1] viridisLite_0.4.2 tictoc_1.2        sf_1.0-13         sdmTMB_0.4.1      lubridate_1.9.2   forcats_1.0.0    
# [7] stringr_1.5.0     dplyr_1.1.2       purrr_1.0.1       readr_2.1.4       tidyr_1.3.0       tibble_3.2.1     
# [13] ggplot2_3.4.2     tidyverse_2.0.0  
# 
# loaded via a namespace (and not attached):
#   [1] sandwich_3.0-2     utf8_1.2.3         generics_0.1.3     class_7.3-22       KernSmooth_2.23-21
# [6] stringi_1.7.12     lattice_0.21-8     hms_1.1.3          magrittr_2.0.3     grid_4.3.0        
# [11] timechange_0.2.0   estimability_1.4.1 mvtnorm_1.2-1      Matrix_1.6-1.1     e1071_1.7-13      
# [16] DBI_1.1.3          survival_3.5-5     multcomp_1.4-25    mgcv_1.8-42        fansi_1.0.4       
# [21] scales_1.2.1       TH.data_1.1-2      codetools_0.2-19   cli_3.6.1          rlang_1.1.1       
# [26] units_0.8-2        munsell_0.5.0      splines_4.3.0      withr_2.5.0        tools_4.3.0       
# [31] tzdb_0.4.0         coda_0.19-4        colorspace_2.1-0   assertthat_0.2.1   vctrs_0.6.2       
# [36] R6_2.5.1           proxy_0.4-27       classInt_0.4-9     zoo_1.8-12         lifecycle_1.0.3   
# [41] emmeans_1.8.6      MASS_7.3-60        pkgconfig_2.0.3    pillar_1.9.0       gtable_0.3.3      
# [46] Rcpp_1.0.10        glue_1.6.2         tidyselect_1.2.0   rstudioapi_0.14    xtable_1.8-4      
# [51] nlme_3.1-162       compiler_4.3.0 


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



# Set working directory on Mac
setwd("/Users/janellemorano")
# Set working directory on Cloud Server PC
setwd("D/:")




#----- Data Prep ------------------------------------------------------------


#----- Read in Full dataset
# SKIP if using Test dataset
menhaden <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)
# For Virtual Windows Server
menhaden <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20230724.csv", header = TRUE)

# Remove NAs
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.)))

# Make "State" a factor
menhaden$State <- as.factor(menhaden$State)

# add UTM
menhaden <- menhaden[-1]
get_crs(menhaden, c("Longitude", "Latitude"))
menhaden <- add_utm_columns(menhaden, c("Longitude", "Latitude"))
# convert UTM from meters (default) to km
# menhaden$X <- menhaden$X/1000
# menhaden$Y <- menhaden$Y/1000

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972)
menhaden.fall <- menhaden %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)

# # Create smaller set for testing, 2010-2020
# test.spring <- menhaden.spring %>%
#   filter(Year >=2010 & Year <=2020)
# test.fall <- menhaden.fall %>%
#   filter(Year >=2010 & Year <=2020) 
# # Write test data set
# write.csv(test.spring,"~/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", row.names = TRUE)
# write.csv(test.fall,"~/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", row.names = TRUE)


#----- Read in Test dataset
# menhaden.spring <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)
# # FOR VIRTUAL PC
# menhaden.spring <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)


#----- Make the mesh
mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("Latitude", "Longitude"), n_knots = 400, type = "cutoff_search") #500
# plot(mesh.spring)
# mesh.spring$mesh$n # extract number of vertices/knots
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("Latitude", "Longitude"), n_knots = 400, type = "cutoff_search")
# plot(mesh.fall)


#----- Read in Extrapolation Grid
# Grid has bathymetry and bottom temp data for each X,Y
# Read in SPRING grid by years
nd.grid.yrs.spring <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP.rds")
nd.grid.yrs.fall <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP.rds")

# For Virtual PC
# nd.grid.yrs <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years_NEFSC-NEAMAP.rds")
nd.grid.yrs.spring <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP.rds")
nd.grid.yrs.fall <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP.rds")



#----- Presence-Absence Model  ----------
tic()
pa.fit <- sdmTMB(
  Presence ~ s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  family = binomial(link = "logit"),
  spatial = "on",
  time = "Year",
  spatiotemporal = "AR1"
)
toc()
#28.45 sec elapsed on c20.m160
sanity(pa.fit)
pa.fit
pa.fit <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/pa-spatiotemporal-fit.rds")

tidy(pa.fit)
# Spatiotemporal model fit by ML ['sdmTMB']
# Formula: Presence ~ s(Bottemp) + s(Depth)
# Mesh: mesh.spring (isotropic covariance)
# Time column: Year
# Data: menhaden.spring
# Family: binomial(link = 'logit')
# 
# coef.est coef.se
# (Intercept)   -14.53    2.52
# sBottemp      -31.06   50.45
# sDepth          0.50  186.34
# 
# Smooth terms:
#   Std. Dev.
# sds(Bottemp)     37.55
# sds(Depth)       72.84
# 
# Spatiotemporal AR1 correlation (rho): 0.21
# Matern range: 0.96
# Spatial SD: 8.48
# Spatiotemporal SD: 9.12
# ML criterion at convergence: 3372.987
tidy(pa.fit, effects = "ran_pars")
# term    estimate std.error
# <chr>      <dbl> <lgl>    
#   1 range      0.957 NA       
# 2 sigma_O    8.48  NA       
# 3 sigma_E    9.12  NA       
# 4 rho        0.211 NA 

menhaden.spring$resids <- residuals(pa.fit) # randomized quantile residuals
qqnorm(menhaden.spring$resids)
qqline(menhaden.spring$resids)

saveRDS(pa.fit, "D:/MODEL_OUTPUT/pa-spatiotemporal-fit.rds")



#----- Predictions
tic()
pa.pred <- predict(pa.fit, newdata = nd.grid.yrs)
toc()
# 152 sec elapsed
saveRDS(pa.pred, file = "D:/MODEL_OUTPUT/pa-spatiotemporal-predictions.rds")
select(pa.pred, Latitude, Longitude, est:epsilon_st) %>%
  as_tibble()



#----- Results
# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, color = {{ column }})) +
    geom_point() +
    coord_fixed()
}

# Predictions with all fixed and random effects

plot_map(pa.pred.sub, est) +
  scale_fill_viridis_c(
    trans = "sqrt",
    # trim extreme high values to make spatial variation more visible
    na.value = "grey", limits = c(0, quantile(exp(pa.pred.sub$est), 0.995))
  ) +
  facet_wrap(~Year) +
  ggtitle("Prediction (fixed effects + all random effects)",
          subtitle = paste("maximum estimated biomass density =", round(max(exp(pa.pred.sub$est))))
  )

# Predictions with just fixed effects
plot_map(pa.pred.sub, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")





