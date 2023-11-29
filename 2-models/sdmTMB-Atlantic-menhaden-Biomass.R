# sdmTMB-Atlantic-menhaden-Biomass.R
######################################
# Janelle L. Morano

# Objectives:
# Build Biomass spatio-temporal model in sdmTMB

# Primarily to compare with VAST model

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


# Set working directory on Mac
setwd("/Users/janellemorano")
# Set working directory on Cloud Server PC
setwd("D/:")




#----- Data Prep ------------------------------------------------------------

# Full dataset
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

# # Read in test datasets created above
# menhaden.spring <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("~/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)
# # FOR VIRTUAL PC
# menhaden.spring <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.spring.csv", header = TRUE)
# menhaden.fall <- read.csv("D:/DATA/Atlantic_menhaden_modeling/1-data-input/test.fall.csv", header = TRUE)

#----- Make the mesh

mesh.spring <- make_mesh(menhaden.spring, xy_cols = c("Latitude", "Longitude"), n_knots = 300, type = "cutoff_search") #500
# plot(mesh.spring)
# mesh.spring$mesh$n # extract number of vertices/knots
mesh.fall <- make_mesh(menhaden.fall, xy_cols = c("Latitude", "Longitude"), n_knots = 300, type = "cutoff_search")
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




#----- Model fit  ------------------------------------------------------------


#----- Spatiotemporal Model  ----------
tic()
st.fit <- sdmTMB(
  Biomass ~ s(Bottemp) + s(Depth),
  family = tweedie(link = "log"),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  spatial = "off", #off when using spatiotemporal
  spatiotemporal = "AR1"
)
toc()
#Test dataset won't converge
#Full dataset: 4895.55  sec elapsed
sanity(st.fit)
#✔ Non-linear minimizer suggests successful convergence
#✔ Hessian matrix is positive definite
#✔ No extreme or very small eigenvalues detected
#✔ No gradients with respect to fixed effects are >= 0.001
#✔ No fixed-effect standard errors are NA
#✖ `ln_smooth_sigma` standard error may be large
#ℹ Try simplifying the model, adjusting the mesh, or adding priors
#
#✔ No sigma parameters are < 0.01
#✔ No sigma parameters are > 100
#✔ Range parameter doesn't look unreasonably large

# Save and then move it to DATA storage
# on mac
# saveRDS(st.fit, file = "/Users/janellemorano/MODEL_OUTPUT/_currentrun/st.fit-spatiotemporal.rds")
# on Virtual PC
saveRDS(st.fit, file = "D:/MODEL_OUTPUT/st.fit-spatiotemporal-fulldata.rds" )
# st.fit <- readRDS("D:/MODEL_OUTPUT/st.fit-spatiotemporal-fulldata.rds")

# Read on Mac
st.fit <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/st.fit-spatiotemporal-fulldata.rds")
# st.fit
tidy(st.fit)
param <- tidy(st.fit, effects = "ran_pars", conf.int = TRUE)

## Basic sanity checks on model
sanity(st.fit)

## Look at smoother effect in link space with randomized quantile partial residuals
#visreg::visreg(st.fit, xvar = "Bottemp")

# Or on the response scale
#visreg::visreg(st.fit, xvar = "Bottemp", scale = "response")


#----- Make predictions
tic()
st.fit.pred <- predict(st.fit, newdata = nd.grid.yrs.spring, return_tmb_object = TRUE) #need return_tmb_object = TRUE to be able to do index and COG
toc()
#1630.09 sec elapsed

# on Virtual PC
saveRDS(st.fit.pred, file = "D:/MODEL_OUTPUT/st.fit.pred-spatiotemporal-predictions.rds" )

# Read into Mac
st.fit.pred <- readRDS(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/st.fit.pred-spatiotemporal-predictions.rds")

p <- select(st.fit.pred, Longitude:epsilon_st) %>%
  as_tibble()
# Save as a csv
write.table(p, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/3-model-output-save/sdmTMB/st.fit.pred-spatiotemporal-predictions-byloc.csv")


#----- Make figures
# Function to make maps
plot_map <- function(dat, column) {
  ggplot(dat, aes(X, Y, fill = {{ column }})) +
    geom_point() +
    coord_fixed()
}


# Predictions with all fixed and random effects
# Subset a few years
psub <- p %>% filter(Year == 2021)

ggplot(psub, aes(X, Y, fill = exp(est))) +
  geom_point() +
  coord_fixed() +
  scale_fill_viridis_c(trans = "sqrt", 
                       na.value = "grey", 
                       limits = c(0, quantile(exp(psub$est), 0.995))) # trim extreme high values to make spatial variation more visible
  #facet_wrap(~Year)
ggtitle("Prediction (fixed effects + all random effects)",
        subtitle = paste("maximum estimated biomass density =", round(max(exp(p2$est))))
        
        

# Predictions with just fixed effects
plot_map(p2, exp(est_non_rf)) +
  scale_fill_viridis_c() +
  ggtitle("Prediction (fixed effects only)")

# Spatial random effects
plot_map(p2, omega_s) +
  scale_fill_gradient2() +
  ggtitle("Spatial random effects only")





# Area-weighted standardization population index
index <- get_index(p)
# Need to do the prediction with return_tmb_object = TRUE

# Center of gravity
cog <- get_cog(p, format = "wide")



#----- Delta Spatio-Temporal Model ----------------------
# Model Type        |	Built-in delta function	  | Presence-absence model		| Positive catch model
# Delta-gamma	      | delta_gamma()		          | binomial(link = "logit")	| Gamma(link = "log")
# Delta-lognormal		| delta_lognormal()		      | binomial(link = "logit")	| lognormal(link = "log")
# Delta-NB1	        | delta_truncated_nbinom1()	| binomial(link = "logit")	| truncated_nbinom1(link = "log")
# Delta-NB2	        |delta_truncated_nbinom2()	| binomial(link = "logit")	| truncated_nbinom2(link = "log")

tic()
delta.fit <- sdmTMB(
  
  Biomass ~ 1 + s(Bottemp) + s(Depth),
  data = menhaden.spring,
  mesh = mesh.spring,
  time = "Year", # column in `data`
  family = delta_gamma()
)
toc()
# 264.68 sec elapsed sec elapsed 0 + as.factor(Year) + s(log(Depth))
# 576.69 sec elapsed s(Bottemp)
# 644.38 sec elapsed (poisson link, delta_poisson_link_gamma())

## Basic sanity checks on model
sanity(delta.fit)
#See `?run_extra_optimization()`
#ℹ Or refit with `control = sdmTMBcontrol(newton_loops = 1)`
#Try simplifying the model, adjusting the mesh, or adding priors
