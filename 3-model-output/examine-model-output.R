# examine-model-output.R
###############################################
# Janelle L. Morano
# This assumes you ran a menhaden model in VAST 3.8.2 and you have the fit.fall and fit.spring in your environment still.
# last updated 7 Dec 2021

# 1. Density Estimates
# 1.a. Re-Mapping Density Estimates
# 2. Center of gravity
###############################################
###############################################

####################
# 1. Density Estimates
####################


# SD-- still working on
sd <- fit.spring$parameter_estimates$SD$sd


sample = sample_variable(Sdreport=fit.spring$parameter_estimates$SD, Obj=fit.spring$tmb_list$Obj, variable_name="D_gct" )

# Sample from joint precision matrix
jointp <- as.matrix(fit.spring$parameter_estimates$SD$jointPrecision)

# https://github.com/pbs-assess/sdmTMB#calculating-uncertainty-on-spatial-predictions
# Following above, compute the SE at each location manually using many predictions for each location from your simulation replicates.  You can probably hack this together by doing some manual manipulation of the predicted densities provided by the simulate_data() function in FishStatsUtils, for a VAST model.  

simulate_data(jointp[1:10,1:10], type = 3, random_seed = NULL)
samps <- gather_sims(fit.spring, nsim = 1000)





# Spring
#############
## Get the model estimate of density, D_gct,
## for each grid (g), category (c; not used here single
## univariate); and year (t); and link it spatially to a lat/lon
## extrapolation point.  You can do this for any _gct or _gc
## variable in the Report.
names(fit.spring$Report)[grepl('_gc|_gct', x=names(fit.spring$Report))]

D_gt <- fit.spring$Report$D_gct[,1,] # drop the category
#dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years) #not working
## tidy way of doing this, reshape2::melt() does
## it cleanly but is deprecated
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "YearRep", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
PredDensity.spring <- D %>%
  rename(Year = YearRep) %>%
  mutate(D, logD =log(D))
PredDensity.spring <- tibble(PredDensity.spring)
# Write to a csv for keeping and using later
write_csv(PredDensity.spring, file = "PredDensity_spring.csv")
min(PredDensity.spring$logD) #-24.96282 [(ln(re 1e-06 m-2.kg))]
max(PredDensity.spring$logD) #24.01938 [(ln(re 1e-06 m-2.kg))]


# Fall
#############
## Repeat steps from above
names(fit.fall$Report)[grepl('_gc|_gct', x=names(fit.fall$Report))]

D_gt <- fit.fall$Report$D_gct[,1,] # drop the category
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "YearRep", values_to='D')
D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')
PredDensity.fall <- D %>%
  rename(Year = YearRep) %>%
  mutate(D, logD =log(D))
PredDensity.fall <- tibble(PredDensity.fall)
# Write to a csv for keeping and using later
write_csv(PredDensity.fall, file = "PredDensity_fall.csv")
min(PredDensity.fall$logD) #-27.65603 [(ln(re 1e-06 m-2.kg))]
max(PredDensity.fall$logD) #31.98642 [(ln(re 1e-06 m-2.kg))]


####################
# 2. Abundance Index
####################
# This figure should match the Abundance Index figure
library(tidyverse)
ggplot(est_density, aes(Year, Density)) +
  geom_boxplot()

# Now compare where values for SS3 table come. Should be sum of biomass
total_biomass <- est_density %>%
  group_by(Year) %>%
  mutate(Density, list(~cumsum))
total_biomass <- aggregate(est_density$Density, by=list(Category=est_density$Year), FUN=sum)  

####################
# 3. Re-Mapping Density Estimates
####################

# Spring
#############
## Remake map list locally for recreating plots
mdl.spring <- make_map_info(Region = settings$Region,
                     spatial_list = fit.spring$spatial_list,
                     Extrapolation_List = fit.spring$extrapolation_list)


# Fall
#############
## Remake map list locally for recreating plots
mdl.fall <- make_map_info(Region = settings$Region,
                          spatial_list = fit.fall$spatial_list,
                          Extrapolation_List = fit.fall$extrapolation_list)



# Correcting scales between seasons
#############
# The max and min density between seasons is not equal, so to directly compare, re-scale the density

# Read in the density output to make sure I have what I need
den.spring <- read_csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Spring gamma/PredDensity_spring.csv")
den.fall <- read_csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Fall gamma/PredDensity_fall.csv")

max(den.spring$logD)
max(den.fall$logD)
# 32 was max value across both seasons
den.spring <- mutate(den.spring, CorrDen = logD/32)
max(den.spring$CorrDen)
# 0.7506057
min(den.spring$CorrDen)
# -0.7800881
den.fall <- mutate(den.fall, CorrDen = logD/32)
max(den.fall$CorrDen)
# 0.9995762
min(den.fall$CorrDen)
# -0.8642411

# Now use den.spring and den.fall datasets and $CorrDen to graph predicted density

########
# Create basemap
library(tidyverse)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
world <- ne_countries(scale = "medium", returnclass = "sf")
us <- ne_states(geounit = "United States of America", returnclass = "sf")  
canada <- ne_states(geounit = "Canada", returnclass = "sf")

## Draw just 1 years for Spring
ggplot(data = world) +
  geom_sf(data = us) + 
  geom_sf(data = canada) +
  coord_sf (xlim = c(-81,-64), ylim = c (32,46), expand = FALSE ) +
  geom_point(data = subset(den.spring, Year %in% c(2019)), 
             aes(Lon, Lat, color=CorrDen, group=NULL), #used to be log(D)
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=1, stroke=0.5,shape=16) + 
  facet_wrap('Year', ncol = 1) +
  theme(strip.text.x = element_text(size = 15),
        strip.background = element_blank()) +
  scale_color_viridis_c(option = "viridis", limit = c(-0.79, 1), oob = scales::squish) +
  theme_classic() +
  theme(axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Spring")

## Draw just 1 years for Fall
ggplot(data = world) +
  geom_sf(data = us) + 
  geom_sf(data = canada) +
  coord_sf (xlim = c(-81,-64), ylim = c (32,46), expand = FALSE ) +
  geom_point(data = subset(den.fall, Year %in% c(2019)), 
             aes(Lon, Lat, color=CorrDen), #used to be log(D)   #, group=NULL
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=1, stroke=0.5,shape=16) + 
  facet_wrap(~Year, ncol = 1) +
  theme(strip.background = element_blank(), 
        strip.text = element_blank()) +
  scale_color_viridis_c(option = "viridis", limit = c(-0.79, 1), oob = scales::squish) +
  theme_classic() +
  theme(axis.text = element_blank()) +
  xlab("longitude") + 
  ylab("latitude") +
  ggtitle("Fall")

## Now draw all years
# I can't seem to modify the year labels
ggplot(data = world) +
  # geom_sf(data = us) + 
  # geom_sf(data = canada) +
  geom_sf() + # use this when not using "us" and "canada"
  coord_sf (xlim = c(-83,-60), ylim = c (25,48), expand = FALSE ) +
  geom_point(data = subset(den.fall, Year %in% c(2007:2021)), # if all data (data = den.spring, 
             aes(Lon, Lat, color=CorrDen, group=NULL), #used to be log(D)
             ## These settings are necessary to avoid
             ## overlplotting which is a problem here. May need
             ## to be tweaked further.
             size=.5, stroke=0.5,shape=16) + 
  facet_wrap('Year') +
  theme(strip.text.x = element_blank(),
        strip.background = element_blank()) +
  scale_color_continuous(type = "viridis") +
  # scale_color_viridis_c(option = "magma") +
  theme_classic() + #theme_bw()
  theme(axis.text = element_blank()) #+
# xlab("longitude") + 
# ylab("latitude") 


####################
# Center of Gravity
####################
fit.spring[["Report"]][["mean_Z_ctm"]]
# Spatial_axis = E_km is eastings
easting <- fit.spring[["Report"]][["mean_Z_ctm"]][,,1]
# Spatial_axis = N_km is northings
northing <- fit.spring[["Report"]][["mean_Z_ctm"]][,,2]
# pull together
cog <- data.frame(northing, easting)
write_csv(cog, file = "CenterofGravity_spring.csv")
cog <- drop_units(cog)
ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
  geom_line()+
  theme_classic()

fit.fall[["Report"]][["mean_Z_ctm"]]
# 1 is eastings
easting <- fit.fall[["Report"]][["mean_Z_ctm"]][,,1]
# 2 is northings
northing <- fit.fall[["Report"]][["mean_Z_ctm"]][,,2]
# Year, not sure where it is so I'm going to guess it's in order from low to high and create
year <- c(2007:2019)

cog <- data.frame(year, northing, easting)
write_csv(cog, file = "CenterofGravity_fall.csv")
cog <- drop_units(cog)
ggplot(cog, aes(x=easting, y=northing, color = "#FF68F46FF", lwd=5.0)) +
  geom_line()+
  theme_classic()





#convert northing/easting to UTM
cord.dec = SpatialPoints(cbind(cog$long, -cog$lat), proj4string=CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32748"))
cord.UTM

#northing
ggplot(cog, aes(x=year, y=northing)) +
  geom_line()
#easting
ggplot(cog, aes(x=year, y=easting)) +
  geom_line()



