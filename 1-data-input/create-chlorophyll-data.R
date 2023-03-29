# create-chlorophyll-data.R
######################################
# Janelle L. Morano
# Setting up chlorophyll data for using

# last updated 22 February 2023
###############################################
###############################################


#### AQUA MODIS ####
#########################
modis <- read.csv("/Users/janellemorano/DATA/Chlorophyll-a/AquaMODIS/erdMH1chlamday_16d7_984a_b5c3.csv", header = TRUE)
str(modis)

modis <- modis[modis$chlorophyll != "NaN",]
modis <- modis[-1,]

modis$chlorophyll <- as.numeric(modis$chlorophyll)

library(lubridate)
modis$time2 <- ymd_hms(modis$time)
modis$year <- year(modis$time2)
modis$month <- month(modis$time2)

# Keep only SPRING (March-April)
modis.2 <- modis[modis$month %in% c(3,4), ]
library(dplyr)
modis.sp <- modis.2 %>%
  group_by(latitude, longitude, year) %>%
  summarise(avechlor = mean(chlorophyll))

# Write csv of average monthly
write.csv(modis.sp, "/Users/janellemorano/DATA/Chlorophyll-a/AquaMODIS/chlorophyll-AquaMODIS-USeastcoast-2003-2022-spring-monthly-ave.csv")

# Keep only FALL (September-October
modis.3 <- modis[modis$month %in% c(9,10), ]
modis.fa <- modis.3 %>%
  group_by(latitude, longitude, year) %>%
  summarise(avechlor = mean(chlorophyll))

# Write csv of average monthly
write.csv(modis.fa, "/Users/janellemorano/DATA/Chlorophyll-a/AquaMODIS/chlorophyll-AquaMODIS-USeastcoast-2003-2022-fall-monthly-ave.csv")


#### NOAA S-NPPVIIRS ####
#########################
chl <- read.csv("/Users/janellemorano/DATA/Chlorophyll-a/noaacwNPPVIIRSSQchlaMonthly_3ea2_2e18_f050.csv", header = TRUE)
str(chl)

chl <- chl[chl$chlor_a != "NaN",]
chl <- chl[-1,]

chl$chlor_a <- as.numeric(chl$chlor_a)

library(lubridate)
chl$time2 <- ymd_hms(chl$time)
chl$year <- year(chl$time2)
chl$month <- month(chl$time2)

# Keep only SPRING (March-April)
chl.2 <- chl[chl$month %in% c(3,4), ]
library(dplyr)
chl.sp <- chl.2 %>%
  group_by(latitude, longitude, year) %>%
  summarise(avechlor = mean(chlor_a))

# Write csv of average monthly
getwd()
write.csv(chl.sp, "chlorophyll-NOAA-S-NPPVIIRS-USeastcoast-2012-2021-spring-monthly-ave.csv")

# Keep only FALL (September-October
chl.3 <- chl[chl$month %in% c(9,10), ]
chl.fa <- chl.3 %>%
  group_by(latitude, longitude, year) %>%
  summarise(avechlor = mean(chlor_a))

# Write csv of average monthly
getwd()
write.csv(chl.fa, "chlorophyll-NOAA-S-NPPVIIRS-USeastcoast-2012-2021-fall-monthly-ave.csv")
