# RipleysK-menhaden.R
######################################
# Janelle L. Morano

# Objectives: Analyze spatial point process data to summarize a point pattern,test hypotheses about the pattern, estimate parameters, and fit models with Ripley's K.

# Using predicted density data from VAST menhaden model

# last updated 6 July 2023
###############################################
###############################################

library(dplyr)
library(spatstat)


#----- Load predicted density data
men.sp <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20230111 Spring/PredDensity_spring_1972-2021.csv", header = TRUE)

# Filter 1 year, 2019, to start
men.sp.2019 <- filter(men.sp, Year == 2019)
# Eliminate absences
men.sp.2019 <- filter(men.sp.2019, D >0)

hist(men.sp.2019$D)
hist(men.sp.2019$logD)
hist(men.sp.2019$D[men.sp.2019$D>0.5])

#### FYI
# Change Inf e.g., from log(0) to NA
if( replace_Inf_with_NA==TRUE ){
  Y_gct = ifelse( abs(Y_gct)==Inf, NA, Y_gct )
}
######

#Try below with only D>0.5
men.sp.2019 <- filter(men.sp.2019, D >0.5)


range(men.sp.2019$Lat) #32.55069 44.79284
range(men.sp.2019$Lon) #-79.12287 -65.23891
mypattern <- ppp(men.sp.2019$Lat, men.sp.2019$Lon, c(32.55069, 44.79284), c(-79.12287, -65.23891))
plot(mypattern)
# Basic summary of data
summary(mypattern)
# Ripley's K-function
# When observed K > expected K == distribution is more clustered than random
plot(Kest(mypattern))
# Envelopes of K-function
plot(envelope(mypattern,Kest))
# Kernel smoother of density
plot(density(mypattern))

# Additional data are added as marks
marks(mypattern) <- men.sp.2019[, 3]
plot(Smooth(mypattern))
