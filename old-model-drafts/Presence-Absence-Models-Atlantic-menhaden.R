# Presence-Absence-Models-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Presence-Absence models in
# GAM
# sdmTMB

# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 20 July 2023
###############################################
###############################################


library(tidyverse)
library(mgcv)


#----- Data Prep ------------------------------------------------------------

menhaden <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230612.csv", header = TRUE)
menhaden <- menhaden %>% 
  mutate(presence = case_when(.$Abundance == 0 ~ "0",
                              .$Abundance >= 1 ~ "1"))
menhaden$presence <-  as.numeric(menhaden$presence)
# Remove NAs
sapply(menhaden, function(x) sum(is.na(x)))
menhaden <- menhaden %>%
  filter_at(vars(Depth, Bottemp, Abundance), all_vars(!is.na(.)))

# Create Spring and Fall datasets, from 1972+
menhaden.spring <- menhaden %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972)
menhaden.fall <- menhaden %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)


#----- GAM of Presence by s(Year, by = State) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") WITH REML---------------------------------------------------------------

#----- Run the model

#### SPRING Presence/Absence with Time & Space interaction
pa.gam.spring <- gam(presence ~ s(Year, by = LatCat, k = 10) + s(Bottemp) + s(Depth) + LatCat, family = binomial(link = "logit"), method = "REML", data = menhaden.spring)
gam.check(pa.gam.spring)
plot(pa.gam.spring, rug = TRUE, residuals = TRUE, pch = 1, cex = 1, shift = coef(pa.gam.fall)[1])

concurvity(pa.gam.spring, full = TRUE) #if values are high, >0.8, run with FALSE
concurvity(pa.gam.spring, full = FALSE)

# pa2.gam.spring <- gam(presence ~ s(Year) + s(Bottemp) + s(Depth) + LatCat, family = binomial(link = "logit"), method = "REML", data = menhaden.spring)
# gam.check(pa2.gam.spring)
# plot(pa2.gam.spring)

AIC(pa.gam.spring) #lower AIC = 9593.833
# AIC(pa2.gam.spring) #9601.214

# Predictions
preddata.spring <- data.frame()
for (i in unique(menhaden.spring$LatCat)) {
  new <- data.frame(Year = sort(unique(menhaden.spring$Year)),
                    LatCat = i,
                    Bottemp = median(menhaden.spring$Bottemp[menhaden.spring$LatCat == i], na.rm=TRUE),
                    Depth = median(menhaden.spring$Depth[menhaden.spring$LatCat == i], na.rm=TRUE))
  preddata.spring <- rbind(preddata.spring, new)
}

pred.gam.spring <- predict(pa.gam.spring, se.fit=TRUE, newdata=preddata.spring, type = "response")
range(log(menhaden.spring$Biomass+1))
range(pred.gam.spring$fit)
plot(preddata.spring$Year,pred.gam.spring$fit,type="b")  

#### FALL Presence/Absence with Time & Space interaction
pa.gam.fall <- gam(presence ~ s(Year, by = LatCat) + s(Bottemp) + s(Depth) + LatCat, family = binomial(link = "logit"), method = "REML", data = menhaden.fall)
gam.check(pa.gam.fall)
plot(pa.gam.fall, rug = TRUE, residuals = TRUE, pch = 1, cex = 1, shift = coef(pa.gam.fall)[1])

# Predictions
preddata.fall <- data.frame()
for (i in unique(menhaden.fall$LatCat)) {
  new <- data.frame(Year = sort(unique(menhaden.fall$Year)),
                    LatCat = i,
                    Bottemp = median(menhaden.fall$Bottemp[menhaden.fall$LatCat == i], na.rm=TRUE),
                    Depth = median(menhaden.fall$Depth[menhaden.fall$LatCat == i], na.rm=TRUE))
  preddata.fall <- rbind(preddata.fall, new)
}

pred.gam.fall <- predict(pa.gam.fall, se.fit=TRUE, newdata=preddata.fall, type = "response")
range(log(menhaden.fall$Biomass+1))
range(pred.gam.fall$fit)
plot(preddata.fall$Year,pred.gam.fall$fit,type="b")  
