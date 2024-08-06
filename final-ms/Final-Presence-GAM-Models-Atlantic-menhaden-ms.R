# Final-Models-Atlantic-menhaden-ms.R
######################################
# Janelle L. Morano

# Data and choice models for the Atlantic menhaden manuscript.

# last updated 26 July 2024

###############################################
###############################################

###########################################################################################
#-Table of Contents ------------------------------------------------------------
#----- Data Prep (federal and state surveys)
#       (Line 26)
#----- Presence-Absence GAM: Presence ~ s(Year, by = State) + s(Depth) + s(Bottemp) + State 
#       (Line 71)
#----- Biomass GAM: Biomass ~ s(Year, by = State) + s(Depth) + s(Bottemp) + State
#----- sdmTMB: 
###########################################################################################


library(tidyverse)
library(mgcv)


###########################################################################################
#----- Data Prep ------------------------------------------------------------
###########################################################################################


# #----- Federal surveys
# Atlantic menhaden data from NEFSC and NEAMAP surveys
federal <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240617.csv", header = TRUE)
# Add Presence/Absence column numeric
federal$Presence <- as.numeric(federal$Presence)

# Remove NAs
sapply(federal, function(x) sum(is.na(x)))
federal <- federal %>%
  # filter_at(vars(Depth, Bottemp, Abundance, Presence), all_vars(!is.na(.)))
  filter_at(vars(Abundance, Presence), all_vars(!is.na(.)))
  
# Create Spring and Fall datasets, from 1972+
federal.spring <- federal %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972)
federal.fall <- federal %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)


# #----- State Data
statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240726.csv", header = TRUE)

sapply(statedata, function(x) sum(is.na(x)))
# SEAMAP doesn't have any depth, and so is completely lost
# Ignore temp and depth to maximize spatial coverage
statedata <- statedata %>%
  filter_at(vars(MenhadenTotal, Presence), all_vars(!is.na(.)))


#----- Create Spring and Fall datasets
state.spring <- statedata[statedata$Month > 2 & statedata$Month < 5,]
state.fall <- statedata[statedata$Month > 8 & statedata$Month <11 ,]


#----- Create a list of the datasets
data.list <- list(federal.spring = federal.spring, federal.fall = federal.fall, state.spring = state.spring, state.fall = state.fall)

#----- Save list as object
saveRDS(data.list, "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/data.list.rds")

# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/data.list.rds")




###########################################################################################
#----- Presence/Absence GAM ---------------------------------------------------------------
###########################################################################################


#----- Run the models: Just Presence, no covariates -----------------------------------
# Federal spring and fall
pa.gam.list <- list()
pa.gam.summaries <- list()
for (name in names(data.list[1:2])) {
  pa.gam.list[[name = name]] = gam(Presence ~ s(Year, by = as.factor(State)) + as.factor(State), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
}

# State spring and fall
# Append to previous list
for (name in names(data.list[3:4])) {
  pa.gam.list[[name = name]] = gam(Presence ~ s(Year, by = as.factor(State)) + as.factor(State), family = binomial(link = "logit"), method = "REML", data = data.list[[name]])
  pa.gam.summaries[[name = name]] <- summary(pa.gam.list[[name]])
}

#----- Save model runs as RDS
saveRDS(pa.gam.list, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-results.rds")
saveRDS(pa.gam.summaries, file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-summaries.rds")

#----- Check the models ---------------------------------------------------------------
# Get model coefficients to compare
library(modelsummary)
pa.gam.table <- modelsummary(pa.gam.list, output = "data.frame")
# alternative view
modelsummary(pa.gam.list, output = "flextable")
results <- summary(pa.gam.list[[1]])

# Review individuals
summary(pa.gam.list[[1]])
gam.check(pa.gam.list[[1]])
concurvity(pa.gam.list[[1]], full = TRUE) #if values are high, >0.8, run with FALSE
concurvity(pa.gam.list, full = FALSE)
plot(pa.gam.list[[1]], rug = TRUE, residuals = TRUE, pch = 1, cex = 1, shift = coef(pa.gam.list[[1]])[1])


# Visualize effects
png(file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/Final-model-output/PA-GAM-federal-spring-effects.png", width=800, height=800)
gam.check(pa.gam.list[[1]])
dev.off()

png(file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/Final-model-output/PA-GAM-federal-fall-effects.png", width=800, height=800)
gam.check(pa.gam.list[[2]])
dev.off()

png(file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/Final-model-output/PA-GAM-state-spring-effects.png", width=800, height=800)
gam.check(pa.gam.list[[3]])
dev.off()

png(file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/Final-model-output/PA-GAM-state-fall-effects.png", width=800, height=800)
gam.check(pa.gam.list[[4]])
dev.off()

#par(mfrow = c(1,1))


#----- Make New Data for Predictions ---------------------------------------------------------------
#-- Federal Spring
fed.preddata.pa.gam.spring <- data.frame()
for (i in unique(data.list$federal.spring$State)) {
  new <- data.frame(Year = sort(unique(data.list$federal.spring$Year)),
                    State = i,
                    Bottemp = median(data.list$federal.spring$Bottemp[data.list$federal.spring$State == i], na.rm=TRUE),
                    Depth = median(data.list$federal.spring$Depth[data.list$federal.spring$State == i], na.rm=TRUE))
  fed.preddata.pa.gam.spring <- rbind(fed.preddata.pa.gam.spring, new)
}
#-- Federal Fall
fed.preddata.pa.gam.fall <- data.frame()
for (i in unique(data.list$federal.fall$State)) {
  new <- data.frame(Year = sort(unique(data.list$federal.fall$Year)),
                    State = i,
                    Bottemp = median(data.list$federal.fall$Bottemp[data.list$federal.fall$State == i], na.rm=TRUE),
                    Depth = median(data.list$federal.fall$Depth[data.list$federal.fall$State == i], na.rm=TRUE))
  fed.preddata.pa.gam.fall <- rbind(fed.preddata.pa.gam.fall, new)
}
#-- State Spring
st.preddata.pa.gam.spring <- data.frame()
for (i in unique(data.list$state.spring$State)) {
  new <- data.frame(Year = sort(unique(data.list$state.spring$Year)),
                    State = i,
                    SurfTemp = median(data.list$state.spring$SurfTemp[data.list$state.spring$State == i], na.rm=TRUE),
                    Depth.m = median(data.list$state.spring$Depth.m[data.list$state.spring$State == i], na.rm=TRUE))
  st.preddata.pa.gam.spring <- rbind(st.preddata.pa.gam.spring, new)
}
#-- State Fall
st.preddata.pa.gam.fall <- data.frame()
for (i in unique(data.list$state.fall$State)) {
  new <- data.frame(Year = sort(unique(data.list$state.fall$Year)),
                    State = i,
                    SurfTemp = median(data.list$state.fall$SurfTemp[data.list$state.fall$State == i], na.rm=TRUE),
                    Depth.m = median(data.list$state.fall$Depth.m[data.list$state.fall$State == i], na.rm=TRUE))
  st.preddata.pa.gam.fall <- rbind(st.preddata.pa.gam.fall, new)
}


#----- Create a list of the New Predicting Data datasets
preddata.pa.gam.list <- list(fed.preddata.pa.gam.spring = fed.preddata.pa.gam.spring, fed.preddata.pa.gam.fall = fed.preddata.pa.gam.fall, st.preddata.pa.gam.spring = st.preddata.pa.gam.spring, st.preddata.pa.gam.fall = st.preddata.pa.gam.fall)
saveRDS(preddata.pa.gam.list , file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-preddata.rds")

#----- Predictions for PA GAM models ---------------------------------------------------------------
#predictions.pa.gam <- list()
# # Trouble with loop, just do individually below
# for (names in names(pa.gam.list[1:2])) {
#    preds <- predict(pa.gam.list[[names]], se.fit=TRUE, newdata=preddata.pa.gam.list[[names]], type = "response") 
#    predictions.pa.gam <- list(names = preds)
#   }

# Loop above isn't working so doing this individually
predictions.pa.gam.fed.spring <- predict(pa.gam.list[[1]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response")
predictions.pa.gam.fed.fall <- predict(pa.gam.list[[2]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response")
predictions.pa.gam.state.spring <- predict(pa.gam.list[[3]], se.fit=TRUE, newdata=preddata.pa.gam.list[[3]], type = "response")
predictions.pa.gam.state.fall <- predict(pa.gam.list[[4]], se.fit=TRUE, newdata=preddata.pa.gam.list[[4]], type = "response")


# Create Indiv Datasets of Predictions
predictions.pa.gam.fed.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.pa.gam.fed.spring))
predictions.pa.gam.fed.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.pa.gam.fed.fall))
predictions.pa.gam.state.spring.df <- cbind(preddata.pa.gam.list[[3]], data.frame(predictions.pa.gam.state.spring))
predictions.pa.gam.state.fall.df <- cbind(preddata.pa.gam.list[[4]], data.frame(predictions.pa.gam.state.fall))

# Make list of Dataframes of Predictions
predictions.pa.gam.list <- list(predictions.pa.gam.fed.spring.df = predictions.pa.gam.fed.spring.df, predictions.pa.gam.fed.fall.df = predictions.pa.gam.fed.fall.df, predictions.pa.gam.state.spring.df = predictions.pa.gam.state.spring.df, predictions.pa.gam.state.fall.df = predictions.pa.gam.state.fall.df)
saveRDS(predictions.pa.gam.list, file = "//Users/janellemorano/Git/AtlanticMenhaden/final-ms/PA-GAM-predictions.rds")
    

# # Some quick plots. Leaving here for now
# pred.pa.gam.spring <- predict(pa.gam.spring, se.fit=TRUE, newdata=preddata.spring, type = "response")
# range(log(menhaden.spring$Biomass+1))
# range(pred.pa.gam.spring$fit)
# plot(preddata.spring$Year,pred.pa.gam.spring$fit,type="b")
# # Create Dataset of Predictions from PA GAM
# predictions.pa.gam.spring <- cbind(preddata.spring, data.frame(pred.pa.gam.spring))
# # OPTIONAL Save predictions.pa.gam.spring as object
# # write.csv(predictions.pa.gam.spring, "", row.names = TRUE)
# 
# pred.pa.gam.fall <- predict(pa.gam.fall, se.fit=TRUE, newdata=preddata.fall, type = "response")
# range(log(menhaden.fall$Biomass+1))
# range(pred.pa.gam.fall$fit)
# plot(preddata.fall$Year,pred.pa.gam.fall$fit,type="b")
# # Create Dataset of Predictions from PA GAM
# predictions.pa.gam.fall <- cbind(preddata.fall, data.frame(pred.pa.gam.fall))
# # OPTIONAL Save predictions.pa.gam.fall as object
# # write.csv(predictions.pa.gam.fall, "", row.names = TRUE)





###########################################################################################
#----- Biomass GAM ---------------------------------------------------------------
###########################################################################################


#----- Run the models  ---------------------------------------------------------------
# Federal spring and fall
bgam.list <- list()
bgam.summaries <- list()
for (name in names(data.list[1:2])) {
  bgam.list[[name = name]] = gam(Biomass ~ s(Year, by = as.factor(State)) + s(Bottemp) + s(Depth) + as.factor(State) + (1|Survey), family ="quasipoisson"(link="log"), method = "REML", data = data.list[[name]])
  bgam.summaries[[name = name]] <- summary(bgam.list[[name]])
}

# State spring and fall
# Append to previous list
for (name in names(data.list[3:4])) {
  bgam.list[[name = name]] = gam(Weight.kg ~ s(Year, by = as.factor(State)) + s(SurfTemp) + s(Depth.m) + as.factor(State) + (1|Survey), family ="quasipoisson"(link="log"), data = data.list[[name]], method = "REML")
  bgam.summaries[[name = name]] <- summary(bgam.list[[name]])
}

saveRDS(bgam.list, file ="/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-results.rds")
saveRDS(bgam.summaries, file ="/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-summaries.rds")

summary(bgam.list[[1]])
gam.check(bgam.list[[1]])


#----- Predictions for GAM models ---------------------------------------------------------------
predictions.bgam.fed.spring <- predict(bgam.list[[1]], se.fit=TRUE, newdata=preddata.pa.gam.list[[1]], type = "response")
predictions.bgam.fed.fall <- predict(bgam.list[[2]], se.fit=TRUE, newdata=preddata.pa.gam.list[[2]], type = "response")
predictions.bgam.state.spring <- predict(bgam.list[[3]], se.fit=TRUE, newdata=preddata.pa.gam.list[[3]], type = "response")
predictions.bgam.state.fall <- predict(bgam.list[[4]], se.fit=TRUE, newdata=preddata.pa.gam.list[[4]], type = "response")

# Create Indiv Datasets of Predictions
predictions.bgam.fed.spring.df <- cbind(preddata.pa.gam.list[[1]], data.frame(predictions.bgam.fed.spring))
predictions.bgam.fed.fall.df <- cbind(preddata.pa.gam.list[[2]], data.frame(predictions.bgam.fed.fall))
predictions.bgam.state.spring.df <- cbind(preddata.pa.gam.list[[3]], data.frame(predictions.bgam.state.spring))
predictions.bgam.state.fall.df <- cbind(preddata.pa.gam.list[[4]], data.frame(predictions.bgam.state.fall))

# Make list of Dataframes of Predictions
predictions.bgam.list <- list(predictions.bgam.fed.spring.df = predictions.bgam.fed.spring.df, predictions.bgam.fed.fall.df = predictions.bgam.fed.fall.df, predictions.bgam.state.spring.df= predictions.bgam.state.spring.df, predictions.bgam.state.fall.df = predictions.bgam.state.fall.df)
saveRDS(predictions.bgam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/final-ms/B-GAM-predictions.rds")
