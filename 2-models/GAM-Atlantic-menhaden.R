# GAM-Atlantic-menhaden.R
######################################
# Janelle L. Morano

# Objectives:
# 1. Determine the affect of depth, temperature, and chlorophyll on menhaden abundance
# 2. Determine the spatial areas of highest menhaden density
# 3. Use GAM model to address if menhaden distribution is different between, or changes over time, along a north/south gradient or inshore/offshore gradient, using different spatial variables (strata, latitude, distance from shore)


# Primarily to compare with VAST model
# Using NEFSC and NEAMAP data that goes into VAST menhaden model

# last updated 19 July 2023
###############################################
###############################################

#----- Table of Contents -------------------------------------------------

# A. Setup NMFS & NEAMAP Survey Data in a list
#      -surveydata.spring: NEFSC+NEAMAP, without chlorophyll data, 1972-2021, SPRING season only
#      -surveydata.fall: NEFSC+NEAMAP, without chlorophyll data, 1972-2021, FALL season only
#      -surveydata.chl.spring: NEFSC+NEAMAP, with chlorophyll data, 2012-2021, SPRING season only
#      -surveydata.chl.fall: NEFSC+NEAMAP, with chlorophyll data, 2012-2021, FALL season only
# 1. GLM: Biomass (weight CPUE: kg/tow) ~ as.factor(Year) + Bottemp + Depth + Stratum (family="quasipoisson")
# 2. GAM: Biomass (weight CPUE: kg/tow) ~ as.factor(Year) + s(Bottemp) + s(Depth) + Stratum, family ="quasipoisson"(link="log") 
# 2b. GAM: Biomass (weight CPUE: kg/tow) ~ s(Year) + s(Bottemp) + s(Depth) + Stratum, family ="quasipoisson"(link="log")
# 3. GAM: Biomass (weight CPUE: kg/tow) ~ s(Year) + s(Bottemp) + s(Depth) + STATE, family ="quasipoisson"(link="log")
# 4. GAM of Biomass by s(Year, by = State) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") 
# 4b. GAM of Biomass by s(Year, by = State) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") WITH REML
# Z. AIC of models
# (not yet) GAM of Biomass ~ *create interaction between Strata and Year
# (not yet) GAM2: Biomass ~ s(Year) + Stratum + Bottemp + Depth + Chlorophyll


library(tidyverse)
library(janitor)
library(mgcv)
library(flextable)
library(modelsummary)
library(stats)



#----- A. Setup NMFS & NEAMAP Survey Data -------------------------------------------------

surveydata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/combined-catch-envtl-20230612.csv", header = TRUE)
# Remove salinity cols, Surftemp, and X because not working with now
surveydata <- surveydata %>% select(-c(X, Surfsalin, Botsalin, Surftemp))

#----- Find Strata where NO MENHADEN are ever found
surveydata2 <- surveydata %>%
  group_by(Stratum) %>%
  summarise(total = sum(Biomass))
remove <- c(surveydata2[,1][surveydata2[,2]==0])
surveydata <- surveydata %>% filter(!Stratum %in% c(remove))

# Max Biomass, might be worth removing
surveydata[which.max(surveydata$Biomas),]

#----- Remove rows with NAs NO DON'T BECAUSE IT TAKES OUT YEARS BEFORE 2012 BECAUSE THERE ARE NOT CHLOROPHYLL DATA
# sapply(surveydata, function(x) sum(is.na(x)))
# surveydata <- na.omit(surveydata)

#----- Create Spring and Fall datasets
## surveydata
surveydata.spring <- surveydata[surveydata$Season == "SPRING",]
surveydata.fall <- surveydata[surveydata$Season == "FALL",]

# Max Biomass
surveydata.spring[which.max(surveydata.spring$Biomass),]
surveydata.fall[which.max(surveydata.fall$Biomass),]
# Mean Biomass by Strata
mean.spring <- surveydata.spring %>%
  group_by(Stratum) %>%
  summarise(AveBiomass = mean(Biomass)) %>%
  arrange(desc(AveBiomass))
write.csv(mean.spring, "/Users/janellemorano/Git/AtlanticMenhaden/figs-maps/table-mean-biomass-by-strata-spring.csv")
mean.fall <- surveydata.fall %>%
  group_by(Stratum) %>%
  summarise(AveBiomass = mean(Biomass)) %>%
  arrange(desc(AveBiomass))
write.csv(mean.fall, "/Users/janellemorano/Git/AtlanticMenhaden/figs-maps/table-mean-biomass-by-strata-fall.csv")

#----- Create 1972-2021 dataset
surveydata.spring <- filter(surveydata.spring, Year >=1972)
surveydata.fall <- filter(surveydata.fall, Year >= 1972)

#----- Create 2012-2021 that has chlorophyll
surveydata.chl.spring <- filter(surveydata.spring, Year >=2012)
surveydata.chl.fall <- filter(surveydata.fall, Year >= 2012)

#----- Create separate NEFSC and NEAMAP datasets
nefsc.spring <- filter(surveydata.spring, Survey == "NEFSC")
nefsc.fall <- filter(surveydata.fall, Survey == "NEFSC")
neamap.spring <- filter(surveydata.spring, Survey == "NEFSC")
neamap.fall <- filter(surveydata.fall, Survey == "NEAMAP")

# library(corrplot)
# M <- cor(surveydata.chl.spring[, c(5, 9, 10, 11, 13, 16)], use = "complete.obs")
# corrplot(M, method = 'ellipse', type = 'upper')

#----- Create a list of the datasets
data.list <- list(all.spring = surveydata.spring, all.fall = surveydata.fall, wchl.spring = surveydata.chl.spring, wchl.fall = surveydata.chl.fall, nefsc.spring = nefsc.spring, nefsc.fall = nefsc.fall, neamap.spring = neamap.spring, neamap.fall = neamap.fall)
# Save list as object
saveRDS(data.list, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/federal.data.list.0612.rds")

#----- Strata with biomass: Spring
unique(surveydata.chl.spring$Stratum)
# [1] "15N" "53"  "44"  "61"  "63"  "64"  "14N" "41"  "62"  "65"  "13N" "38"  "66"  "12N" "35"  "11N" "32"  "29"  "10N" "69"  "9N" # [22] "26"  "74"  "73"  "8N"  "23"  "20"  "7N"  "3"   "2"   "4"   "17"  "6N"  "12"  "7"   "1"   "8"   "11"  "10"  "15"  "14"  "6" # [43] "5N"  "4N"  "13"  "5"   "9"   "3N"  "19"  "46"  "18"  "2N"  "1N"  "16"  "RIN" "BIN" "45"  "25"  "24"  "56"  "59"  "28"  "60" 
# [64] "22"  "27"  "30"  "37"  "36"  "40"  "34"  "39"  "50"  "51"  "52" 
ccstrata.spring <- c("56") #off cape cod
nylistrata.spring <- c("RIN", "BIN", "1N", "5N", "1", "2", "3", "4", "5", "6", "7", "10", "11", "12") #nybight and mouth of long island sound
destrata.spring <- c("9N", "22", "23", "24", "26") #delaware bay
vastrata.spring <- c("12N", "13N", "65", "66") #chesapeake
ncstrata.spring <- c("14N", "62", "63", "64") #nc coast
strata.list.spring <- c(ccstrata.spring, nylistrata.spring, destrata.spring, vastrata.spring, ncstrata.spring)
# Save list as object
saveRDS(strata.list.spring, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/strata.list.spring.rds")

#----- Strata with biomass: Fall
unique(surveydata.chl.fall$Stratum)
 # [1] "53"  "52"  "50"  "51"  "15N" "44"  "62"  "64"  "61"  "41"  "14N" "63" 
# [13] "13N" "38"  "65"  "66"  "35"  "12N" "32"  "11N" "10N" "69"  "29"  "9N" 
# [25] "26"  "74"  "73"  "8N"  "23"  "4"   "7N"  "20"  "2"   "3"   "17"  "6N" 
# [37] "1"   "12"  "8"   "11"  "6"   "7"   "15"  "14"  "5N"  "10"  "13"  "4N" 
# [49] "3N"  "5"   "9"   "19"  "18"  "2N"  "46"  "16"  "1N"  "RIN" "BIN" "25" 
# [61] "45"  "24"  "56"  "59"  "60"  "28"  "22"  "30"  "27"  "37"  "36"  "40" 
# [73] "34"  "39" 
neamap.strata <- c("15N", "14N", "13N", "12N", "11N", "10N", "9N", "8N", "7N", "6N", "5N", "4N", "3N", "2N", "1N", "BIN", "RIN")
ccstrata.fall <- c("23", "24", "26", "56") #off cape cod, #440154
nylistrata.fall <- c("RIN", "BIN", "1N", "5N", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "12") #nybight and mouth of long island sound, #3b528b
destrata.fall <- c("9N", "22", "23", "24", "26", "69") #delaware bay, #21918c
vastrata.fall <- c("12N", "13N", "65", "66") #chesapeake, #5ec962
ncstrata.fall <- c("14N", "62", "63", "64") #nc coast, #fde725
strata.list.fall <- c(ccstrata.fall, nylistrata.fall, destrata.fall, vastrata.fall, ncstrata.fall)
# Save list as object
saveRDS(strata.list.fall, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/strata.list.fall.rds")


# #----- 1. GLM: Biomass (weight CPUE: kg/tow) ~ as.factor(Year) + Bottemp + Depth + Stratum, family="quasipoisson" ---------------------------------------------------

# # Only run for 1972-2021 dataset, Spring & Fall
glm1.list <- list()
for (name in names(data.list[1:2])) {
  glm1.list[[name = name]] = gam(Biomass ~ as.factor(Year) + Bottemp + Depth + Stratum, family="quasipoisson", data = data.list[[name]])
}
# gam.check(glm1.list[[1]])
# gam.check(glm1.list[[2]])
# formula(glm1.list[[1]])
# summary(glm1.list[[1]])
# summary(glm1.list[[2]])

# #----- GLM1 SPRING Predictions, for Selected Strata 
# preddata1.spring <- data.frame()
# for (i in unique(strata.list.spring)) {
	# new <- data.frame(Year = sort(unique(surveydata.spring$Year)),
						# Stratum = i,
						# Bottemp = median(surveydata.spring$Bottemp, na.rm=TRUE),
						# Depth = median(surveydata.spring$Depth, na.rm=TRUE))
	# preddata1.spring <- rbind(preddata1.spring, new)
# }

# pred.glm1.spring <- predict(glm1.list[[1]], se.fit=TRUE, newdata=preddata1.spring, type = "response")
# range(log(surveydata.spring$Biomass+1))
# range(pred.glm1.spring$fit)
# plot(preddata1.spring$Year,pred.glm1.spring$fit,type="b")

# #----- Create Dataset of Predictions from GLM1 spring
# predictions.glm1.spring <- cbind(preddata1.spring, data.frame(pred.glm1.spring))
# predictedmeans.glm1.spring <- data.frame(predictions.glm1.spring %>% group_by(Stratum) %>%
					# summarise(mean = mean(fit))) %>%
					# arrange(mean)


# #----- GLM1 FALL Predictions, for Selected Strata 
# preddata1.fall <- data.frame()
# for (i in unique(strata.list.fall)) {
	# new <- data.frame(Year = sort(unique(surveydata.fall$Year)),
						# Stratum = i,
						# Bottemp = median(surveydata.fall$Bottemp, na.rm=TRUE),
						# Depth = median(surveydata.fall$Depth, na.rm=TRUE))
	# preddata1.fall <- rbind(preddata1.fall, new)
# }

# pred.glm1.fall <- predict(glm1.list[[1]], se.fit=TRUE, newdata=preddata1.fall, type = "response")
# range(log(surveydata.fall$Biomass+1))
# range(pred.glm1.fall$fit)
# plot(preddata1.fall$Year,pred.glm1.fall$fit,type="b")

# #----- Create Dataset of Predictions from GLM1 fall
# predictions.glm1.fall <- cbind(preddata1.fall, data.frame(pred.glm1.fall))
# predictedmeans.glm1.fall <- data.frame(predictions.glm1.fall %>% group_by(Stratum) %>%
					# summarise(mean = mean(fit))) %>%
					# arrange(mean)



#----- 2. GAM of Biomass by as.factor(Year) + s(Bottemp) + s(Depth) + Stratum (gaussian(link="log")) -------------------------------------------------

#----- Run the model
# Only run for 1972-2021 dataset, Spring & Fall
gam2.list <- list()
for (name in names(data.list[1:2])) {
  gam2.list[[name = name]] = gam(Biomass ~ as.factor(Year) + s(Bottemp) + s(Depth) + Stratum, family ="quasipoisson"(link="log"), data = data.list[[name]])
}

# Save gam2.list as object
saveRDS(gam2.list, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/gam2.list.rds")

gam.check(gam2.list[[1]])
gam.check(gam2.list[[2]])

# flextable::as_flextable(gam2.list[[1]])
summary(gam2.list[[1]])
summary(gam2.list[[2]])

par(mfrow = c(2, 2))
plot(gam2.list[[1]], all.terms = TRUE)
par(mfrow = c(2, 2))
plot(gam2.list[[2]], all.terms = TRUE)


#----- GAM2 SPRING Predictions, for Selected Strata 
preddata2.spring <- data.frame()
for (i in unique(strata.list.spring)) {
	new <- data.frame(Year = sort(unique(surveydata.spring$Year)),
						Stratum = i,
						Bottemp = median(surveydata.spring$Bottemp[surveydata.spring$Stratum == i], na.rm=TRUE),
						Depth = median(surveydata.spring$Depth[surveydata.spring$Stratum == i], na.rm=TRUE))
	preddata2.spring <- rbind(preddata2.spring, new)
}

pred.gam2.spring <- predict(gam2.list[[1]], se.fit=TRUE, newdata=preddata2.spring, type = "response")
range(log(surveydata.spring$Biomass+1))
range(pred.gam2.spring$fit)
plot(preddata2.spring$Year,pred.gam2.spring$fit,type="b")  

#----- Create Dataset of Predictions from GAM2 spring
predictions.gam2.spring <- cbind(preddata2.spring, data.frame(pred.gam2.spring))
# Add region code to strata
predictions.gam2.spring <- predictions.gam2.spring %>%
	mutate(Region = case_when(
		Stratum == ccstrata.spring ~ "CapeCod",
		Stratum == nylistrata.spring ~ "NY-LongIs",
		Stratum == vastrata.spring ~ "Chesapeake",
		Stratum == ncstrata.spring ~ "NCcoast"))	
# Summarize means of strata
predictedmeans.gam2.spring <- data.frame(predictions.gam2.spring %>% group_by(Stratum) %>%
					summarise(mean = mean(fit))) %>%
					arrange(mean)


#----- GAM2 FALL Predictions, for Selected Strata 
preddata2.fall <- data.frame()
for (i in unique(strata.list.fall)) {
	new <- data.frame(Year = sort(unique(surveydata.fall$Year)),
						Stratum = i,
						Bottemp = median(surveydata.fall$Bottemp, na.rm=TRUE),
						Depth = median(surveydata.fall$Depth, na.rm=TRUE))
	preddata2.fall <- rbind(preddata2.fall, new)
}

pred.gam2.fall <- predict(gam2.list[[2]], se.fit=TRUE, newdata=preddata2.fall, type = "response")
range(log(surveydata.fall$Biomass+1))
range(pred.gam2.fall$fit)
plot(preddata2.fall$Year,pred.gam2.fall$fit,type="b")

#----- Create Dataset of Predictions from GAM2 fall
predictions.gam2.fall <- cbind(preddata2.fall, data.frame(pred.gam2.fall))
# Add region code to strata
predictions.gam2.fall <- predictions.gam2.fall %>%
	mutate(Region = case_when(
		Stratum == ccstrata.fall ~ "CapeCod",
		Stratum == nylistrata.fall ~ "NY-LongIs",
		Stratum == vastrata.fall ~ "Chesapeake",
		Stratum == ncstrata.fall ~ "NCcoast"))	
# Summarize means of strata
predictedmeans.gam2.fall <- data.frame(predictions.gam2.fall %>% group_by(Stratum) %>%
					summarise(mean = mean(fit))) %>%ion
					arrange(mean)


#----- GAM2 Predicted Means Summary of Strata
predictedmeans.gam2.spring
predictedmeans.gam2.fall


#----- View tables and figures of results

# Can get nice tables via flextable() & modelsummary()
flextable <- flextable::as_flextable(gam.list[[1]])
modelsummary::modelsummary(gam.list[[1]])



#----- 2b. GAM of Biomass by s(Year) + s(Bottemp) + s(Depth) + Stratum (gaussian(link="log")) --------------------------------------------------------

#----- Run the model
# Only run for 1972-2021 dataset, Spring & Fall
gam2b.list <- list()
for (name in names(data.list[1:2])) {
  gam2b.list[[name = name]] = gam(Biomass ~ s(Year) + s(Bottemp) + s(Depth) + Stratum, family ="quasipoisson"(link="log"), data = data.list[[name]])
}

gam.check(gam2b.list[[1]])
gam.check(gam2b.list[[2]])

# Save gam2b.list as object
saveRDS(gam2b.list, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/gam2b.list.rds")


# flextable::as_flextable(gam2.list[[1]])
summary(gam2b.list[[1]])
summary(gam2b.list[[2]])

#----- GAM2 SPRING Predictions, for Selected Strata 
preddata2.spring <- data.frame()
for (i in unique(strata.list.spring)) {
	new <- data.frame(Year = sort(unique(surveydata.spring$Year)),
						Stratum = i,
						Bottemp = median(surveydata.spring$Bottemp, na.rm=TRUE),
						Depth = median(surveydata.spring$Depth, na.rm=TRUE))
	preddata2.spring <- rbind(preddata2.spring, new)
}

pred.gam2.spring <- predict(gam2.list[[1]], se.fit=TRUE, newdata=preddata2.spring, type = "response")
range(log(surveydata.spring$Biomass+1))
range(pred.gam2.spring$fit)
plot(preddata2.spring$Year,pred.gam2.spring$fit,type="b")  




#----- 3. GAM of Biomass by s(Year) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") ---------------------------------------------------------------

# TEMP HERE, but adding a State code to the data to run this model. Ideally, later it will go in data creation.
# Dummy it up for now and use LatCat to get a rough approximation of states  
surveydata.spring.2 <- surveydata.spring %>%
mutate(across(c("LatCat", "LonCat"), round, 0)) %>%
    mutate(
      State = case_when(
        LatCat >= 41 ~ "RI&MA",
        LatCat == 40 ~ "NYBLI",
        LatCat == 39 ~ "NJ",
        LatCat == 38 ~ "DEMD",
        LatCat == 37 ~ "VANC",
        LatCat == 36 ~ "VANC",
        LatCat <= 35 ~ "NCGA"
      )
    )
    
surveydata.fall.2 <- surveydata.fall %>%
mutate(across(c("LatCat", "LonCat"), round, 0)) %>%
    mutate(
      State = case_when(
        LatCat >= 41 ~ "RI&MA",
        LatCat == 40 ~ "NYBLI",
        LatCat == 39 ~ "NJ",
        LatCat == 38 ~ "DEMD",
        LatCat == 37 ~ "VANC",
        LatCat == 36 ~ "VANC",
        LatCat <= 35 ~ "NCGA"
      )
    )
data.list.2 <- list(all.spring = surveydata.spring.2, all.fall = surveydata.fall.2)
# Save list as object
saveRDS(data.list.2, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/federal.data.list.2.rds")   

# Max Biomass by State
mean.spring <- surveydata.spring.2 %>%
  group_by(State) %>%
  summarise(AveBiomass = mean(Biomass)) %>%
  arrange(desc(AveBiomass))
#write.csv(mean.spring, "/Users/janellemorano/Git/AtlanticMenhaden/figs-maps/table-mean-biomass-by-strata-spring.csv")
mean.fall <- surveydata.fall.2 %>%
  group_by(State) %>%
  summarise(AveBiomass = mean(Biomass)) %>%
  arrange(desc(AveBiomass))


#----- Run the model
############### FIRST RUN WITHOUT STATE BEING MADE A FACTOR WILL NEED TO RERUN ####################
# Only run for 1972-2021 dataset, Spring & Fall
gam3.list <- list()
for (name in names(data.list.2[1:2])) {
  gam3.list[[name = name]] = gam(Biomass ~ s(Year) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log"), data = data.list.2[[name]])
}

gam.check(gam3.list[[1]])
gam.check(gam3.list[[2]])

# Save gam3.list as object
saveRDS(gam3.list, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/gam3.list.rds")

summary(gam3.list[[1]])
summary(gam3.list[[2]])

par(mfrow = c(2, 2))
plot(gam3.list[[1]], all.terms = TRUE)
par(mfrow = c(2, 2))
plot(gam3.list[[2]], all.terms = TRUE)


#----- GAM3 SPRING Predictions 
preddata3.spring <- data.frame()
for (i in unique(surveydata.spring.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.spring.2$Year)),
						State = i,
						Bottemp = median(surveydata.spring.2$Bottemp[surveydata.spring.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.spring.2$Depth[surveydata.spring.2$State == i], na.rm=TRUE))
	preddata3.spring <- rbind(preddata3.spring, new)
}

pred.gam3.spring <- predict(gam3.list[[1]], se.fit=TRUE, newdata=preddata3.spring, type = "response")
range(log(surveydata.spring.2$Biomass+1))
range(pred.gam3.spring$fit)
plot(preddata3.spring$Year,pred.gam3.spring$fit,type="b")  

# Create Dataset of Predictions from GAM3
predictions.gam3.spring <- cbind(preddata3.spring, data.frame(pred.gam3.spring))
# Save predictions.gam3.spring as object
write.csv(predictions.gam3.spring, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam3.spring.csv", row.names = TRUE)

#----- GAM3 FALL Predictions 
preddata3.fall <- data.frame()
for (i in unique(surveydata.fall.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.fall.2$Year)),
						State = i,
						Bottemp = median(surveydata.fall.2$Bottemp[surveydata.fall.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.fall.2$Depth[surveydata.fall.2$State == i], na.rm=TRUE))
	preddata3.fall <- rbind(preddata3.fall, new)
}

pred.gam3.fall <- predict(gam3.list[[2]], se.fit=TRUE, newdata=preddata3.fall, type = "response")
range(log(surveydata.fall.2$Biomass+1))
range(pred.gam3.fall$fit)
plot(preddata3.fall$Year,pred.gam3.fall$fit,type="b")  

# Create Dataset of Predictions from GAM3
predictions.gam3.fall <- cbind(preddata3.fall, data.frame(pred.gam3.fall))
# Save predictions.gam3.fall as csv
write.csv(predictions.gam3.fall, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam3.fall.csv", row.names = TRUE)



#----- 4. GAM of Biomass by s(Year, by = State) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") ---------------------------------------------------------------
#----- Run the model
# Only run for 1972-2021 dataset, Spring & Fall
gam4.list <- list()
for (name in names(data.list.2[1:2])) {
  gam4.list[[name = name]] = gam(Biomass ~ s(Year, by = as.factor(State)) + s(Bottemp) + s(Depth) + as.factor(State), family ="quasipoisson"(link="log"), data = data.list.2[[name]])
}

gam.check(gam4.list[[1]])
gam.check(gam4.list[[2]])

# Save gam4.list as object
saveRDS(gam4.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/gam4.list.rds")

summary(gam4.list[[1]])
summary(gam4.list[[2]])

par(mfrow = c(2, 2))
plot(gam4.list[[1]], all.terms = TRUE)
par(mfrow = c(2, 2))
plot(gam4.list[[2]], all.terms = TRUE)


#----- GAM4 SPRING Predictions 
preddata4.spring <- data.frame()
for (i in unique(surveydata.spring.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.spring.2$Year)),
						State = i,
						Bottemp = median(surveydata.spring.2$Bottemp[surveydata.spring.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.spring.2$Depth[surveydata.spring.2$State == i], na.rm=TRUE))
	preddata4.spring <- rbind(preddata4.spring, new)
}

pred.gam4.spring <- predict(gam4.list[[1]], se.fit=TRUE, newdata=preddata4.spring, type = "response")
range(log(surveydata.spring.2$Biomass+1))
range(pred.gam4.spring$fit)
plot(preddata4.spring$Year,pred.gam4.spring$fit,type="b")  

# Create Dataset of Predictions from GAM4
predictions.gam4.spring <- cbind(preddata4.spring, data.frame(pred.gam4.spring))
# Save predictions.gam4.spring as object
write.csv(predictions.gam4.spring, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4.spring.csv", row.names = TRUE)

#----- GAM4 FALL Predictions 
preddata4.fall <- data.frame()
for (i in unique(surveydata.fall.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.fall.2$Year)),
						State = i,
						Bottemp = median(surveydata.fall.2$Bottemp[surveydata.fall.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.fall.2$Depth[surveydata.fall.2$State == i], na.rm=TRUE))
	preddata4.fall <- rbind(preddata4.fall, new)
}

pred.gam4.fall <- predict(gam4.list[[2]], se.fit=TRUE, newdata=preddata4.fall, type = "response")
range(log(surveydata.fall.2$Biomass+1))
range(pred.gam4.fall$fit)
plot(preddata4.fall$Year,pred.gam4.fall$fit,type="b")  

# Create Dataset of Predictions from GAM4
predictions.gam4.fall <- cbind(preddata4.fall, data.frame(pred.gam4.fall))
# Save predictions.gam4.fall as csv
write.csv(predictions.gam4.fall, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4.fall.csv", row.names = TRUE)




#----- 4b. GAM of Biomass by s(Year, by = State) + s(Bottemp) + s(Depth) + State, family ="quasipoisson"(link="log") WITH REML---------------------------------------------------------------
#----- Run the model
# Only run for 1972-2021 dataset, Spring & Fall
# TRY WITH REML
gam4b.list <- list()
for (name in names(data.list.2[1:2])) {
  gam4b.list[[name = name]] = gam(Biomass ~ s(Year, by = as.factor(State)) + s(Bottemp) + s(Depth) + as.factor(State), family ="quasipoisson"(link="log"), method = "REML", data = data.list.2[[name]])
}
gam.check(gam4b.list[[1]])
plot(gam4b.list[[1]])
gam.check(gam4b.list[[2]])
plot(gam4b.list[[2]])

#----- GAM4b SPRING Predictions 
preddata4b.spring <- data.frame()
for (i in unique(surveydata.spring.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.spring.2$Year)),
						State = i,
						Bottemp = median(surveydata.spring.2$Bottemp[surveydata.spring.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.spring.2$Depth[surveydata.spring.2$State == i], na.rm=TRUE))
	preddata4b.spring <- rbind(preddata4b.spring, new)
}

pred.gam4b.spring <- predict(gam4b.list[[1]], se.fit=TRUE, newdata=preddata4b.spring, type = "response")
range(log(surveydata.spring.2$Biomass+1))
range(pred.gam4b.spring$fit)
plot(preddata4b.spring$Year,pred.gam4b.spring$fit,type="b")  

# Create Dataset of Predictions from GAM4b
predictions.gam4b.spring <- cbind(preddata4b.spring, data.frame(pred.gam4b.spring))
# Save predictions.gam4b.spring as object
write.csv(predictions.gam4b.spring, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4b.spring.csv", row.names = TRUE)

#----- GAM4b FALL Predictions 
preddata4b.fall <- data.frame()
for (i in unique(surveydata.fall.2$State)) {
	new <- data.frame(Year = sort(unique(surveydata.fall.2$Year)),
						State = i,
						Bottemp = median(surveydata.fall.2$Bottemp[surveydata.fall.2$State == i], na.rm=TRUE),
						Depth = median(surveydata.fall.2$Depth[surveydata.fall.2$State == i], na.rm=TRUE))
	preddata4b.fall <- rbind(preddata4b.fall, new)
}

pred.gam4b.fall <- predict(gam4b.list[[2]], se.fit=TRUE, newdata=preddata4b.fall, type = "response")
range(log(surveydata.fall.2$Biomass+1))
range(pred.gam4b.fall$fit)
plot(preddata4b.fall$Year,pred.gam4b.fall$fit,type="b")  

# Create Dataset of Predictions from GAM4
predictions.gam4b.fall <- cbind(preddata4b.fall, data.frame(pred.gam4b.fall))
# Save predictions.gam4.fall as csv
write.csv(predictions.gam4b.fall, "/Users/janellemorano/Git/AtlanticMenhaden/3-model-output/GAMoutput/predictions.gam4b.fall.csv", row.names = TRUE)





#-----------------------------------------------------------------------
#----- Z. AIC of models --------------------
# SPRING
AIC(gam2.list[[1]], gam2b.list[[1]], gam3.list[[1]], gam4.list[[1]])
# FALL
AIC(gam2.list[[2]], gam2b.list[[2]], gam3.list[[2]], gam4.list[[2]])







#-----------------------------------------------------------------------------------------------------
# EXTRA CODE NOT CURRENTLY BEING USED BUT KEEP FOR NOW IN CASE USEFUL
#-----------------------------------------------------------------------------------------------------


# #----- Store results
# # Get the summaries using `lapply
# summary_list <- lapply(gam.list, summary)
# # extract the coefficients from these summaries
# p.table_list <- lapply(summary_list, `[[`, 'p.table')
# s.table_list <- lapply(summary_list, `[[`, 's.table')

# # Get Significant Coefficient Names
# sig.coef.names.list <- list()
# for (name in names(p.table_list)) {
  # p <- data.frame(p.table_list[[name]])
  # p <- p %>% filter(Pr...t.. < 0.05)
  # sig.coef.names.list[[name = name]] <- row.names(p)
# }


#-------- Need to assess if this code is relevant and can be incorporated above -----------------------------
# preddata <- data.frame(Year = surveydata.spring$Year,
#                        Stratum = surveydata.spring$Stratum,
#                        Bottemp = surveydata.spring$Bottemp,
#                        Depth = surveydata.spring$Depth)
# pred.spring.gam1a <- predict.gam(gam1a.list[[1]], newdata = preddata, se.fit=TRUE)
# head(pred.spring.gam1a)
# 
# plot(surveydata.spring$Biomass ~ surveydata.spring$Year, ylim=c(0,50))
# lines(preddata$Year, pred.spring.gam1a$fit, col="red")
# lines(preddata$Year, pred.spring.gam$fit+2*pred.spring.gam$se.fit,lty=2, col="red")
# lines(preddata$Year, pred.spring.gam$fit-2*pred.spring.gam$se.fit,lty=2, col="red")



# # Add coefficients from regression above onto figure
# coeff <- coefficients(abund.lm)
# # Equation of the line : 
# eq = paste0("y = ", round(coeff[2],1), "*x + ", round(coeff[1],1))
# # Plot
# ggplot(surveydata, aes(x=Year, y=log(Abundance))) +
#   geom_point(shape=19, color="blue") +
#   geom_abline(intercept = -3.56, slope = 0.0018075, color = "red", size = 1.5) +
#   ggtitle(eq) +
#   theme_bw()

# # Alternatively, use geom_smooth
# # Spring
# ggplot(surv.spring, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(color="#22A884FF") +
#   geom_smooth(method=lm)
# 
# # Fall
# ggplot(surv.fall, aes(x=Year, y=log(Abundance +1))) +
#   geom_point(color="#414487FF") +
#   geom_smooth(method=lm)

#### GAM of log(Abundance) by Year
# library(mgcv)
# 
# #Spring
# abun.gam.spring = gam(log(Abundance +1) ~ s(Year), data = surv.spring)
# summary(abun.gam.spring)
# plot(abun.gam.spring)
# 
# preddata <- data.frame(Year = surv.spring$Year) #,
# # Bottemp = surv.spring$Bottemp,
# # Depth = surv.spring$Depth,
# # Lat = surv.spring$Lat)
# pred.abun.gam.spring <- predict.gam(abun.gam.spring, newdata = preddata, type="response", se.fit=TRUE)
# head(pred.abun.gam.spring)
# 
# plot(log(surv.spring$Abundance +1) ~ surv.spring$Year)
# lines(preddata$Year, pred.abun.gam.spring$fit, col="red")
# lines(preddata$Year, pred.abun.gam.spring$fit+2*pred.abun.gam.spring$se.fit,lty=2, col="red")
# lines(preddata$Year, pred.abun.gam.spring$fit-2*pred.abun.gam.spring$se.fit,lty=2, col="red")
# #---------------------------------------------------------
# 
# 
# 
# 
# #----- GAM2: Biomass ~ s(Year) + Stratum + Bottemp + Depth + Chlorophyll --------------------------------
# 
# #----- Run the model
# # Only run for 2012-2021 dataset, Spring & Fall
# gam3.list <- list()
# for (name in names(data.list[3:4])) {
#   gam3.list[[name = name]] = gam(Biomass ~ s(Year) + Stratum + Bottemp + Depth + Avechlor, data = data.list[[name]])
# }
# 
# # Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
# # A term has fewer unique covariate combinations than specified maximum degrees of freedom
# 
