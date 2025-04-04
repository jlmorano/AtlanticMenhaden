# create-combined-federal-state-menhaden-model-data.R
######################################
# Janelle L. Morano

# Combines data from federal and state survey data previously collated ("combined-catch-envtl-20240616.csv" and "statesurvey_menhaden_data_20250402.csv") into a single dataset (1972-2023) for use in distribution modeling of Atlantic menhaden presence in GAM in manuscript

# last updated 2 April 2025

###############################################
###############################################

library(tidyverse)
library(mgcv)


###########################################################################################
#----- Federal surveys --------------------------------------------------
###########################################################################################

# Atlantic menhaden data from NEFSC and NEAMAP surveys
federal <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20250326.csv", header = TRUE)
# Add Presence/Absence column numeric
federal$Presence <- as.numeric(federal$Presence)

# Remove NAs where there are no samples of menhaden
sapply(federal, function(x) sum(is.na(x)))
federal <- federal %>%
  # filter_at(vars(Depth, Bottemp, Abundance, Presence), all_vars(!is.na(.)))
  filter_at(vars(Abundance, Presence), all_vars(!is.na(.)))

# Create Spring and Fall datasets, from 1972-2023
federal.spring <- federal %>%
  filter(Season == "SPRING") %>%
  filter(Year >=1972 & Year <=2023) %>%
  arrange(Year)
federal.fall <- federal %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972 & Year <=2023)%>%
  arrange(Year)



###########################################################################################
#----- State surveys --------------------------------------------------
###########################################################################################

statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20250402.csv", header = TRUE)

sapply(statedata, function(x) sum(is.na(x)))
# SEAMAP doesn't have any depth, and so would be completely lost
# Ignore depth to maximize spatial coverage
statedata <- statedata %>%
  filter_at(vars(MenhadenTotal, Presence), all_vars(!is.na(.)))


#----- Create Spring and Fall datasets
state.spring <- statedata |>
  filter(Month > 2 & Month <5) |>
  filter(Year >=1972 & Year <=2023) |>
  arrange(Year)
state.fall <- statedata |>
  filter(Month > 8 & Month < 11) |>
  filter(Year >=1972 & Year <=2023) |>
  arrange(Year)


###########################################################################################
#----- Combine federal and state --------------------------------------------------
###########################################################################################
federal.spring2 <- federal.spring %>%
  select(Survey, Season, Year, Depth, Bottemp, Biomass, Presence, State, Inoffshore) %>%
  rename(WaterTemp = Bottemp)
federal.fall2 <- federal.fall %>%
  select(Survey, Season, Year, Depth, Bottemp, Biomass, Presence, State, Inoffshore) %>%
  rename(WaterTemp = Bottemp)


state.spring2 <- state.spring %>%
  select(Survey, Season, Year, Depth.m, BotTemp, Weight.kg, Presence, State) %>%
  rename(Depth = Depth.m) |>
  rename(WaterTemp = BotTemp) %>%
  rename(Biomass = Weight.kg) |>
  mutate(Inoffshore = "coastal")
state.fall2 <- state.fall %>%
  select(Survey, Season, Year, Depth.m, BotTemp, Weight.kg, Presence, State) %>%
  rename(Depth = Depth.m) |>
  rename(WaterTemp = BotTemp) %>%
  rename(Biomass = Weight.kg) |>
  mutate(Inoffshore = "coastal")

# Combined federal and state data
alldata.spring <- rbind(federal.spring2, state.spring2)
alldata.fall <- rbind(federal.fall2, state.fall2)

# Convert State to factor with ordered levels from North to South
alldata.spring$State <- factor(alldata.spring$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
alldata.fall$State <- factor(alldata.fall$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Convert Survey and Inoffshore to factor
cvt <- c("Survey", "Inoffshore")
alldata.spring[cvt] <- lapply(alldata.spring[cvt], factor)
alldata.fall[cvt] <- lapply(alldata.fall[cvt], factor)

# Convert Year to numeric
alldata.spring$Year <- as.numeric(alldata.spring$Year)
alldata.fall$Year <- as.numeric(alldata.fall$Year)


# # ----- Create a list of the datasets
gam.data.list <- list(alldata.spring = alldata.spring, alldata.fall = alldata.fall)

#----- Save list as object
# saveRDS(gam.data.list, "/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
