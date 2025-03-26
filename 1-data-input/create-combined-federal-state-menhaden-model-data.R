# create-combined-federal-state-menhaden-model-data.R
######################################
# Janelle L. Morano

# Combines data from federal and state survey data previously collated ("combined-catch-envtl-20240616.csv" and "statesurvey_menhaden_data_20240726.csv") into a single dataset for use in distribution modeling

# last updated 20 March 2025
# ***Need to go back and add Chlorophyll-a data

###############################################
###############################################

library(tidyverse)
library(mgcv)


###########################################################################################
#----- Federal surveys --------------------------------------------------
###########################################################################################

# Atlantic menhaden data from NEFSC and NEAMAP surveys
federal <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-catch-envtl-20240819.csv", header = TRUE)
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
  filter(Year >=1972) %>%
  arrange(Year)
federal.fall <- federal %>%
  filter(Season == "FALL") %>%
  filter(Year >=1972)%>%
  arrange(Year)



###########################################################################################
#----- State surveys --------------------------------------------------
###########################################################################################

statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240726.csv", header = TRUE)

sapply(statedata, function(x) sum(is.na(x)))
# SEAMAP doesn't have any depth, and so would be completely lost
# Ignore depth to maximize spatial coverage
statedata <- statedata %>%
  filter_at(vars(MenhadenTotal, Presence), all_vars(!is.na(.)))


#----- Create Spring and Fall datasets
state.spring <- statedata |>
  filter(Month > 2 & Month <5) |>
  filter(Year >=1972) |>
  arrange(Year)
state.fall <- statedata |>
  filter(Month > 8 & Month < 11) |>
  filter(Year >=1972) |>
  arrange(Year)


###########################################################################################
#----- Combine federal and state --------------------------------------------------
###########################################################################################
federal.spring2 <- federal.spring %>%
  select(Survey, Season, Year, Bottemp, Presence, State, Inoffshore) %>%
  rename(WaterTemp = Bottemp)
federal.fall2 <- federal.fall %>%
  select(Survey, Season, Year, Bottemp, Presence, State, Inoffshore) %>%
  rename(WaterTemp = Bottemp)


state.spring2 <- state.spring %>%
  select(Survey, Season, Year, SurfTemp, Presence, State) %>%
  rename(WaterTemp = SurfTemp) %>%
  mutate(Inoffshore = "coastal")
state.fall2 <- state.fall %>%
  select(Survey, Season, Year, SurfTemp, Presence, State) %>%
  rename(WaterTemp = SurfTemp) %>%
  mutate(Inoffshore = "coastal")

# Combined federal and state data
alldata.spring <- rbind(federal.spring2, state.spring2)
alldata.fall <- rbind(federal.fall2, state.fall2)

# Convert Survey, State, and Inoffshore to factor
cvt <- c("Survey", "State", "Inoffshore")
alldata.spring[cvt] <- lapply(alldata.spring[cvt], factor)
alldata.fall[cvt] <- lapply(alldata.fall[cvt], factor)

alldata.spring$Year <- as.numeric(alldata.spring$Year)
alldata.fall$Year <- as.numeric(alldata.fall$Year)

# # ----- Create a list of the datasets
data.list <- list(alldata.spring = alldata.spring, alldata.fall = alldata.fall)

#----- Save list as object
saveRDS(data.list, "/Users/janellemorano/Git/menhaden-dist-ms/data/data.list.rds")
