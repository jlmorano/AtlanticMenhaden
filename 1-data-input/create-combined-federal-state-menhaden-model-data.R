# create-combined-allsurveys-menhaden-model-data.R
######################################
# Janelle L. Morano

# Combines data from NEFSC & NEAMAP surveys and state survey data previously collated ("statesurvey_menhaden_data_20260414.csv" and "nefsc-neamap-menhaden-data-20260414.csv") into a single dataset (1972-2023) for use in distribution modeling of Atlantic menhaden presence in GAM in manuscript

# last updated 16 April 2026 (dataset remains 20260414)

###############################################
###############################################

library(tidyverse)
library(mgcv)


###########################################################################################
#----- Federal surveys --------------------------------------------------
###########################################################################################

# Atlantic menhaden data from NEFSC and NEAMAP surveys
federal <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/nefsc-neamap-menhaden-data-20260414.csv", header = TRUE)
# Add Presence/Absence column numeric
federal$Presence <- as.numeric(federal$Presence)

# Remove rows with NAs for Abundance and Presence of menhaden
sapply(federal, function(x) sum(is.na(x)))
federal <- federal %>%
  # filter_at(vars(Depth, Bottemp, Abundance, Presence), all_vars(!is.na(.)))
  filter_at(vars(Abundance, Presence), all_vars(!is.na(.)))

colnames(federal)
# "X"           "Survey"      "Cruise"      "Station"     "Stratum"     "Inoffshore"  "State"       "Year"       
# "Season"      "Latitude"    "Longitude"   "Areasw"      "Depth"       "Surftemp"    "Surfsalin"   "Bottemp"    
# "Botsalin"    "Abundance"   "Biomass"     "Presence"    "CentroidLat" "CentroidLon"

# Select choice columns and modify names to match statedata
federal2 <- federal |>
  select(Survey, Season, Year, Depth, Bottemp, Botsalin, Surftemp, Surfsalin, Abundance, Biomass, Presence, State, Inoffshore) |>
  rename(MenhadenTotal = Abundance,
         Weight.kg = Biomass,
         BotTemp = Bottemp,
         BotSalin = Botsalin,
         SurfTemp = Surftemp,
         SurfSalin = Surfsalin) |>
  mutate(Month = case_when(Season == "SPRING" ~ 3.5, 
                               Season == "FALL" ~ 9.5),
                               .after = "Year")
    

  

###########################################################################################
#----- State surveys --------------------------------------------------
###########################################################################################

statedata <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey-menhaden-data-20260414.csv", header = TRUE)

sapply(statedata, function(x) sum(is.na(x)))
# SEAMAP doesn't have any depth recorded. To add an approximate value to keep the data, samples were taken between 3 and 10 meters, so add 6.5
statedata$Depth.m[is.na(statedata$Depth.m) & statedata$Survey == "SEAMAP"] <- 6.5

# Remove any samples without menhaden (NAs)
statedata <- statedata %>%
  filter_at(vars(MenhadenTotal, Presence), all_vars(!is.na(.)))

colnames(statedata)
# "X"             "Survey"        "Stratum"       "Season"        "Month"         "Day"           "Year"         
# "Latitude"      "Longitude"     "Depth.m"       "SurfSalin"     "SurfTemp"      "BotSalin"      "BotTemp"      
# "MenhadenTotal" "Weight.kg"     "Presence"      "State" 

statedata2 <- statedata %>%
  select(Survey, Season, Year, Month, Depth.m, BotTemp, BotSalin, SurfTemp, SurfSalin, MenhadenTotal, Weight.kg, Presence, State) %>%
  rename(Depth = Depth.m) |>
  mutate(Inoffshore = case_when(Survey == "NJOT" | Survey == "SEAMAP" ~ "nearshore",
                                TRUE ~ "inshore"))



###########################################################################################
#----- Combine federal and state --------------------------------------------------
###########################################################################################

alldata <- rbind(federal2, statedata2)

# Convert State to factor with ordered levels from North to South
alldata$State <- factor(alldata$State, levels = c("MENH", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Convert Survey and Inoffshore to factor
cvt <- c("Survey", "Inoffshore")
alldata[cvt] <- lapply(alldata[cvt], factor)

# Convert Year to numeric
alldata$Year <- as.numeric(alldata$Year)

# Save dataset
# write.csv(alldata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/combined-allsurveys-menhaden-data-20260414.csv", row.names = TRUE)




###########################################################################################
#----- Create Spring and Fall datasets for modeling ---------------------------------------
###########################################################################################

alldata.spring <- alldata |>
  filter(Year >=1972 & Year <=2023) |>
  filter(Season == "SPRING") |>
  arrange(Year)
alldata.fall <- alldata |>
  filter(Year >=1972 & Year <=2023) |>
  filter(Season == "FALL") |>
  arrange(Year)





# # ----- Create a list of the datasets
gam.data.list <- list(alldata.spring = alldata.spring, alldata.fall = alldata.fall)

#----- Save list as object
# saveRDS(gam.data.list, "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/menhaden.data.list.rds")

