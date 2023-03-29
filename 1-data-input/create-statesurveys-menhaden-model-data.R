# create-statesurveys-menhaden-model-data.R
######################################
# Janelle L. Morano

# Collate state data of surveys with menhaden:
# - CTLISTS data
# - NYDEC Western Long Island Sound survey data
# - NJ Ocean Trawl
# - Delaware Bay
# - (not yet) Maryland Gill Net (lacks lat/long info)
# - ChesMMAP
# - (not yet) VIMS Shad Gill net
# - Georgia EMTS

# last updated 29 March 2023
###############################################
###############################################

library(tidyverse)
library(janitor)

##### Load State Data -------------------------------------------------

# Need survey, stratum, season, month, day, year, depth, lat, lon, salinity, temp, species, count, weight


#---- CT Long Island Sound Trawl Survey (CTLISTS) -----

ct <- read.csv("/Users/janellemorano/DATA/CT Long Island Sound survey/CTLISTS_menhaden.csv", header = TRUE)
# colnames(ct)
# Add column with Month as numeric THERE IS A SPACE AFTER THE MONTH NAMES!
ct <- ct %>%
  rename(MonthNm = Month) %>%
  mutate(Month = case_when(MonthNm == "MAY " ~ 5, 
                           MonthNm == "JUN " ~ 6,
                           MonthNm == "APR " ~ 4,
                           MonthNm == "SEP " ~ 9,
                           MonthNm == "OCT " ~ 10,
                           MonthNm == "SOC " ~ 9)) #pretty sure SOC is supposed to be SEP

ct2 <- ct %>%
  select(Survey, Season, Month, Day, Year, Latitude, Longitude, Depth.m, SurfSalin, SurfTemp, BotSalin, BotTemp, TotalCount, Weight.kg) %>%
  rename(MenhadenTotal = TotalCount) %>%
  add_column(Stratum = NA, .after = "Survey")



#---- NY Western Long Island Seine Survey -----

wli <- read.csv("/Users/janellemorano/DATA/NY Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)
# colnames(wli)
wli <- clean_names(wli, "upper_camel")
# colnames(wli)
# Water temp = H2O

wli2<- wli %>%
  select(Month, Day, Year, Latitude, Longitutde, H2O, MenhadenTotal) %>%
  rename(Longitude = Longitutde, 
         SurfTemp = H2O) %>%
  add_column(Survey = "WLI", .before = "Month") %>%
  add_column(Stratum = NA, .after = "Survey") %>%
  add_column(Season = NA, .before = "Month") %>%
  add_column(Depth.m = NA, .after = "Longitude") %>%
  add_column(SurfSalin = NA, .after = "Depth.m") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin") %>%
  add_column(Weight.kg = NA, .after = "MenhadenTotal")


#---- NJ Ocean Trawl Survey -----

nj <- read.csv("/Users/janellemorano/DATA/NJ Ocean Trawl Survey/NJOTMenhadenCatch.csv", header = TRUE)
colnames(nj)
#convert lat & lon columns to decimal degrees
nj <- mutate(nj, Latitude = ELAT/100) %>%
  mutate(nj, Longitude = ELONG/100)

nj2 <- nj %>%
  select(STRATUM, Month, STA, Year, Latitude, Longitude, ENDDEPTH, SALSURF, TEMPSURF, SALBOT, TEMPBOT, NUMBER, WEIGHT) %>%
  rename(Stratum = STRATUM, 
         Day = STA,
         Depth.m = ENDDEPTH,
         SurfSalin = SALSURF,
         SurfTemp = TEMPSURF,
         BotSalin = SALBOT,
         BotTemp = TEMPBOT,
         MenhadenTotal = NUMBER,
         Weight.kg = WEIGHT) %>%
  add_column(Survey = "NJOT", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum")



#---- Delaware Bay ------

de <- read.csv("/Users/janellemorano/DATA/Delaware Bay Adult Trawl/DelawareBay-menhaden.csv", header = TRUE)
colnames(de)

de2 <- de %>%
  select(MON1, DAY1, Year, EndLat, EndLon, DEPTHm, SAL_SUR, TEMP_SUR, SAL_BOT, TEMP_BOT, NUMBER, WEIGHTkg) %>%
  rename(Month = MON1,
         Day = DAY1,
         Latitude = EndLat,
         Longitude = EndLon,
         Depth.m = DEPTHm,
         SurfSalin = SAL_SUR,
         SurfTemp = TEMP_SUR,
         BotSalin = SAL_BOT,
         BotTemp = TEMP_BOT,
         MenhadenTotal = NUMBER,
         Weight.kg = WEIGHTkg) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "DEBay", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum")



#---- Maryland Gill Net (Upper Chesapeake Bay and Potomac River) -----

# md <- read.csv("/Users/janellemorano/DATA/MD Gill Net/MD_SprGN_8521_AG.csv", header = TRUE)
# colnames(md)

# Lacks lat/long or other specific identifying location info. Need to investigate.



#---- ChesMMAP -----

chesmap <- read.csv("/Users/janellemorano/DATA/ChesMMAP/CMMenhaden.csv", header = TRUE)
colnames(chesmap)

# fix Date 
library(lubridate)
chesmap$Date <- mdy(chesmap$Date)
chesmap$Day <- day(chesmap$Date)

# convert Depth_ft to meters
chesmap <- mutate(chesmap, Depth.m = Depth_ft/3.281 )

chesmap2 <- chesmap %>%
  select(Month, Day, Year, LATITUDE_START, LONGITUDE_START, Depth.m, Salinity_psu, Temperature_deg_C, Count, Biomass_kg) %>%
  rename(Latitude = LATITUDE_START,
         Longitude = LONGITUDE_START,
         SurfSalin = Salinity_psu,
         SurfTemp = Temperature_deg_C,
         MenhadenTotal = Count,
         Weight.kg = Biomass_kg) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "ChesMMAP", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin")



#---- VIMS Staked Gill Net -----

# vims <- read.csv(" ", header = TRUE)
# colnames(vims)



#---- Georgia EMTS -----

ga <- read.csv("/Users/janellemorano/DATA/GA EMTS/Menhaden GA EMTS.csv", header = TRUE)
colnames(ga)

# library(lubridate)
ga$TowDate <- mdy(ga$TowDate)
ga$Month <- month(ga$TowDate)
ga$Day <- day(ga$TowDate)
ga$Year <- year(ga$TowDate)

# convert Depth_ft to meters
ga <- mutate(ga, Depth.m = Depth..Ft../3.281 )


ga2 <- ga %>%
  select(Month, Day, Year, LatBeg, LonBeg, Depth.m, Salinity..ppt., Wtemp..Deg..C., TotNum, TotWt) %>%
  rename(Latitude = LatBeg,
         Longitude = LonBeg,
         SurfSalin = Salinity..ppt.,
         SurfTemp = Wtemp..Deg..C.,
         MenhadenTotal = TotNum,
         Weight.kg = TotWt) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "GAEMTS", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin")


##### Merge datasets ------------------------------------------------------

# Verify colnames are the same
colnames(ct2)
colnames(wli2)


# Merge state data
statedata <- bind_rows(ct2, wli2, nj2, de2, chesmap2, ga2)
unique(statedata$Survey)
# "CTLISTS" "WLI"     "NJOT"    "DEBay"   ChesMMAP        "GAEMTS" 
unique(statedata$Month)

# Write dataset as .csv file
#### THIS WILL OVERWRITE!!
# write.csv(statedata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data.csv", row.names = TRUE)
