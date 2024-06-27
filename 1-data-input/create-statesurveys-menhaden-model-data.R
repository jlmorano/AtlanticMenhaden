# create-statesurveys-menhaden-model-data.R
######################################
# Janelle L. Morano

# Collate state data of surveys with menhaden:
# - CTLISTS data (Northern Adult Index)
# - Delaware Bay (Northern Adult Index, 2 datasets: 30' and 16' trawls, only incorporated 30' into final statedata)
# - Georgia EMTS (Southern Adult Index)
# - Maryland Gill Net (1985-2021 currently not in the dataset) (Mid-Atlantic Adult Index)
# - NJ Ocean Trawl (Northern Adult Index)
# - (not yet, and I think omit) VIMS Shad Gill net (Mid-Atlantic Adult Index)
# - NC p915 (Southern Adult Index)
# - VIMS Staked Gill Net American Shad (Not currently included)
# - ChesMMAP (not included in SEDAR)
# - SEAMAP (Southern Adult Index)

# Not included:
# - NYDEC Peconic Trawl Survey (YOY)
# - NYDEC Western Long Island Sound seine survey data (YOY)

# last updated 11 June 2024 to include data through 2023
###############################################
###############################################

library(tidyverse)
library(janitor)
library(lubridate)

##### Load State Data -------------------------------------------------

# Need survey, stratum, season, month, day, year, depth, lat, lon, salinity, temp, species, count, weight

# Some datasets have bottom temperatures or just water temperatures, so those data were coerced to be Surface Temperatures. If a dataset had both, both measurements were kept. But analysis will focus on surface measurements.


#---- CT Long Island Sound Trawl Survey (CTLISTS) -----

# Weights and counts are pre-standardized to a 30 min tow
ct <- read.csv("/Volumes/Eurybia/CT Long Island Sound survey/CTLISTS_menhaden_Cornell_1984-2023.csv", header = TRUE, na.strings = c(". "))
# colnames(ct)

# Because Month in the original data includes "SOC", when an extra sample was done, pull month from Date instead.

# library(lubridate)
ct$DATE <- mdy(ct$Date)
ct$MONTH <- month(ct$DATE)
ct$DAY <- day(ct$DATE)
ct$YEAR <- year(ct$DATE)
unique(ct$MONTH)
plot(ct$MONTH, ct$TotalCount)
unique(ct$YEAR)
plot(ct$YEAR, ct$TotalCount)

ct2 <- ct %>%
  select(MONTH, DAY, YEAR, Latitude, Longitude, Depth.m, SurfSalin, SurfTemp, BotSalin, BotTemp, TotalCount, Weight.kg) %>%
  rename(MenhadenTotal = TotalCount,
         Month = MONTH,
         Day = DAY,
         Year = YEAR) %>%
  add_column(Survey = "CTLISTS", .before = "Month") %>%
  add_column(Stratum = NA, .after = "Survey") %>%
  add_column(Season = NA, .after = "Stratum")
str(ct2)
ct2$Stratum <- as.character(ct2$Stratum)
ct2$Season <- as.character(ct2$Season)
ct2$Day <- as.numeric(ct2$Day)





#---- Delaware Bay ------

de30 <- read.csv("/Volumes/Eurybia/Delaware Bay Adult Trawl/DEBay_trawl_survey_30ft_2023.csv", header = TRUE, na.strings = c("."))
de16 <- read.csv("/Volumes/Eurybia/Delaware Bay Adult Trawl/DEBay_trawl_survey_16ft_2023.csv", header = TRUE, na.strings = c("."))
colnames(de30)
unique(de30$Month)
plot(de30$Month, de30$Number.Menhaden.Caught)
unique(de30$Year)
plot(de30$Year, de30$Number.Menhaden.Caught)

colnames(de16)
unique(de16$Month)
plot(de16$Month, de16$Number.Menhaden.Caught)
unique(de16$Year)
plot(de16$Year, de16$Number.Menhaden.Caught)

de30.2 <- de30 %>%
  select(Month, Day, Year, EndLat..DD., EndLon..DD., Depth..m., SurSal..ppt., SurTemp..C.., BotSal..ppt., BotTemp..C.., Number.Menhaden.Caught, Weight..kg.) %>%
  rename(Latitude = EndLat..DD.,
         Longitude = EndLon..DD.,
         Depth.m = Depth..m.,
         SurfSalin = SurSal..ppt.,
         SurfTemp = SurTemp..C..,
         BotSalin = BotSal..ppt.,
         BotTemp = BotTemp..C..,
         MenhadenTotal = Number.Menhaden.Caught,
         Weight.kg = Weight..kg.) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "DEBay30ft", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  mutate(Longitude = Longitude*-1) #Fix Longitude, it's without the "-" in the data
str(de30.2)
de30.2$Stratum <- as.character(de30.2$Stratum)
de30.2$Season <- as.character(de30.2$Season)
de30.2$Month <- as.numeric(de30.2$Month)
de30.2$Day <- as.numeric(de30.2$Day)
de30.2$Year <- as.numeric(de30.2$Year)
de30.2$Depth.m <- as.numeric(de30.2$Depth.m)
de30.2$MenhadenTotal <- as.numeric(de30.2$MenhadenTotal)



# de16.2 <- de16 %>%
#   select(Month, Day, Year, EndLat..DD., EndLon..DD., Depth..ft., SurSal..ppt., SurTemp..C.., BotSal..ppt., BotTemp..C.., Number.Menhaden.Caught) %>%
#   rename(Latitude = EndLat..DD.,
#          Longitude = EndLon..DD.,
#          Depth.m = Depth..ft.,
#          SurfSalin = SurSal..ppt.,
#          SurfTemp = SurTemp..C..,
#          BotSalin = BotSal..ppt.,
#          BotTemp = BotTemp..C..,
#          MenhadenTotal = Number.Menhaden.Caught) %>%
#   mutate(Depth.m = Depth.m/3.281) %>% #convert from feet to meters
#   add_column(Stratum = NA, .before = "Month") %>%
#   add_column(Survey = "DEBay30ft", .before = "Stratum") %>%
#   add_column(Season = NA, .after = "Stratum") %>%
#   add_column(Weight.kg = NA, .after = "MenhadenTotal")


#---- Georgia EMTS -----

ga <- read.csv("/Volumes/Eurybia/GA EMTS/EMTS Menhaden all data 1995-2023.csv", header = TRUE)
colnames(ga)

# library(lubridate)
ga$TowDate <- mdy(ga$TowDate)
ga$Month <- month(ga$TowDate)
ga$Day <- day(ga$TowDate)
ga$Year <- year(ga$TowDate)

unique(ga$Year)
unique(ga$Month)
plot(ga$Year, ga$TotNum)

# convert Depth_ft to meters
ga <- mutate(ga, Depth.m = Depth..ft./3.281 )


ga2 <- ga %>%
  select(Month, Day, Year, LatBeg, LonBeg, Depth.m, Salinity.ppt., Wtemp.C., TotNum, TotWt) %>%
  rename(Latitude = LatBeg,
         Longitude = LonBeg,
         SurfSalin = Salinity.ppt.,
         SurfTemp = Wtemp.C.,
         MenhadenTotal = TotNum,
         Weight.kg = TotWt) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "GAEMTS", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin")
str(ga2)
ga2$Stratum <- as.character(ga2$Stratum)
ga2$Season <- as.character(ga2$Season)
ga2$Day <- as.numeric(ga2$Day)
ga2$BotSalin <- as.numeric(ga2$BotSalin)
ga2$BotTemp <- as.numeric(ga2$BotTemp)
ga2$MenhadenTotal <- as.numeric(ga2$MenhadenTotal)




#---- Maryland Gill Net (Upper Chesapeake Bay and Potomac River) -----

# These data have info on set time, length, mesh size to calculate CPUE. I need to decide how to deal with this.

# 2 sets of data with different column names but basically the same info
# 2022-2023
md.a <- read.csv("/Volumes/Eurybia/MD Gill Net/MenhadenData_MDDNR Spring22and23_ASMFC SA.csv", header = TRUE)
# 1986-2021
md.b <- read.csv("/Volumes/Eurybia/MD Gill Net/MD_SprGN_1986-2021_AG.csv", header = TRUE)
colnames(md.a)
colnames(md.b)
# revise md.b to match md.a
md.bb <- md.b %>%
  select(AREA, DATE, YEAR, MONTH, SETNO, SITENO, DEPMIN, DEPMAX, TEMPWATR, TEMPAIR, SAL, SECCHI, GEARLEN, GEARWDTH, MESH, DURATION, SPECCNT) %>%
  rename(SET.NUMBER = SETNO,
         SITE.NUMBER = SITENO,
         MIN.WATER.DEPTH.FT = DEPMIN,
         MAX.WATER.DEPTH.FT = DEPMAX,
         SURF.TEMP.C = TEMPWATR,
         AIR.TEMP.C = TEMPAIR,
         SURF.SALINITY= SAL,
         X = SECCHI,
         LENGTH.NET.SET.FT = GEARLEN,
         WIDTH.NET.SET.FT = GEARWDTH,
         MESH.INCHES = MESH,
         TOTAL.TIME.NET.SET = DURATION,
         NUMBER.MENHADEN = SPECCNT) %>%
  add_column(GEAR = NA, .after = "X") %>%
  add_column(TIMESTRT = NA, .after = "MESH.INCHES") %>%
  add_column(TIMESTOP = NA, .after = "TIMESTRT")
md.bb$X <- as.character(md.bb$X)
md.bb$GEAR <- as.character(md.bb$GEAR)
md.bb$TIMESTRT <- as.character(md.bb$TIMESTRT)
md.bb$TIMESTOP <- as.character(md.bb$TIMESTOP)

str(md.a)
str(md.bb)

# Merge pre-2022 and 2022+
md <- bind_rows(md.a, md.bb)

# Bring in site location info
md.locs <- read.csv("/Volumes/Eurybia/MD Gill Net/MD gill net Spring gill net sites.csv", header = TRUE)
colnames(md.locs)

# Merge lat/long for site locations
md.merge <- left_join(md.locs, md, by = c("AREA", "SITE.NUMBER"))
colnames(md.merge)

# Grab month, day, year from DATE
md.merge$NewDate <- mdy(md.merge$DATE)
md.merge$MONTH <- month(md.merge$NewDate)
md.merge$DAY <- day(md.merge$NewDate)
md.merge$YEAR <- year(md.merge$NewDate)

md.2 <- md.merge %>%
  select(MONTH, DAY, YEAR, Lat.DecimalDegrees, Lon.DecimalDegrees, MAX.WATER.DEPTH.FT, SURF.SALINITY, SURF.TEMP.C, NUMBER.MENHADEN) %>%
  rename(Month = MONTH,
         Day = DAY,
         Year = YEAR,
         Latitude = Lat.DecimalDegrees,
         Longitude = Lon.DecimalDegrees,
         Depth.m = MAX.WATER.DEPTH.FT,
         SurfSalin = SURF.SALINITY,
         SurfTemp = SURF.TEMP.C,
         MenhadenTotal = NUMBER.MENHADEN) %>%
  mutate(Depth.m = Depth.m/3.281) %>% #convert from feet to meters
  add_column(Season = NA, .before = "Month") %>%
  add_column(Stratum = NA, .before = "Season") %>%
  add_column(Survey = "MDGill", .before = "Stratum") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin") %>%
  add_column(Weight.kg = NA, .after = "MenhadenTotal")
str(md.2)
md.2$Stratum <- as.character(md.2$Stratum)
md.2$Season <- as.character(md.2$Season)
md.2$Day <- as.numeric(md.2$Day)
md.2$BotSalin <- as.numeric(md.2$BotSalin)
md.2$BotTemp <- as.numeric(md.2$BotTemp)
md.2$MenhadenTotal <- as.numeric(md.2$MenhadenTotal)
md.2$Weight.kg <- as.numeric(md.2$Weight.kg)




#---- NJ Ocean Trawl Survey -----

nj <- read.csv("/Volumes/Eurybia/New Jersey Ocean Trawl Survey/NJOTMenhadenCatch_1988-2023.csv", header = TRUE, na.strings = c("."))
colnames(nj)
unique(nj$Month)
plot(nj$Month, nj$NUMBER)
unique(nj$Year)
plot(nj$Year, nj$NUMBER)

nj2 <- nj %>%
  select(STRATUM, Month, STA, Year, ELAT.DecimalDegrees, ELON.DecimalDegrees, ENDDEPTH, SALSURF, TEMPSURF, SALBOT, TEMPBOT, NUMBER, WEIGHT) %>%
  rename(Stratum = STRATUM, 
         Day = STA,
         Latitude = ELAT.DecimalDegrees,
         Longitude = ELON.DecimalDegrees,
         Depth.m = ENDDEPTH,
         SurfSalin = SALSURF,
         SurfTemp = TEMPSURF,
         BotSalin = SALBOT,
         BotTemp = TEMPBOT,
         MenhadenTotal = NUMBER,
         Weight.kg = WEIGHT) %>%
  add_column(Survey = "NJOT", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum")
str(nj2)
nj2$Stratum <- as.character(nj2$Stratum)
nj2$Season <- as.character(nj2$Season)
nj2$Month <- as.numeric(nj2$Month)
nj2$Day <- as.numeric(nj2$Day)
nj2$Year <- as.numeric(nj2$Year)
nj2$MenhadenTotal <- as.numeric(nj2$MenhadenTotal)



#---- NY Western Long Island Seine Survey -----
## This is only YOY, not adult, so shouldn't use
## NOT UPDATED, ONLY THROUGH 2021##
# 
# wli <- read.csv("/Volumes/Eurybia/New York Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)
# # colnames(wli)
# wli <- clean_names(wli, "upper_camel")
# # colnames(wli)
# # Water temp = H2O
# unique(wli$Year)
# plot(wli$Year, wli$MenhadenTotal)
# unique(wli$Month)
# plot(wli$Month, wli$MenhadenTotal)
# 
# 
# wli2<- wli %>%
#   select(Month, Day, Year, Latitude, Longitutde, H2O, MenhadenTotal) %>%
#   rename(Longitude = Longitutde, 
#          SurfTemp = H2O) %>%
#   add_column(Survey = "WLI", .before = "Month") %>%
#   add_column(Stratum = NA, .after = "Survey") %>%
#   add_column(Season = NA, .before = "Month") %>%
#   add_column(Depth.m = NA, .after = "Longitude") %>%
#   add_column(SurfSalin = NA, .after = "Depth.m") %>%
#   add_column(BotSalin = NA, .after = "SurfTemp") %>%
#   add_column(BotTemp = NA, .after = "BotSalin") %>%
#   add_column(Weight.kg = NA, .after = "MenhadenTotal")
# 



#---- NC p915 (Southern Adult Index) -----

nc.a <- read.csv("/Volumes/Eurybia/North Carolina Division of Marine Fisheries /AtlMenhadenDataTemplate_NC_2008-2023-WILM.csv", header = TRUE, na.strings = c("."))
nc.b <- read.csv("/Volumes/Eurybia/North Carolina Division of Marine Fisheries /AtlMenhadenDataTemplate_NC_2008-2023-Rivers.csv", header = TRUE, na.strings = c("."))
nc.c <- read.csv("/Volumes/Eurybia/North Carolina Division of Marine Fisheries /AtlMenhadenDataTemplate_NC_2008-2023-PamSnd.csv", header = TRUE, na.strings = c("."))
nc <- rbind(nc.a, nc.b, nc.c)
unique(nc$YEAR)
str(nc)
colnames(nc)
unique(nc$MONTH)
plot(nc$MONTH, nc$COLNUM)
unique(nc$YEAR)
plot(nc$YEAR, nc$COLNUM)

# Need survey, stratum, season, month, day, year, depth, lat, lon, salinity, temp, species, count, weight

nc.2 <- nc %>%
  select(AREA, MONTH, DAY, YEAR, LATITUDE, LONGITUDE, DEPTH, Surface.Salinity..ppt., Surface.Temperature...C., Bottom.Salinity..ppt., Bottom.Temperature...C., COLNUM) %>%
  rename(Stratum = AREA,
         Month = MONTH,
         Day = DAY, 
         Year = YEAR,
         Latitude = LATITUDE,
         Longitude = LONGITUDE,
         Depth.m = DEPTH,
         SurfSalin = Surface.Salinity..ppt.,
         SurfTemp = Surface.Temperature...C.,
         BotSalin = Bottom.Salinity..ppt.,
         BotTemp = Bottom.Temperature...C.,
         MenhadenTotal = COLNUM) %>%
  add_column(Survey = "NCp915", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(Weight.kg = NA, .after = "MenhadenTotal")
str(nc.2)
nc.2$Season <- as.character(nc.2$Season)
nc.2$Month <- as.numeric(nc.2$Month)
nc.2$Day <- as.numeric(nc.2$Day)
nc.2$Year <- as.numeric(nc.2$Year)
nc.2$Weight.kg <- as.numeric(nc.2$Weight.kg)




#---- VIMS Staked Gill Net American Shad -----
# Not sure if I should bother
# vims <- read.csv("/Volumes/Eurybia/VIMS Shad Gill Net/Atlantic Menhaden Data for FI American Shad Surveys VIMS.csv", header = TRUE, na.strings = c("."))
# colnames(vims)
# [1] "Data.Source"          "Begin.Year"           "Begin.Month"         
# [4] "Begin.Day"            "Begin.Time"           "End.Year"            
# [7] "End.Month"            "End.Day"              "End.Time"            
# [10] "TOW.ID"               "Gear"                 "X..menhaden"         
# [13] "Wgt.menhaden..UNITS." "Stratum"              "Depth"               
# [16] "Surface.Temp"         "Salinity"             "DO"                  
# [19] "River"     




#---- ChesMMAP -----

chesmap <- read.csv("/Volumes/Eurybia/ChesMMAP/ChesMMAP_AtlanticMenhaden_Catch-2002-2024.csv", header = TRUE)
colnames(chesmap)
unique(chesmap$month)
plot(chesmap$month, chesmap$raw_total_count)
unique(chesmap$year)
plot(chesmap$year, chesmap$raw_total_count)


chesmap2 <- chesmap %>%
  select(month, year, latitude, longitude, depth_m, SA, WT, raw_total_count, raw_total_biomass) %>%
  rename(Month = month,
         Year = year, 
         Latitude = latitude,
         Longitude = longitude,
         Depth.m = depth_m,
         SurfSalin = SA,
         SurfTemp = WT,
         MenhadenTotal = raw_total_count,
         Weight.kg = raw_total_biomass) %>%
  add_column(Stratum = NA, .before = "Month") %>%
  add_column(Survey = "ChesMMAP", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin") %>%
  add_column(Day = NA, .after = "Month")
str(chesmap2)
chesmap2$Stratum <- as.character(chesmap2$Stratum)
chesmap2$Season <- as.character(chesmap2$Season)
chesmap2$Month <- as.numeric(chesmap2$Month)
chesmap2$Day <- as.numeric(chesmap2$Day)
chesmap2$Year <- as.numeric(chesmap2$Year)
chesmap2$BotSalin <- as.numeric(chesmap2$BotSalin)
chesmap2$BotTemp <- as.numeric(chesmap2$BotTemp)
chesmap2$MenhadenTotal <- as.numeric(chesmap2$MenhadenTotal)





#---- SEAMAP -----

seamap <- read.csv("/Volumes/Eurybia/SEAMAP/janelle.morano.Coastal Survey.ABUNDANCEBIOMASS.2024-06-11T14.24.25.csv", header = TRUE)
colnames(seamap)

# library(lubridate)
seamap$DATEnew <- mdy(seamap$DATE)
seamap$Month <- month(seamap$DATEnew)
seamap$Day <- day(seamap$DATEnew)
seamap$Year <- year(seamap$DATEnew)
unique(seamap$Year)

seamap2 <- seamap %>%
  select(REGION, Month, Day, Year, LATITUDESTART, LONGITUDESTART, SALINITYSURFACE, SALINITYBOTTOM, TEMPSURFACE, TEMPBOTTOM, NUMBERTOTAL, SPECIESTOTALWEIGHT) %>%
  rename(Stratum = REGION,
         Latitude = LATITUDESTART,
         Longitude = LONGITUDESTART,
         SurfSalin = SALINITYSURFACE,
         BotSalin = SALINITYBOTTOM,
         SurfTemp = TEMPSURFACE,
         BotTemp = TEMPBOTTOM,
         MenhadenTotal = NUMBERTOTAL,
         Weight.kg = SPECIESTOTALWEIGHT) %>%
  add_column(Survey = "SEAMAP", .before = "Stratum") %>%
  add_column(Season = NA, .after = "Stratum") %>%
  add_column(Depth.m = NA, .after = "Longitude")

seamap2$Stratum <- gsub("=","",as.character(seamap2$Stratum) )

str(seamap2)
seamap2$Season <- as.character(seamap2$Season)
seamap2$Day <- as.numeric(seamap2$Day)
seamap2$Depth.m <- as.numeric(seamap2$Depth.m)
seamap2$MenhadenTotal <- as.numeric(seamap2$MenhadenTotal)


#----- Merge datasets ------------------------------------------------------

# Verify colnames are the same
colnames(ct2)
colnames(de30.2)
colnames(ga2)
colnames(md.2)
colnames(nc.2)
colnames(nj2)
colnames(chesmap2)
colnames(seamap2)

# Merge state data
statedata <- bind_rows(ct2, de30.2, ga2, md.2, nc.2, nj2, chesmap2, seamap2)
unique(statedata$Survey)
# [1] "CTLISTS"   "DEBay30ft" "GAEMTS"    "MDGill"    "NCp915"    "NJOT"      "ChesMMAP" 
# [8] "SEAMAP" 


# ## Clean up
colnames(statedata)
# [1] "Survey"        "Stratum"       "Season"        "Month"         "Day"          
# [6] "Year"          "Latitude"      "Longitude"     "Depth.m"       "SurfSalin"    
# [11] "SurfTemp"      "BotSalin"      "BotTemp"       "MenhadenTotal" "Weight.kg"    
unique(statedata$Season)
# NA   
unique(statedata$Month)
# 9 10 11  5  6  4  7  8 12  1  3  2 NA
unique(statedata$Year)
# [1] 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001
# [19] 2002 2003 2004 2005 2006 2007 2008 2009 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021
# [37] 2022 2023 2010 1966 1967 1968 1969 1970 1971 1974 1979 1980 1981 1982 1983 2020   NA 2024


# Because some datasets have
 
statedata <- statedata %>%
  # Add Spring & Fall seasons (only)
  mutate(Season = case_when(Month == 1 ~ NA,
                            Month == 2 ~ NA,
                            Month == 3 ~ "SPRING",
                            Month == 4 ~ "SPRING",
                            Month == 5 ~ NA,
                            Month == 6 ~ NA,
                            Month == 7 ~ NA,
                            Month == 8 ~ NA,
                            Month == 9 ~ "FALL",
                            Month == 10 ~ "FALL",
                            Month == 11 ~ NA,
                            Month == 12 ~ NA)) %>%
  # Add Presence/Absence
  mutate(Presence = ifelse(.$MenhadenTotal >0, 1, 0)) %>%
  # Add state ID
  mutate(State = case_when(Survey =="CTLISTS" ~ "RICTNY",
                           Survey =="NJOT" ~ "NJ",
                           Survey =="DEBay30ft" ~ "DEMD",
                           Survey =="MDGill" ~ "DEMD",
                           Survey =="ChesMMAP" ~ "VA",
                           Survey =="NCp915" ~ "NC",
                           Survey =="GAEMTS" ~ "NC",
                           Survey =="SEAMAP" ~ "NC"))


#----- Write dataset as .csv file --------------------------------------------------

#### THIS WILL OVERWRITE!!
# write.csv(statedata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240627.csv", row.names = TRUE)


summary <- statedata %>%
  group_by(Survey) %>%
  summarise(startyr = min(Year, na.rm=TRUE),
            endyr = max(Year, na.rm=TRUE)) 
