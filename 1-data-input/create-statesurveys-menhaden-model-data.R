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
# - (not yet, and I think omit) VIMS Shad Gill net
# - Georgia EMTS
# - SEAMAP

# last updated 29 May 2024
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

wli <- read.csv("/Users/janellemorano/DATA/New York Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)
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

nj <- read.csv("/Users/janellemorano/DATA/New Jersey Ocean Trawl Survey/NJOTMenhadenCatch.csv", header = TRUE)
colnames(nj)

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

nj2$Stratum <- as.character(nj2$Stratum)



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
  add_column(Season = NA, .after = "Stratum") %>%
  mutate(Longitude = Longitude*-1) #Fix Longitude



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


#---- SEAMAP -----

seamap <- read.csv("/Users/janellemorano/DATA/SEAMAP/janelle.morano.Coastal Survey.ABUNDANCEBIOMASS.2023-06-06T15.35.29.csv", header = TRUE)
colnames(seamap)

# library(lubridate)
seamap$DATE <- mdy(seamap$DATE)
seamap$Month <- month(seamap$DATE)
seamap$Day <- day(seamap$DATE)
seamap$Year <- year(seamap$DATE)

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




#----- Merge datasets ------------------------------------------------------

# Verify colnames are the same
colnames(ct2)
colnames(wli2)
colnames(nj2)
colnames(de2)
colnames(chesmap2)
colnames(ga2)
colnames(seamap2)

unique(ct2$Year)
# 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
# 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021
unique(wli2$Year)
# 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
# 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
unique(nj2$Year)
# 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
# 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2022
unique(de2$Year)
#  1966 1967 1968 1969 1970 1971 1974 1979 1980 1981 1982 1983 1984 1990 1991 1992 1993 1994 1995 1996 1997 1998
# 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
# 2021 2022
unique(chesmap2$Year)
# 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018
unique(ga2$Year)
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 1995 1996
# 1997 1998
unique(seamap2$Year)
# 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010
# 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021 2022   NA

unique(ct2$Month)
# 5  6  4  9 10
unique(wli2$Month)
# 5  6  7  8  4 10  9 11
unique(nj2$Month)
# 8  9 10 11  1  2  4  6 12  3  5  7
unique(de2$Month)
# 8  9 10 11 12  1  3  5  6  7  2  4
unique(chesmap2$Month)
# 3  5  7  9 10 11
unique(ga2$Month)
# 4  5  6  7  9  2  3  8 10  1 12 11
unique(seamap2$Month)
# 4  5  6  7  8 10 11  9 NA


# Merge state data
statedata <- bind_rows(ct2, wli2, nj2, de2, chesmap2, ga2, seamap2)
unique(statedata$Survey)
# "CTLISTS"  "WLI"      "NJOT"     "DEBay"    "ChesMMAP" "GAEMTS"   "SEAMAP" 
unique(statedata$Month)

## Clean up
colnames(statedata)
# [1] "Survey"        "Stratum"       "Season"        "Month"         "Day"          
# [6] "Year"          "Latitude"      "Longitude"     "Depth.m"       "SurfSalin"    
# [11] "SurfTemp"      "BotSalin"      "BotTemp"       "MenhadenTotal" "Weight.kg" 
unique(statedata$Survey) #OK
unique(statedata$Season)
# "SPRING " "FALL "   NA   
unique(statedata$Month)
# 5  6  4  9 10  7  8 11  1  2 12  3 NA
unique(statedata$Year)
# 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005
# 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021 2020 2022 1966 1967 1968 1969 1970
# 1971 1974 1979 1980 1981 1982 1983   NA



statedata <- statedata %>%
  # Add Spring & Fall seasons (only)
  mutate(Season = case_when(Month == 1 ~ NA, #I don't know why it won't accept a multiple, so doing this the hard way
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
                           Survey =="WLI" ~ "RICTNY",
                           Survey =="NJOT" ~ "NJ",
                           Survey =="DEBay" ~ "DEMD",
                           Survey =="ChesMMAP" ~ "VA",
                           Survey =="GAEMTS" ~ "NC",
                           Survey =="SEAMAP" ~ "NC"))


#----- Write dataset as .csv file --------------------------------------------------

#### THIS WILL OVERWRITE!!
# write.csv(statedata,"/Users/janellemorano/DATA/Atlantic_menhaden_modeling/statesurvey_menhaden_data_20230727.csv", row.names = TRUE)
