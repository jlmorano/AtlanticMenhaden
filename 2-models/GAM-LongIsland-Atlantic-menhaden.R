# GAM-LongIsland-Atlantic-menhaden.R
######################################
# Janelle L. Morano
# Using NYDEC Western Long Island Sound survey data
# And CTLISTS data
# GAM model
# This is for starting to build up state data analysis as I get more state data

# last updated 10 January 2023
###############################################
###############################################

library(tidyverse)
library(janitor)

##### Compile State Data
###############################################

# Need survey, season, month, day, year, depth, lat, lon, salinity, temp, species, count, weight

####
## NY Western Long Island Seine Survey
####

wli <- read.csv("/Users/janellemorano/DATA/NY Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)
colnames(wli)
wli <- clean_names(wli, "upper_camel")
colnames(wli)
# Water temp = H2O

wli2<- wli %>%
  select(Month, Day, Year, Latitude, Longitutde, H2O, MenhadenTotal) %>%
  rename(Longitude = Longitutde, 
         SurfTemp = H2O) %>%
  add_column(Survey = "WLI", .before = "Month") %>%
  add_column(Season = NA, .before = "Month") %>%
  add_column(Depth.m = NA, .after = "Longitude") %>%
  add_column(SurfSalin = NA, .after = "Depth.m") %>%
  add_column(BotSalin = NA, .after = "SurfTemp") %>%
  add_column(BotTemp = NA, .after = "BotSalin") %>%
  add_column(Weight.kg = NA, .after = "MenhadenTotal")
  

####
## CT Long Island Sound Trawl Survey (CTLISTS)
####

ct <- read.csv("/Users/janellemorano/DATA/CT Long Island Sound survey/CTLISTS_menhaden.csv", header = TRUE)
colnames(ct)
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
  rename(MenhadenTotal = TotalCount)


####
## Merge datasets
####

# Verify colnames are the same
colnames(wli2)
colnames(ct2)
statedata <- bind_rows(wli2, ct2)

#### GAM of Count by smooth(Year), Water Temp
####################################################################
library(mgcv)

# Menhaden Catch over Years
state.gam = gam(MenhadenTotal ~ s(Year) + SurfTemp, data = statedata)
summary(state.gam)
plot(state.gam, main = "WLI & CTLITS")

# Plot with mgcViz
library(mgcViz)
b <- getViz(state.gam)
plot( sm(b, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  # scale_y_break(c(50, 3600)) +
  theme_classic()


#### GAM of Count by smooth(Year), Water Temp, Survey (as a proxy for location)
####################################################################

state.gam2 = gam(MenhadenTotal ~ s(Year) + SurfTemp + Survey, data = statedata)
summary(state.gam2)
plot(state.gam2, main = "WLI & CTLITS")

