# create-forecasting-datasets.R
######################################
# Janelle L. Morano

# Generate datasets for retrospective forecasting menhaden presence
# 1. "forecasting45.data.list"
#    -Historic dataset, 45 yr: 1972-2017
#    -Forecasting dataset: 2018-2023, with low temperature increase
#    -Forecasting dataset: 2018-2023, with high temperature increase
#    -Forecasting dataset: 2018-2023, with observed water temperatures
#
# 2. "forecasting10.data.list"
#    -Historic dataset, 10 yr: 2018-2023
#    -Forecasting dataset: 2018-2023, with low temperature increase
#    -Forecasting dataset: 2018-2023, with high temperature increase


# last updated 30 April 2025
###############################################
###############################################


library(tidyverse)


# Read in saved RDS of menhaden data, 1972-2023
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
# Read in water temperature averages by state
wt45.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/wt45.list.rds") 
wt10.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/wt10.list.rds")



###########################################################################################
#----- Create historic datasets  --------------------------------------------------
###########################################################################################

# Create 45 yr historic (training) data, 1972-2017
historic45.sp <- data.list[[1]] %>%
  filter(Year >=1972 & Year <= 2017) %>%
  arrange(Year)
historic45.fa <- data.list[[2]] %>%
  filter(Year >=1972 & Year <= 2017) %>%
  arrange(Year)

# Create 10 yr historic (training) data, 2007-2017
historic10.sp <- data.list[[1]] %>%
  filter(Year >=2007 & Year <= 2017) %>%
  arrange(Year)
historic10.fa <- data.list[[2]] %>%
  filter(Year >=2007 & Year <= 2017) %>%
  arrange(Year)




###########################################################################################
#----- Create Forecasting dataset for 45yr historic -------------
###########################################################################################

# Get average depth per State, per year
depth.sp <- data.list[[1]] |>
  group_by(State) |>
  summarise(Depth = mean(Depth, na.rm = TRUE), .groups ="drop")
depth.fa <- data.list[[2]] |>
  group_by(State) |>
  summarise(Depth = mean(Depth, na.rm = TRUE), .groups ="drop")

# Create base database of States and Surveys and forecast years 2018:2023
all_states <- unique(c(data.list$alldata.spring$State, data.list$alldata.fall$State))
all_surveys <- unique(c(data.list$alldata.spring$Survey, data.list$alldata.fall$Survey))

# Spring
future.sp.base <- expand.grid(
  Year = 2018:2023,
  State = all_states,
  Survey = all_surveys,
  stringsAsFactors = FALSE
)
future.sp.base <- future.sp.base %>%
  left_join(depth.sp, by = "State")

# Fall
future.fa.base <- expand.grid(
  Year = 2018:2023,
  State = all_states,
  Survey = all_surveys,
  stringsAsFactors = FALSE
)
future.fa.base <- future.fa.base %>%
  left_join(depth.fa, by = "State")




#-----  Add LOW temp scenario --------------------
#-- Spring 
wt.sp.lookup <- wt45.list$ave.wt45.spring |>
  select(State, AveWT) |>
  distinct()

future45.lowwt.sp <- future.sp.base %>%
  left_join(wt.sp.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 0.25) %>%
  select(Year, State, Survey, WaterTemp, Depth)

#-- Fall 
wt.fa.lookup <- wt45.list$ave.wt45.fall |>
  select(State, AveWT) |>
  distinct()

future45.lowwt.fa <- future.fa.base %>%
  left_join(wt.fa.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 0.25) %>%
  select(Year, State, Survey, WaterTemp, Depth)


#-----  Add HIGH temp scenario --------------------
#-- Spring 
future45.highwt.sp <- future.sp.base %>%
  left_join(wt.sp.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 1) %>%
  select(Year, State, Survey, WaterTemp, Depth)

#-- Fall 
future45.highwt.fa <- future.fa.base %>%
  left_join(wt.fa.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 1) %>%
  select(Year, State, Survey, WaterTemp, Depth)


#-----  Keep observed water temps --------------------
#-- Spring 
# Create 45 yr historic (training) data, 1972-2017
future45.obswt.sp <- data.list[[1]] %>%
  filter(Year >=2018 & Year <= 2023) %>%
  arrange(Year)
future45.obswt.fa <- data.list[[2]] %>%
  filter(Year >=2018 & Year <= 2023) %>%
  arrange(Year)


#-----  Create a list of datasets --------------------
forecasting45.data.list <- list(
  historic45.sp = historic45.sp,
  historic45.fa = historic45.fa,
  future45.lowwt.sp = future45.lowwt.sp,
  future45.lowwt.fa = future45.lowwt.fa,
  future45.highwt.sp = future45.highwt.sp,
  future45.highwt.fa = future45.highwt.fa, 
  future45.obswt.sp = future45.obswt.sp,
  future45.obswt.fa = future45.obswt.fa
)

#-- Save
saveRDS(forecasting45.data.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.data.list")




###########################################################################################
#----- Create Forecasting dataset for 10yr historic -------------
###########################################################################################

# Use future.base from above

#-----  Add LOW temp scenario --------------------
#-- Spring 
wt.sp.lookup <- wt10.list$ave.wt10.spring |>
  select(State, AveWT) |>
  distinct()

future10.lowwt.sp <- future.sp.base %>%
  left_join(wt.sp.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 0.25) %>%
  select(Year, State, Survey, WaterTemp, Depth)

#-- Fall 
wt.fa.lookup <- wt10.list$ave.wt10.fall |>
  select(State, AveWT) |>
  distinct()

future10.lowwt.fa <- future.fa.base %>%
  left_join(wt.fa.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 0.25) %>%
  select(Year, State, Survey, WaterTemp, Depth)


#-----  Add HIGH temp scenario --------------------
#-- Spring 
future10.highwt.sp <- future.sp.base %>%
  left_join(wt.sp.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 1) %>%
  select(Year, State, Survey, WaterTemp, Depth)

#-- Fall 
future10.highwt.fa <- future.fa.base %>%
  left_join(wt.fa.lookup, by = "State") %>%
  mutate(WaterTemp = AveWT + 1) %>%
  select(Year, State, Survey, WaterTemp, Depth)



#-----  Create a list of datasets --------------------

forecasting10.data.list <- list(
  historic10.sp = historic10.sp,
  historic10.fa = historic10.fa,
  future10.lowwt.sp = future10.lowwt.sp,
  future10.lowwt.fa = future10.lowwt.fa,
  future10.highwt.sp = future10.highwt.sp,
  future10.highwt.fa = future10.highwt.fa
)

#-- Save
saveRDS(forecasting10.data.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.data.list")

