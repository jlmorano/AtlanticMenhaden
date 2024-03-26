# create_chesmap-model-data.R
######################################
# Janelle L. Morano

# Use NJ Ocean Trawl survey data and create a dataset for annual estimation in sdmTMB

# last updated 14 Feb 2024
###############################################
###############################################

library(tidyverse)
library(janitor)

##### Load State Data -------------------------------------------------

state <- read.csv("/Volumes/Eurybia/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20230727.csv", header = TRUE)

ches <- state %>%
  filter(Survey == "ChesMMAP") %>%
  mutate(datetime = lubridate::make_datetime(Year, Month, Day))


# Annual average
ches.ann <- ches %>%
  group_by(Year) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg))

ggplot(data = ches.ann, aes( x= Year, y = average)) +
  geom_line() +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(0.05)) +
  theme_classic()
# Big variation in recent years

# Look at seasonality
# Seasonal monthly average
ches.sea <- ches %>%
  group_by(Month) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg))

ggplot(data = ches.sea, aes( x= Month, y = average)) +
  geom_line() +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(0.05)) +
  theme_classic()

# **Lots of variation in the winter

# Monthly, Annual average
ches.over <- ches %>%
  group_by(Year, Month) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg))



ggplot() +
  geom_line(data = ches.over, aes(x = Month, y = average, colour = factor(Year))) +
  theme_classic()





