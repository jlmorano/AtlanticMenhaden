# create_njoceantrawl-model-data.R
######################################
# Janelle L. Morano

# Use NJ Ocean Trawl survey data and create a dataset for annual estimation in sdmTMB

# last updated 18 Oct 2024
###############################################
###############################################

library(tidyverse)
library(janitor)
library(cowplot)


##### Load State Data -------------------------------------------------

state <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/1-data-input/statesurvey_menhaden_data_20240726.csv", header = TRUE)

nj <- state %>%
  filter(Survey == "NJOT") %>%
  mutate(datetime = lubridate::make_datetime(Year, Month, Day))

# Sampled Weight over Years
c <- ggplot(data = nj, aes ( x = Year, y = Weight.kg)) +
  geom_point(color = "royalblue") +
  theme_classic() +
  ggtitle("New Jersey Ocean Trawl")

# Sampled Weight over Months
d <- ggplot(data = nj, aes ( x = Month, y = Weight.kg)) +
  geom_point(color = "royalblue") +
  theme_classic() +
  ggtitle("New Jersey Ocean Trawl")

# 4 panel includes running chesmap
plot_grid(a, b, c, d, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)


# Annual average
nj.ann <- nj %>%
  group_by(Year) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg))

ggplot(data = nj.ann, aes( x= Year, y = average)) +
  geom_line() +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(0.05)) +
  theme_classic()
# So much variation suggests there's something going on within the year.

# Look at seasonality
# Seasonal monthly average
nj.sea <- nj %>%
  group_by(Month) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg))

ggplot(data = nj.sea, aes( x= Month, y = average)) +
  geom_line() +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(0.05)) +
  theme_classic()

# **Lots of variation in the fall

# Monthly, Annual average
nj.over <- nj %>%
  group_by(Year, Month) %>%
  summarise(average = mean(Weight.kg),
            sd = sd(Weight.kg)) %>%
  mutate(datetime = lubridate::make_datetime(Year, Month),
         Monthabb = lubridate::month.abb(Month))

nj.over$Month <- as.factor(nj.over$Month, levels = month.abb)


  
ggplot() +
  geom_line(data = nj.over, aes(x = Month, y = average, colour = factor(Year))) +
  theme_classic()

  



