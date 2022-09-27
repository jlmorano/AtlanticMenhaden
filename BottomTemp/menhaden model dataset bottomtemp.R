# menhaden model dataset bottomtemp.R
######################################
# Janelle L. Morano
# Plot Bottom temp from Atlantic menhaden dataset
# only need covariate data
#
# last updated 28 April 2022
###############################################
###############################################

covariate_data <- read.csv("//Users/janellemorano/DATA/Atlantic_menhaden_modeling/allsurveymenhaden_covariate_data", header = TRUE)
sapply(covariate_data, function(x) sum(is.na(x)))
covariate_data <- na.omit(covariate_data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2021, and NEFSC is 1963-2019.
library(dplyr)



### Chesapeake
### SPRING
ches.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "SPRING") %>%
  filter(Latitude > 36.5 & Latitude < 37.5)
# Average bottom temp
ches.spring <- ches.spring %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))

### FALL
ches.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "FALL") %>%
  filter(Latitude > 36.5 & Latitude < 37.5)
# Average bottom temp
ches.fall <- ches.fall %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))


# NYBight
### SPRING
nyb.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "SPRING") %>%
  filter(Latitude > 38.5 & Latitude < 41)
# Average bottom temp
nyb.spring <- nyb.spring %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))

### FALL
nyb.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "FALL") %>%
  filter(Latitude > 38.5 & Latitude < 41)
# Average bottom temp
nyb.fall <- nyb.fall %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))


### Plot spring and fall by region
ggplot() +
  # chesapeake
  geom_line(data = ches.spring, aes(Year, avg, color = "darkgreen")) +
  geom_line(data = ches.fall, aes(Year, avg, color = "darkgreen2")) +
  # NY bight
  geom_line(data = nyb.spring, aes(Year, avg, color = "blue")) +
  geom_line(data = nyb.fall, aes(Year, avg, color = "blue2")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") 
  # scale_color_identity(name = "Survey Season",
  #                      breaks = c("darkgreen", "blue"),
  #                      labels = c("Spring", "Fall"),
  #                      guide = "legend")



### SPRING
covariate_data.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "SPRING")
# Annual bottom temp
spring.bottemp <- covariate_data.spring %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))


### FALL
covariate_data.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2021) %>%
  filter(Season == "FALL")
# Annual bottom temp
fall.bottemp <- covariate_data.fall %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))




