# Plot Bottom temp from Atlantic menhaden dataset
# only need covariate data

covariate_data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_covariate_data.csv", header = TRUE)
sapply(covariate_data, function(x) sum(is.na(x)))
covariate_data <- na.omit(covariate_data)

# The above dataset is the full date range, but NEAMAP is only from 2007-2021, and NEFSC is 1963-2019.
# Modify the dataset for only overlapping years, 2007-2019, AND dividing into SPRING and FALL seasons.
library(dplyr)


### SPRING
covariate_data.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "SPRING")
# Annual bottom temp
spring.bottemp <- covariate_data.spring %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))


### FALL
covariate_data.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "FALL")
# Annual bottom temp
fall.bottemp <- covariate_data.fall %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))


### Plot by Region
### SPRING
ches.spring <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "SPRING") %>%
  filter(Latitude > 36.5 & < 37.5)
# Annual bottom temp
nyb.spring <- covariate_data.spring %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))
### FALL
ches.fall <- covariate_data %>%
  filter(Year >= 2007 & Year <= 2019) %>%
  filter(Season == "FALL")
# Annual bottom temp
nyb.fall <- covariate_data.fall %>%
  group_by(Year) %>%
  summarise(avg = mean(Bottemp))

ggplot() +
  geom_line(data = subset(spring.bottemp, month_name %in% c("Sep")), 
                          aes(Year, avg, color = "darkgreen")) +
  geom_line(data = fall.bottemp, aes(Year, avg, color = "blue")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = "Survey Season",
                       breaks = c("darkgreen", "blue"),
                       labels = c("Spring", "Fall"),
                       guide = "legend")


### Plot spring and fall
ggplot() +
  geom_line(data = spring.bottemp, aes(Year, avg, color = "darkgreen")) +
  geom_line(data = fall.bottemp, aes(Year, avg, color = "blue")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = "Survey Season",
                       breaks = c("darkgreen", "blue"),
                       labels = c("Spring", "Fall"),
                       guide = "legend")




