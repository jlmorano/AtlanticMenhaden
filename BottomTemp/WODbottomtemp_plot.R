# Plot WOD bottom temp
################

library(tidyverse)
library(lubridate)
library(viridis)

ches<-read.csv("/Users/janellemorano/DATA/WOD/WOD_CTD_format_D50_2007-2019_Chesapeake.csv", header = TRUE)
ches$date<-as.Date(ches$date)
ches <- ches %>%
  mutate(month_name = month(date, label = TRUE)) %>%
  mutate(year_name= year(date))

nyb<-read.csv("/Users/janellemorano/DATA/WOD/WOD_CTD_format_D50_2007-2019_NYBight.csv", header = TRUE)
nyb$date<-as.Date(nyb$date)
nyb <- nyb %>%
  mutate(month_name = month(date, label = TRUE)) %>%
  mutate(year_name= year(date))

# Average Bottom Temp per month
library(dplyr)
ave.ches <- ches %>%
  group_by(year_name, month_name) %>%
  summarise(avg_bottemp = mean(bot_temp))

ave.nyb <- nyb %>%
  group_by(year_name, month_name) %>%
  summarise(avg_bottemp = mean(bot_temp))


# By month

ggplot() +
  geom_line(data = ave.nyb, aes(x=month_name, y= avg_bottemp, color = "darkblue")) +
  geom_line(data = ave.ches, aes(x=month_name, y= avg_bottemp, color = "orange")) +
  theme_minimal() + #linetype = as.character(year_name)
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("darkblue", "orange"),
                       labels = c("Chesapeake", "NY Bight"),
                       guide = "legend")

# by specific month

ggplot() +
  geom_line(data = , aes(x=month_name, y= avg_bottemp, color = "darkblue")) +
 # geom_line(data = ave.ches, aes(x=month_name, y= avg_bottemp, color = "orange")) +
  theme_minimal() + #linetype = as.character(year_name)
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("darkblue", "orange"),
                       labels = c("Chesapeake", "NY Bight"),
                       guide = "legend")

# By year
ggplot() +
  geom_line(data = subset(ave.nyb, month_name %in% c("Mar")), 
            aes(x=year_name, y= avg_bottemp, color = "darkblue")) + #, linetype = month_name
  geom_line(data = ave.ches, 
            aes(x=year_name, y= avg_bottemp, color = "orange")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("darkblue", "orange"),
                       labels = c("Chesapeake", "NY Bight"),
                       guide = "legend")

# By season
# Spring
ggplot() +
  geom_line(data = subset(ave.nyb, month_name %in% c("Mar")), aes(x=year_name, y= avg_bottemp, color = "darkblue")) + #, linetype = month_name
  geom_line(data = subset(ave.ches, month_name %in% c("Mar")), aes(x=year_name, y= avg_bottemp, color = "orange")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Spring Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("darkblue", "orange"),
                       labels = c("Chesapeake", "NY Bight"),
                       guide = "legend")

# Fall
ggplot() +
  geom_line(data = subset(ave.nyb, month_name %in% c("Sep")), 
            aes(x=year_name, y= avg_bottemp, color = "darkblue")) + #, linetype = month_name
  geom_line(data = subset(ave.ches, month_name %in% c("Sep")), 
            aes(x=year_name, y= avg_bottemp, color = "orange")) +
  theme_minimal() +
  theme(legend.position="right") +
  ggtitle("Fall Average Bottom Temperature") +
  xlab("") + 
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("darkblue", "orange"),
                       labels = c("Chesapeake", "NY Bight"),
                       guide = "legend")

# Fall and Spring
ggplot() +
  geom_line(data = subset(ave.nyb, month_name %in% c("Mar")), 
            aes(x=year_name, y= avg_bottemp, color = "#39568CFF"), size = 0.8) + 
  geom_line(data = subset(ave.ches, month_name %in% c("Mar")), 
            aes(x=year_name, y= avg_bottemp, color = "#73D055FF"), size = 0.8) +
  geom_line(data = subset(ave.nyb, month_name %in% c("Sep")), 
            aes(x=year_name, y= avg_bottemp, color = "#33638DFF"), size = 0.8) + 
  geom_line(data = subset(ave.ches, month_name %in% c("Sep")), 
            aes(x=year_name, y= avg_bottemp, color = "#95D840FF"), size = 0.8) +
  theme_classic() +
  theme(legend.position="right") +
  # ggtitle("Spring Average Bottom Temperature") +
  # scale_x_date(date_labels = "%Y") +
  xlab("") +
  ylab("Average Temp (C)") +
  scale_color_identity(name = " ",
                       breaks = c("#39568CFF", "#73D055FF", "#33638DFF", "#95D840FF"),
                       labels = c("NY Bight", "Chesapeake", "NY Bight", "Chesapeake"),
                       guide = "legend")
