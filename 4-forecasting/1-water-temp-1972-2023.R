# 1-water-temp-1972-2023.R
######################################
# Janelle L. Morano

# Water temperature and averages, 1972-2023 (from menhaden data)

# last updated 16 June 2025
###############################################
###############################################


library(tidyverse)
library(cowplot)
library(broom)
library(modelsummary)

# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")

lm.sp <- lm(WaterTemp ~ Year + State, data = data.list$alldata.spring)
lm.fa <- lm(WaterTemp ~ Year + State, data = data.list$alldata.fall)
lm.list <- list(lm.sp, lm.fa)
modelsummary(lm.list, output = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/lm.watertemp.table.docx")


# Linear model of water temp, Spring
lm.model.sp <- data.list$alldata.spring %>%
  group_by(State) %>%
  do({
    model <- lm(WaterTemp ~ Year, data = .)
    new.data <- data.frame(Year = seq(min(.$Year), max(.$Year)))
    pred <- predict(model, newdata = new.data, interval = "confidence")
    cbind(new.data, as.data.frame(pred), State = unique(.$State))
  })

# Linear model of water temp, Fall
lm.model.fa <- data.list$alldata.fall %>%
  group_by(State) %>%
  do({
    model <- lm(WaterTemp ~ Year, data = .)
    new.data <- data.frame(Year = seq(min(.$Year), max(.$Year)))
    pred <- predict(model, newdata = new.data, interval = "confidence")
    cbind(new.data, as.data.frame(pred), State = unique(.$State))
  })



###########################################################################################
#----- Plot water temperature (wt) trends over time by State --------------------------------------
###########################################################################################

pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent

# Plot water temperature points
figS1a <- ggplot(data = data.list$alldata.spring, aes(x = Year, y = WaterTemp, color = State)) +
  geom_point() +
  scale_color_manual(values = pal8) +
  theme_classic() +
  theme(legend.position = "none") +
  ylim(0, 40) +
  theme(text = element_text(size = 12)) +
  labs(x = " ", y = "Water Temperature (°C)")
figS1b <- ggplot(data = data.list$alldata.fall, aes(x = Year, y = WaterTemp, color = State)) +
  geom_point() +
  scale_color_manual(values = pal8) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x = " ", y = "Water Temperature (°C)")


# Plot lm slope lines only
figS1c <- ggplot(data = data.list$alldata.spring, aes(x = Year, y = fit, color = State)) +
  geom_ribbon(data = lm.model.sp,
              aes(x = Year, ymin = lwr, ymax = upr, fill = State),
              alpha = 0.2, color = NA) +
  scale_fill_manual(values = pal8lite) +
  geom_line(data = lm.model.sp, aes(x = Year, y = fit, color = State), linewidth = 0.8) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  ylim(0, 40) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x = " ", y = "Water Temperature (°C)")


figS1d <- ggplot(data = data.list$alldata.fall, aes(x = Year, y = fit, color = State)) +
  geom_ribbon(data = lm.model.fa,
              aes(x = Year, ymin = lwr, ymax = upr, fill = State),
              alpha = 0.2, color = NA) +
  scale_fill_manual(values = pal8lite) +
  geom_line(data = lm.model.fa, aes(x = Year, y = fit, color = State), linewidth = 0.8) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  ylim(0, 40) +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  labs(x = " ", y = "Water Temperature (°C)")


plot_grid(figS1a, figS1b, figS1c, figS1d, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/figS1-watertemp-1972-2023.png", width=8, height = 7)








###########################################################################################
#----- Average water temperature, by 3 time periods  --------------------------------------
###########################################################################################
# for 1972-2017
ave.wt45.spring <-  data.list$alldata.spring |>
  filter(Year >= 1972, Year <= 2017) |>
  summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
            se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
  arrange(State)
ave.wt45.fall <-  data.list$alldata.fall |>
  filter(Year >= 1972, Year <= 2017) |>
  summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
            se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
  arrange(State)

# # for 2007-2017
# ave.wt10.spring <-  data.list$alldata.spring |>
#   filter(Year >= 2007, Year <= 2017) |>
#   summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
#             se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
#   arrange(State)
# ave.wt10.fall <-  data.list$alldata.fall |>
#   filter(Year >= 2007, Year <= 2017) |>
#   summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
#             se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
#   arrange(State)

# for 2018-2023
ave.wt6.spring <-  data.list$alldata.spring |>
  filter(Year >= 2018, Year <= 2023) |>
  summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
            se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
  arrange(State)
ave.wt6.fall <-  data.list$alldata.fall |>
  filter(Year >= 2018, Year <= 2023) |>
  summarise(AveWT = mean(WaterTemp, na.rm=TRUE), .by=State,
            se = sd(WaterTemp, na.rm = TRUE) / sqrt(n()) ) |>
  arrange(State)

# Spring annual difference
wtdiff.sp <- ave.wt6.spring$AveWT - ave.wt45.spring$AveWT
mean(wtdiff.sp)
# 1.145211
var(wtdiff.sp) / length(wtdiff.sp)
# 0.05787548

# Fall annual difference
wtdiff.fa <- ave.wt6.fall$AveWT - ave.wt45.fall$AveWT
mean(wtdiff.fa)
# 1.4993
var(wtdiff.fa) / length(wtdiff.fa)
# 0.01534209


#----- Create df of average water temperature 
# for 1972-2017
years45 <- 1972:2017
ave.wt45.spring <- ave.wt45.spring |>
  crossing(Year = years45) |>   # Repeat for each year
  arrange(State, Year) |>
  select(Year, State, AveWT, se)
ave.wt45.fall <- ave.wt45.fall |>
  crossing(Year = years45) |>   # Repeat for each year
  arrange(State, Year) |>
  select(Year, State, AveWT, se)

# # for 2007-2017
# years10 <- 2007:2017
# ave.wt10.spring <- ave.wt10.spring |>
#   crossing(Year = years10) |>   # Repeat for each year
#   arrange(State, Year) |>
#   select(Year, State, AveWT, se)
# ave.wt10.fall <- ave.wt10.fall |>
#   crossing(Year = years10) |>   # Repeat for each year
#   arrange(State, Year) |>
#   select(Year, State, AveWT, se)

# for 2018-2023
years6 <- 2018:2023
ave.wt6.spring <- ave.wt6.spring |>
  crossing(Year = years6) |>   # Repeat for each year
  arrange(State, Year) |>
  select(Year, State, AveWT, se)
ave.wt6.fall <- ave.wt6.fall |>
  crossing(Year = years6) |>   # Repeat for each year
  arrange(State, Year) |>
  select(Year, State, AveWT, se) 
  
  
  
#----- Save df of average water temperature
wt45.list <- list(ave.wt45.spring = ave.wt45.spring,
                  ave.wt45.fall = ave.wt45.fall)
saveRDS(wt45.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/wt45.list.rds")
wt10.list <- list(ave.wt10.spring = ave.wt10.spring,
                  ave.wt10.fall = ave.wt10.fall)
saveRDS(wt10.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/wt10.list.rds")




###########################################################################################
#----- Plot average temperatures --------------------------------------
###########################################################################################
# List of States to plot
states <- unique(na.omit(data.list[[1]]$State))
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
# Colors to use
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent

# Spring
fig1a <- ggplot() +
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[1]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[1]) +
  geom_line(data = subset(ave.wt45.spring, State == states[1]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[1], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[1]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[1]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[1]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[1], linewidth = 1.2, 
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[1]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[1]) +
  geom_line(data = subset(ave.wt6.spring, State == states[1]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[1], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[2]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[2]) +
  geom_line(data = subset(ave.wt45.spring, State == states[2]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[2], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[2]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[2]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[2]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[2], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[2]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[2]) +
  geom_line(data = subset(ave.wt6.spring, State == states[2]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[2], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[3]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[3]) +
  geom_line(data = subset(ave.wt45.spring, State == states[3]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[3], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[3]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[3]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[3]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[3], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[3]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[3]) +
  geom_line(data = subset(ave.wt6.spring, State == states[3]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[3], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[4]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[4]) +
  geom_line(data = subset(ave.wt45.spring, State == states[4]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[4], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[4]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[4]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[4]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[4], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[4]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[4]) +
  geom_line(data = subset(ave.wt6.spring, State == states[4]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[4], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[5]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[5]) +
  geom_line(data = subset(ave.wt45.spring, State == states[5]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[5], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[5]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[5]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[5]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[5], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[5]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[5]) +
  geom_line(data = subset(ave.wt6.spring, State == states[5]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[5], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[6]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[6]) +
  geom_line(data = subset(ave.wt45.spring, State == states[6]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[6], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[6]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[6]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[6]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[6], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[6]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[6]) +
  geom_line(data = subset(ave.wt6.spring, State == states[6]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[6], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[7]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[7]) +
  geom_line(data = subset(ave.wt45.spring, State == states[7]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[7], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[7]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[7]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[7]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[7], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[7]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[7]) +
  geom_line(data = subset(ave.wt6.spring, State == states[7]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[7], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.spring, State == states[8]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[8]) +
  geom_line(data = subset(ave.wt45.spring, State == states[8]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[8], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.spring, State == states[8]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[8]) +
  # geom_line(data = subset(ave.wt10.spring, State == states[8]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[8], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.spring, State == states[8]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[8]) +
  geom_line(data = subset(ave.wt6.spring, State == states[8]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[8], linewidth = 1.2, 
            linetype = "2111") +
  
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(4, 26, by = 2), limits = c(4,26)) +
  labs(x = " ", y = "Water Temperature (°C)")

# Fall
fig1b <- ggplot() +
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[1]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[1]) +
  geom_line(data = subset(ave.wt45.fall, State == states[1]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[1], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[1]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[1]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[1]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[1], linewidth = 1.2, 
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[1]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[1]) +
  geom_line(data = subset(ave.wt6.fall, State == states[1]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[1], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[2]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[2]) +
  geom_line(data = subset(ave.wt45.fall, State == states[2]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[2], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[2]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[2]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[2]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[2], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[2]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[2]) +
  geom_line(data = subset(ave.wt6.fall, State == states[2]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[2], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[3]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[3]) +
  geom_line(data = subset(ave.wt45.fall, State == states[3]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[3], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[3]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[3]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[3]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[3], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[3]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[3]) +
  geom_line(data = subset(ave.wt6.fall, State == states[3]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[3], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[4]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[4]) +
  geom_line(data = subset(ave.wt45.fall, State == states[4]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[4], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[4]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[4]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[4]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[4], linewidth = 1.2,
  #           linetype = "6111") +
  # # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[4]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[4]) +
  geom_line(data = subset(ave.wt6.fall, State == states[4]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[4], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[5]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[5]) +
  geom_line(data = subset(ave.wt45.fall, State == states[5]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[5], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[5]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[5]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[5]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[5], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[5]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[5]) +
  geom_line(data = subset(ave.wt6.fall, State == states[5]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[5], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[6]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[6]) +
  geom_line(data = subset(ave.wt45.fall, State == states[6]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[6], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[6]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[6]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[6]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[6], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[6]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[6]) +
  geom_line(data = subset(ave.wt6.fall, State == states[6]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[6], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[7]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[7]) +
  geom_line(data = subset(ave.wt45.fall, State == states[7]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[7], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[7]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[7]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[7]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[7], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[7]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[7]) +
  geom_line(data = subset(ave.wt6.fall, State == states[7]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[7], linewidth = 1.2, 
            linetype = "2111") +
  
  # 45 yr observed average temp
  geom_ribbon(data = subset(ave.wt45.fall, State == states[8]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[8]) +
  geom_line(data = subset(ave.wt45.fall, State == states[8]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[8], linewidth = 1.2) +
  # # 10 yr observed average temp
  # geom_ribbon(data = subset(ave.wt10.fall, State == states[8]), 
  #             aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
  #             fill = pal8lite[8]) +
  # geom_line(data = subset(ave.wt10.fall, State == states[8]),
  #           aes(x = Year, y = AveWT, group = State),
  #           color = pal8[8], linewidth = 1.2,
  #           linetype = "6111") +
  # 6 yr observed average temp
  geom_ribbon(data = subset(ave.wt6.fall, State == states[8]), 
              aes(x=Year, ymin = AveWT-(1.96*se), ymax = AveWT+(1.96*se), group = State), 
              fill = pal8lite[8]) +
  geom_line(data = subset(ave.wt6.fall, State == states[8]),
            aes(x = Year, y = AveWT, group = State),
            color = pal8[8], linewidth = 1.2, 
            linetype = "2111") +
  
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(4, 26, by = 2), limits = c(4,26)) +
  labs(x = " ", y = "Water Temperature (°C)")


plot_grid(fig1a, fig1b, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "//Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/figS1-watertemp-average-1972-2023-3periods.png", width=7, height = 4)


# Combined all 6
plot_grid(figS1a, figS1b, figS1c, figS1d, fig1a, fig1b, labels=c("A", "B", "C", "D", "E", "F"), ncol = 2, nrow = 3)
#ggsave(file = "//Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig1-watertemp-1972-2023-3ways.png", width=7, height = 8)
