# 5-forecasting-state-proportions.R
######################################
# Janelle L. Morano

# Graph annual state proportions for forecasted years 2018-2023


# last updated 17 June 2025
###############################################
###############################################


library(tidyverse)
library(cowplot)


# Read full data 1972-2023 
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")


# Read in saved RDS of predictions
forecasting45.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.predictions.list.rds")
# forecasting10.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.predictions.list.rds")
# Just use [[1:2]] for all



#----- Observed average per state region and year

# Spring, observed average
ave.presence.sp <- data.list[[1]] |>
  group_by(Year, State) |>
  summarise(ave.pres = mean(Presence))

# Fall, observed average
ave.presence.fa <- data.list[[2]] |>
  group_by(Year, State) |>
  summarise(ave.pres = mean(Presence))

# Spring, 45yr, no wt fit
annual45.prop.sp <- forecasting45.predictions.list[[1]]  |>
    group_by(Year, State) |>
    summarise(
      state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
      .groups = 'drop')

# Fall, 45 yr, no wt fit
annual45.prop.fa <- forecasting45.predictions.list[[2]]  |>
  group_by(Year, State) |>
  summarise(
    state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
    .groups = 'drop')

# Spring, 45yr, HI wt fit
annual45.HI.prop.sp <- forecasting45.predictions.list[[7]]  |>
  group_by(Year, State) |>
  summarise(
    state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
    .groups = 'drop')

# Fall, 45 yr, HI wt fit
annual45.HI.prop.fa <- forecasting45.predictions.list[[8]]  |>
  group_by(Year, State) |>
  summarise(
    state.annual.fit = if(all(is.na(fit))) NA_real_ else max(fit, na.rm = TRUE),
    .groups = 'drop')


#----- Difference between forecasts and observations and forecasts
#----- where forecasts-observations, positive number = overestimate
#                           negative number = underestimate

## Spring
obs.sp <- ave.presence.sp |>
  filter(Year >= 2018 & Year <= 2023) |>
  arrange(Year, State) |>
  rename(Obs = ave.pres)
  
fore.sp <- annual45.prop.sp |>
  filter(Year >= 2018 & Year <= 2023) |>
  arrange(Year, State) |>
  rename(Forecast = state.annual.fit)

# spring, Difference between observations and forecasts
prop.diff.sp <- left_join(obs.sp, fore.sp)
prop.diff.sp <- prop.diff.sp |>
  mutate(delta = Forecast - Obs) |>
  mutate(percentdiff = abs(delta)/(Forecast + Obs / 2)*100)


## Fall
obs.fa <- ave.presence.fa |>
  filter(Year >= 2018 & Year <= 2023) |>
  arrange(Year, State) |>
  rename(Obs = ave.pres)

fore.fa <- annual45.prop.fa |>
  filter(Year >= 2018 & Year <= 2023) |>
  arrange(Year, State) |>
  rename(Forecast = state.annual.fit)

# fall, Difference between observations and forecasts
prop.diff.fa <- left_join(obs.fa, fore.fa)
prop.diff.fa <- prop.diff.fa |>
  mutate(delta = Forecast - Obs) |>
  mutate(percentdiff = abs(delta)/(Forecast + Obs / 2)*100)


# 2018-2023 Mean difference between observations and forecasts, per state
# mean
prop.diff.sp |>
  group_by(State) |>  
  summarise(meandelta = mean(delta))
# percent
prop.diff.sp |>
  group_by(State) |> 
  summarise(percentmean = mean(abs(Forecast - Obs) / ((Forecast + Obs) / 2) * 100, na.rm = TRUE))

# mean
prop.diff.fa |>
  group_by(State) |>
  summarise(meandelta = mean(delta))
# percent
prop.diff.fa |>
  group_by(State) |>
  summarise(percentmean = mean(abs(Forecast - Obs) / ((Forecast + Obs) / 2) * 100, na.rm = TRUE))




#----- Plot the proportions

# Colors
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")

# 2000-2017
fobssp1 <- ggplot(subset(ave.presence.sp, Year %in% 2000:2017), aes(x = factor(Year), y = ave.pres, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  labs(x = "", y = "Annual State-Region Prevalence") +
  theme(legend.position = "none")

fobsfa1 <- ggplot(subset(ave.presence.fa, Year %in% 2000:2017), aes(x = factor(Year), y = ave.pres, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  labs(x = "", y = "Annual State-Region Prevalence") +
  theme(legend.position = "none")

# 2018-2023
fobssp <- ggplot(subset(ave.presence.sp, Year %in% 2018:2023), aes(x = factor(Year), y = ave.pres, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  labs(x = "", y = "Annual State-Region Prevalence") +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")

fobsfa <- ggplot(subset(ave.presence.fa, Year %in% 2018:2023), aes(x = factor(Year), y = ave.pres, fill = State)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  labs(x = "", y = "Annual State-Region Prevalence") +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")

# 45-yr forecast NO WT
f45sp <- ggplot() +
  geom_bar(data = annual45.prop.sp, aes(x = Year, y = state.annual.fit, fill = State), position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  scale_x_continuous(breaks = seq(2018, 2023, by = 1)) +
  labs(x = "", y = " ") +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")

f45fa <- ggplot() +
  geom_bar(data = annual45.prop.fa, aes(x = Year, y = state.annual.fit, fill = State), position = "stack", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
  scale_x_continuous(breaks = seq(2018, 2023, by = 1)) +
  labs(x = "", y = " ") +
  theme(axis.text.y = element_blank()) +
  theme(legend.position = "none")

# # 45-yr forecast HI wt
# f45hisp <- ggplot() +
#   geom_bar(data = annual45.HI.prop.sp, aes(x = Year, y = state.annual.fit, fill = State), position = "stack", stat = "identity") +
#   scale_fill_manual(values = pal8) +
#   theme_classic() +
#   scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
#   labs(x = "", y = " ") +
#   theme(axis.text.y = element_blank()) +
#   theme(legend.position = "none")
# 
# f45hifa <- ggplot() +
#   geom_bar(data = annual45.HI.prop.fa, aes(x = Year, y = state.annual.fit, fill = State), position = "stack", stat = "identity") +
#   scale_fill_manual(values = pal8) +
#   theme_classic() +
#   scale_y_continuous(breaks = seq(0, 1.75, by = 0.25), limits = c(0., 1.75)) +
#   labs(x = "", y = " ") +
#   theme(axis.text.y = element_blank()) +
#   theme(legend.position = "none")


# Forecasts - Observations
fdeltasp <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5, color = "grey60") +
  geom_point(data = prop.diff.sp, aes(x = Year, y = delta, color = State), size = 3) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25), limits = c(-1, 1)) +
  theme(legend.position = "none")

fdeltafa <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.5, color = "grey60") +
  geom_point(data = prop.diff.fa, aes(x = Year, y = delta, color = State), size = 3) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25), limits = c(-1, 1)) +
  theme(legend.position = "none")

plot_grid(fobssp, f45sp, fdeltasp, fobsfa, f45fa, fdeltafa, ncol = 3, nrow = 2, rel_widths = c(1, 1, 1))
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/_fig4-stateprop-2017-2023.png", dpi = 400, width=10, height = 8)


f5a <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1.5, color = "grey60") +
  geom_point(data = prop.diff.sp, aes(x = Year, y = percentdiff, color = State), size = 3) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 200)) +
  theme(legend.position = "none")

f5b <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1.5, color = "grey60") +
  geom_point(data = prop.diff.fa, aes(x = Year, y = percentdiff, color = State), size = 3) +
  scale_color_manual(values = pal8) +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 200, by = 50), limits = c(0, 200)) +
  theme(legend.position = "none")
plot_grid(f5a, f5b, ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/_fig5-percentchange-2017-2023.png", dpi = 400, width=8, height = 6)