# forecasting-water-temp.R
######################################
# Janelle L. Morano


### THIS IS NO LONGER USED

# Forecasting water temperature for 2018-2023
#    1. from 45yr historic (1972-2017), by state
#                               and by coastwide
#    2. from 10yr historic (2007-2017), by state
#                               and by coastwide


# last updated 5 April 2025
###############################################
###############################################


library(tidyverse)
library(mgcv)
library(cowplot)

# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")

###########################################################################################
#----- Create 1972-2017 and 2007-2017 data ------------------------------------------------
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

historic.list <- list(historic45.sp = historic45.sp,
                      historic45.fa = historic45.fa,
                      historic10.sp = historic10.sp,
                      historic10.fa = historic10.fa)




###########################################################################################
#----- Fit 45 yrs (1972-2017), by state and coastwide ---------------------------
###########################################################################################
# Data for 45 years are historic.list[[1:2]]

wt45.gam.list <- list()
wt45.gam.summaries <- list()

# "state" model
for (name in names(historic.list)[1:2]) {
  wt45.gam.list[[paste("state", name, sep = ".")]] <- gam(WaterTemp ~ s(Year, by = State) + State + s(Survey, bs = "re"), family = gaussian(), method = "REML", data = historic.list[[name]])
  wt45.gam.summaries[[paste("state", name, sep = ".")]] <- summary(wt45.gam.list[[paste("state", name, sep = ".")]])
}

# "coast" model
for (name in names(historic.list)[1:2]) {
  wt45.gam.list[[paste("coast", name, sep = ".")]] <- gam(WaterTemp ~ s(Year) + State + s(Survey, bs = "re"), family = gaussian(), method = "REML", data = historic.list[[name]])
  wt45.gam.summaries[[paste("coast", name, sep = ".")]] <- summary(wt45.gam.list[[paste("coast", name, sep = ".")]])
}

print(wt45.gam.list)
print(wt45.gam.summaries)



###########################################################################################
#----- Fit 10 yrs (2007-2017), by state and coastwide ---------------------------
###########################################################################################
# Data for 10 years are historic.list[[3:4]]

wt10.gam.list <- list()
wt10.gam.summaries <- list()
# "state" model
for (name in names(historic.list)[3:4]) {
  wt10.gam.list[[paste("state", name, sep = ".")]] <- gam(WaterTemp ~ s(Year, by = State) + State + s(Survey, bs = "re"), family = gaussian(), method = "REML", data = historic.list[[name]])
  wt10.gam.summaries[[paste("state", name, sep = ".")]] <- summary(wt10.gam.list[[paste("state", name, sep = ".")]])
}

# "coast" model
for (name in names(historic.list)[3:4]) {
  wt10.gam.list[[paste("coast", name, sep = ".")]] <- gam(WaterTemp ~ s(Year) + State + s(Survey, bs = "re"), family = gaussian(), method = "REML", data = historic.list[[name]])
  wt10.gam.summaries[[paste("coast", name, sep = ".")]] <- summary(wt10.gam.list[[paste("coast", name, sep = ".")]])
}

# Check the results
print(wt10.gam.list)
print(wt10.gam.summaries)



###########################################################################################
#----- Save 45 and 10yr fits  ---------------------------
###########################################################################################
saveRDS(wt45.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt45-gam-results.rds")
saveRDS(wt45.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt45-gam-summaries.rds")
saveRDS(wt10.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt10-gam-results.rds")
saveRDS(wt10.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt10-gam-summaries.rds")

wt45.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt45-gam-results.rds")
wt10.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt10-gam-results.rds")




###########################################################################################
#----- Create forecast data, 2018-2023  ---------------------------
###########################################################################################

wt.pred.sp <- data.frame()
for (i in seq(2018, 2023)) {
  for (j in unique(data.list$alldata.spring$State)) {
    for(k in unique(data.list$alldata.spring$Survey)) {
      new <- data.frame(Year = i,
                        State = j,
                        Survey = k)
      wt.pred.sp <- rbind(wt.pred.sp, new)
    }
  }
}

wt.pred.fa <- data.frame()
for (i in seq(2018, 2023)) {
  for (j in unique(data.list$alldata.fall$State)) {
    for(k in unique(data.list$alldata.fall$Survey)) {
      new <- data.frame(Year = i,
                        State = j,
                        Survey = k)
      wt.pred.fa <- rbind(wt.pred.fa, new)
    }
  }
}




###########################################################################################
#----- Predict 45yr and 10 yr fits (state and coast) on forecast data, 2018-2023  ---------
###########################################################################################
# newdata = wt.pred.sp OR wt.pred.fa

# 45yr, state = wt45.gam.list[[1:2]]
predictions45.state.wt.pred.sp <- predict(wt45.gam.list[[1]], se.fit=TRUE, newdata=wt.pred.sp, type = "response", exclude = "s(Survey)", interval = "predict")
predictions45.state.wt.pred.fa <- predict(wt45.gam.list[[2]], se.fit=TRUE, newdata=wt.pred.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# 45yr, coast = wt45.gam.list[[3:4]]
predictions45.coast.wt.pred.sp <- predict(wt45.gam.list[[3]], se.fit=TRUE, newdata=wt.pred.sp, type = "response", exclude = "s(Survey)", interval = "predict")
predictions45.coast.wt.pred.fa <- predict(wt45.gam.list[[4]], se.fit=TRUE, newdata=wt.pred.fa, type = "response", exclude = "s(Survey)", interval = "predict")

# 10yr, state = wt10.gam.list[[1:2]]
predictions10.state.wt.pred.sp <- predict(wt10.gam.list[[1]], se.fit=TRUE, newdata=wt.pred.sp, type = "response", exclude = "s(Survey)", interval = "predict")
predictions10.state.wt.pred.fa <- predict(wt10.gam.list[[2]], se.fit=TRUE, newdata=wt.pred.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# 10yr, coast = wt10.gam.list[[3:4]]
predictions10.coast.wt.pred.sp <- predict(wt10.gam.list[[3]], se.fit=TRUE, newdata=wt.pred.sp, type = "response", exclude = "s(Survey)", interval = "predict")
predictions10.coast.wt.pred.fa <- predict(wt10.gam.list[[4]], se.fit=TRUE, newdata=wt.pred.fa, type = "response", exclude = "s(Survey)", interval = "predict")


# Create Indiv Datasets of Predictions
predictions45.state.wt.pred.sp.df <- cbind(wt.pred.sp, data.frame(predictions45.state.wt.pred.sp))
predictions45.state.wt.pred.fa.df <- cbind(wt.pred.fa, data.frame(predictions45.state.wt.pred.fa))
predictions45.coast.wt.pred.sp.df <- cbind(wt.pred.sp, data.frame(predictions45.coast.wt.pred.sp))
predictions45.coast.wt.pred.fa.df <- cbind(wt.pred.fa, data.frame(predictions45.coast.wt.pred.fa))
predictions45.wt.list <- list(predictions45.state.wt.pred.sp.df = predictions45.state.wt.pred.sp.df,
                              predictions45.state.wt.pred.fa.df = predictions45.state.wt.pred.fa.df,
                              predictions45.coast.wt.pred.sp.df = predictions45.coast.wt.pred.sp.df,
                              predictions45.coast.wt.pred.fa.df = predictions45.coast.wt.pred.fa.df)
saveRDS(predictions45.wt.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt45-predictions.rds")

predictions10.state.wt.pred.sp.df <- cbind(wt.pred.sp, data.frame(predictions10.state.wt.pred.sp))
predictions10.state.wt.pred.fa.df <- cbind(wt.pred.fa, data.frame(predictions10.state.wt.pred.fa))
predictions10.coast.wt.pred.sp.df <- cbind(wt.pred.sp, data.frame(predictions10.coast.wt.pred.sp))
predictions10.coast.wt.pred.fa.df <- cbind(wt.pred.fa, data.frame(predictions10.coast.wt.pred.fa))
predictions10.wt.list <- list(predictions10.state.wt.pred.sp.df = predictions10.state.wt.pred.sp.df,
                              predictions10.state.wt.pred.fa.df = predictions10.state.wt.pred.fa.df,
                              predictions10.coast.wt.pred.sp.df = predictions10.coast.wt.pred.sp.df,
                              predictions10.coast.wt.pred.fa.df = predictions10.coast.wt.pred.fa.df)
saveRDS(predictions10.wt.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt10-predictions.rds")


predictions45.wt.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt45-predictions.rds")
predictions10.wt.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/wt10-predictions.rds")


###########################################################################################
#----- Figure of forecasted water temp from 45 & 10 yr historic datasets and observations
###########################################################################################

# List of States to plot
states <- c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL")
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent

## STATEWIDE
# Create an empty list to store plots
fig.comb <- list()

# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Spring -", i)]] <- ggplot() + 
    # 45 yr forecast, statewide
    geom_ribbon(data = subset(predictions45.wt.list[[1]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions45.wt.list[[1]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  # Use dark color for line
    # 10 yr forecast, statewide
    geom_ribbon(data = subset(predictions10.wt.list[[1]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions10.wt.list[[1]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  # Use dark color for line
    # Observations
    geom_point(data = subset(data.list[[1]], State ==i),
              aes(x = Year, y = WaterTemp, group = State),
              color = "darkgray", size = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Fall -", i)]] <- ggplot() + 
    # 45 yr forecast, statewide
    geom_ribbon(data = subset(predictions45.wt.list[[2]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions45.wt.list[[2]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  # Use dark color for line
    # 10 yr forecast, statewide
    geom_ribbon(data = subset(predictions10.wt.list[[2]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions10.wt.list[[2]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  # Use dark color for line
    # Observations
    geom_point(data = subset(data.list[[2]], State ==i),
              aes(x = Year, y = WaterTemp, group = State),
              color = "darkgray", size = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
combined_plot <- plot_grid(plotlist = fig.comb, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-watertemp-bystate.png", width=7, height = 9)


## COASTWIDE
# Create an empty list to store plots
fig.comb.coast <- list()

# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb.coast[[paste("Spring -", i)]] <- ggplot() + 
    # 45 yr forecast, statewide
    geom_ribbon(data = subset(predictions45.wt.list[[3]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions45.wt.list[[3]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  # Use dark color for line
    # 10 yr forecast, statewide
    geom_ribbon(data = subset(predictions10.wt.list[[3]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions10.wt.list[[3]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  # Use dark color for line
    # Observations
    geom_point(data = subset(data.list[[1]], State ==i),
               aes(x = Year, y = WaterTemp, group = State),
               color = "darkgray", size = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}

# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb.coast[[paste("Fall -", i)]] <- ggplot() + 
    # 45 yr forecast, statewide
    geom_ribbon(data = subset(predictions45.wt.list[[4]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions45.wt.list[[4]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  # Use dark color for line
    # 10 yr forecast, statewide
    geom_ribbon(data = subset(predictions10.wt.list[[4]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions10.wt.list[[4]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  # Use dark color for line
    # Observations
    geom_point(data = subset(data.list[[2]], State ==i),
               aes(x = Year, y = WaterTemp, group = State),
               color = "darkgray", size = 0.8) +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
}
combined_plot.coast <- plot_grid(plotlist = fig.comb.coast, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot.coast)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-watertemp-coastwide.png", width=7, height = 9)