# forecasting-menhaden-2016-2023.R
######################################
# Janelle L. Morano

# Retrospective forecasting state and federal presence spring and fall data and break up into:
# Historic dataset: 1972-2015
# Forecasting dataset: 2016-2023
# GAM
# Compare
# Observed presence per region to Predicted presence per region


# last updated 2 April 2025
###############################################
###############################################


library(tidyverse)
library(mgcv)
library(cowplot)


# Read in saved RDS
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")

# Create training (historic) data, 1972-2015
historic.sp <- data.list[[1]] %>%
  filter(Year >=1972 & Year <= 2015) %>%
  arrange(Year)
historic.fa <- data.list[[2]] %>%
  filter(Year >=1972 & Year <= 2015) %>%
  arrange(Year)


# Create future data for predictions, 2016-2023
#-- Forecast Spring
future.spring <- data.frame()
for (i in seq(2016, 2023)) {
  for (j in unique(data.list$alldata.spring$State)) {
    for(k in unique(data.list$alldata.spring$Survey)) {
      new <- data.frame(Year = i,
                        State = j,
                        Survey = k,
                        WaterTemp = median(data.list$alldata.spring$WaterTemp[data.list$alldata.spring$Year == i & data.list$alldata.spring$State == j], na.rm=TRUE))
      future.spring <- rbind(future.spring, new)
    }
  }
}

#-- Forecast Fall
future.fall <- data.frame()
for (i in seq(2016, 2023)) {
  for (j in unique(data.list$alldata.fall$State)) {
    for(k in unique(data.list$alldata.fall$Survey)) {
      new <- data.frame(Year = i,
                        State = j,
                        Survey = k,
                        WaterTemp = median(data.list$alldata.fall$WaterTemp[data.list$alldata.fall$Year == i & data.list$alldata.fall$State == j], na.rm=TRUE))
      future.fall <- rbind(future.fall, new)
    }
  }
}


#----- Create a list of the Historic and Forecast datasets
forecasting.data.list <- list(
  historic.sp = historic.sp,
  historic.fa = historic.fa,
  future.spring = future.spring,
  future.fall = future.fall)



###########################################################################################
#----- GAM model fit to historic (1972-2015) data --------------------------------------------------
###########################################################################################
#----- Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
historic.gam.list <- list()
historic.gam.summaries <- list()
for (name in names(forecasting.data.list)[1:2]) {
  historic.gam.list[[name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp), 
                                  family = binomial(link = "logit"), 
                                  method = "ML", 
                                  data = forecasting.data.list[[name]])
  historic.gam.summaries[[name]] <- summary(historic.gam.list[[name]])
}

historic.gam.summaries[[1]]
historic.gam.summaries[[2]]




###########################################################################################
#----- Forecasting predictions, 2016-2023 ----------------------------------------------
###########################################################################################
forecast.predictions.spring <- predict(historic.gam.list[[1]], se.fit=TRUE, newdata=forecasting.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast.predictions.spring.df <- cbind(forecasting.data.list[[3]], data.frame(forecast.predictions.spring))

forecast.predictions.fall <- predict(historic.gam.list[[2]], se.fit=TRUE, newdata=forecasting.data.list[[4]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast.predictions.fall.df <- cbind(forecasting.data.list[[4]], data.frame(forecast.predictions.fall))



# Calculate residuals (difference between observed and predictions)
residuals <- forecasting.data.list$observed.sp$Presence - forecast.predictions.spring.df$fit





###########################################################################################
#----- Plot the forecast and observations, state by state ---------------------------------
###########################################################################################

# Bring in predictions of full data 1972-2023 (model #3)
obs <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")


#----- State by state: spring and fall ------------------------------------
# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent

# List of States to plot
states <- unique(na.omit(forecast.predictions.spring.df$State))
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Create an empty list to store plots
fig <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Spring -", i)]] <- ggplot() + 
    geom_ribbon(data = subset(forecast.predictions.spring.df, State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecast.predictions.spring.df, State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # observed
    geom_line(data = subset(obs$predictions.pa.gam.3.spring.df, State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    # ylim(c(0, 1)) +
    xlim(c(2016, 2023)) +
    labs(x = " ", y = "Presence") +
    ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Fall -", i)]] <- ggplot() + 
    geom_ribbon(data = subset(forecast.predictions.fall.df, State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecast.predictions.fall.df, State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # observed
    geom_line(data = subset(obs$predictions.pa.gam.3.fall.df, State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    # ylim(c(0, 1)) +
    xlim(c(2016, 2023)) +
    labs(x = " ", y = "Presence") +
    ggtitle(paste("Fall -", i))  # Title with state name
}

combined_plot <- plot_grid(plotlist = fig, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/fig-forecast-GAM-bystate-2016-2023.png", width=7, height = 9)





###########################################################################################
#----- Graph Annual proportion by state ------------------------------------
###########################################################################################

#----- Scale the maximum annual fits by state and year

# Spring
annual.max.sp <- forecast.predictions.spring.df  |>
  group_by(Year) |>
  summarise(annual.max.fit = max(fit, na.rm = TRUE))

forecast.predictions.spring.df <- forecast.predictions.spring.df %>%
  left_join(annual.max.sp, by = "Year") 
forecast.predictions.spring.df <- forecast.predictions.spring.df %>%
  mutate(prop.fit = fit/ annual.max.fit)


# Fall
annual.max.fa <- forecast.predictions.fall.df |>
  group_by(Year) |>
  summarise(annual.max.fit = max(fit, na.rm = TRUE))
forecast.predictions.fall.df <- forecast.predictions.fall.df %>%
  left_join(annual.max.fa, by = "Year") 
forecast.predictions.fall.df <- forecast.predictions.fall.df %>%
  mutate(prop.fit = fit/ annual.max.fit)



#----- Plot the proportions
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
forecast.predictions.spring.df$State <- factor(forecast.predictions.spring.df$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
forecast.predictions.fall.df$State <- factor(forecast.predictions.fall.df$State, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

f2a1 <- ggplot() +
  geom_bar(data = forecast.predictions.spring.df, aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x= " ", y = "Proportion of Total Presence") +
  theme(legend.position = "none")

f2a2 <- ggplot() +
  geom_bar(data = forecast.predictions.fall.df, aes(x = Year, y = prop.fit, fill = State), position = "fill", stat = "identity") +
  scale_fill_manual(values = pal8) +
  theme_classic() +
  labs(x= " ", y = "Proportion of Total Presence") +
  theme(legend.position = "none")


plot_grid(f2a1, f2a2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/fig-stateprop-2016-2023.png", width=7, height = 4)
