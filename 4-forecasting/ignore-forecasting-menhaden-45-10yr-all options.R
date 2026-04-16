# 4B-forecasting-menhaden-45-10yr-alloptions.R
######################################
# Janelle L. Morano

# Retrospective forecasting (2018-2023) menhaden presence spring and fall
# 1. Using 45yr Historic dataset: 1972-2017 and 10 yr historic dataset: 2007-2017
#    a. GAM with smooth trends using spline
#    b. GAM with a random walk smooth
#    c. GAM with a random walk as a random effect
#    d. Bayesian GAM with random walk priors
# 2. Combined figure of forecasts from 45 & 10 yr historic datasets

# last updated 25 April 2025
###############################################
###############################################


library(tidyverse)
library(mgcv)
library(cowplot)



# Read in saved RDS of menhaden data, 1972-2023
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
names(data.list)
# "alldata.spring" "alldata.fall"

# Read in predictions of full data 1972-2023 (model #3)
obs <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")

# Read in saved RDS of data for 45yr set, 1972-2017
forecasting45.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting45.data.list")
names(forecasting45.data.list)
# "historic45.sp"       "historic45.fa"       "future45.statewt.sp" "future45.statewt.fa"
# "future45.coastwt.sp" "future45.coastwt.fa"

# Read in saved RDS of data for 10yr set, 2007-2017
forecasting10.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting10.data.list")
names(forecasting10.data.list)
# "historic10.sp"       "historic10.fa"       "future10.statewt.sp" "future10.statewt.fa" "future10.coastwt.sp"
# "future10.coastwt.fa"




###########################################################################################
###########################################################################################
#----- 1. 45yr Historic dataset: 1972-2017
###########################################################################################
###########################################################################################

###########################################################################################
#----- GAM model fit to historic (1972-2017) data, with State water temp trend -----------
###########################################################################################
#----- Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + WaterTemp
historic45.gam.list <- list()
historic45.gam.summaries <- list()
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("sm_", name)
  historic45.gam.list[[new.name]] <- gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp), 
                                  family = binomial(link = "logit"), 
                                  method = "REML", 
                                  data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- Random Walk Smooth (can't use a true random walk with bs = "rw" so approximate with "fs" for factor-smooth interaction with is a smoother per group (OR use Bayesian))
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("rwre_", name)
  historic45.gam.list[[new.name]] <- gam(Presence ~ s(Year, by = State, bs = "fs") + State + s(Survey, bs = "re") + s(WaterTemp), 
                                          family = binomial(link = "logit"), 
                                          method = "REML", 
                                          data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- Random Walk as a Random Effect
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("rwre_", name)
  historic45.gam.list[[new.name]] <- gamm(Presence ~ s(Year, by = State) + State,
            random = list(Survey = ~1),
            correlation = corAR1(form = ~ Year | State),
            data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}


#----- Bayesian GAM with random walk prior
library(brms)
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("rwre_", name)
  historic45.gam.list[[new.name]] <- brm(WaterTemp ~ rw(Year) + State,
                                         data = your_data,
                                         family = gaussian())


historic45.gam.summaries[[1]]
historic45.gam.summaries[[2]]








#----- Save model runs as RDS
saveRDS(historic45.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/historic45-GAM-results.rds")
saveRDS(historic45.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/historic45-GAM-summaries.rds")




###########################################################################################
#----- Forecasting predictions, 2018-2023 ----------------------------------------------
###########################################################################################

#----- With state trend water temp
forecast45state.predictions.spring <- predict(historic45.gam.list[[1]], se.fit=TRUE, newdata=forecasting45.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45state.predictions.spring.df <- cbind(forecasting45.data.list[[3]], data.frame(forecast45state.predictions.spring))

forecast45state.predictions.fall <- predict(historic45.gam.list[[2]], se.fit=TRUE, newdata=forecasting45.data.list[[4]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45state.predictions.fall.df <- cbind(forecasting45.data.list[[4]], data.frame(forecast45state.predictions.fall))

#----- With coastwide trend water temp
forecast45coast.predictions.spring <- predict(historic45.gam.list[[1]], se.fit=TRUE, newdata=forecasting45.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45coast.predictions.spring.df <- cbind(forecasting45.data.list[[5]], data.frame(forecast45coast.predictions.spring))

forecast45coast.predictions.fall <- predict(historic45.gam.list[[2]], se.fit=TRUE, newdata=forecasting45.data.list[[4]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45coast.predictions.fall.df <- cbind(forecasting45.data.list[[6]], data.frame(forecast45coast.predictions.fall))

# List dfs
forecasting45.predictions.list <- list(
  forecast45state.predictions.spring.df = forecast45state.predictions.spring.df,
  forecast45state.predictions.fall.df = forecast45state.predictions.fall.df,
  forecast45coast.predictions.spring.df = forecast45coast.predictions.spring.df,
  forecast45coast.predictions.fall.df = forecast45coast.predictions.fall.df
)
# Save list
saveRDS(forecasting45.predictions.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting45.predictions.list.rdss")



###########################################################################################
#----- Plot the forecast and observations, state by state ---------------------------------
###########################################################################################

# Read in forecasts
forecasting45.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting45.predictions.list.rds")

# Bring in Observations: predictions of full data 1972-2023 (model #5)
obs <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/PA-GAM-predictions.rds")


#----- State by state: spring and fall ------------------------------------
# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent

# List of States to plot
states <- unique(na.omit(forecasting45.data.list[[1]]$State))
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Create an empty list to store plots
fig <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Spring -", i)]] <- ggplot() + 
    # Start with statewide temp
    geom_ribbon(data = subset(forecasting45.predictions.list[[1]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[1]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # # coastwide trend water temp
    # geom_line(data = subset(forecasting45.predictions.list[[3]], State == i), 
    #           aes(x = Year, y = fit, group = State), 
    #           color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  
    # observed
    geom_line(data = subset(obs[[1]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black", linetype = "dotdash") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) + # 6 with labels and title
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    # ylim(c(0, 1)) +
    xlim(c(2018, 2023)) +
    labs(x = " ", y = " ") #Probability of Presence won't fit
    # ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Fall -", i)]] <- ggplot() + 
    # state trend water temp
    geom_ribbon(data = subset(forecasting45.predictions.list[[2]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[2]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # # coastwide trend water temp
    # geom_line(data = subset(forecasting45.predictions.list[[4]], State == i), 
    #           aes(x = Year, y = fit, group = State), 
    #           color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  
    # observed
    geom_line(data = subset(obs[[2]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black", linetype = "dotdash") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    # ylim(c(0, 1)) +
    xlim(c(2018, 2023)) +
    labs(x = " ", y = " ") #Probability of Presence won't fit
    # ggtitle(paste("Fall -", i))  # Title with state name
}

combined_plot <- plot_grid(plotlist = fig, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-45yr-bystate-2018-2023.png", width=7, height = 9)







###########################################################################################
###########################################################################################
#----- 2. 10 yr historic dataset: 2007-2017
###########################################################################################
###########################################################################################


###########################################################################################
#----- GAM model fit to historic (2010-2020) data -----------------------------------
###########################################################################################
historic10.gam.list <- list()
historic10.gam.summaries <- list()
for (name in names(forecasting10.data.list)[1:2]) {
  historic10.gam.list[[name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp), 
                                    family = binomial(link = "logit"), 
                                    method = "ML", 
                                    data = forecasting10.data.list[[name]])
  historic10.gam.summaries[[name]] <- summary(historic10.gam.list[[name]])
}

historic10.gam.summaries[[1]]
historic10.gam.summaries[[2]]


#----- Save model runs as RDS
saveRDS(historic10.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/historic10-GAM-results.rds")
saveRDS(historic10.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/historic10-GAM-summaries.rds")




###########################################################################################
#----- Forecasting predictions, 2018-2023 ----------------------------------------------
###########################################################################################

#----- With state trend water temp
forecast10state.predictions.spring <- predict(historic10.gam.list[[1]], se.fit=TRUE, newdata=forecasting10.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast10state.predictions.spring.df <- cbind(forecasting10.data.list[[3]], data.frame(forecast10state.predictions.spring))

forecast10state.predictions.fall <- predict(historic10.gam.list[[2]], se.fit=TRUE, newdata=forecasting10.data.list[[4]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast10state.predictions.fall.df <- cbind(forecasting10.data.list[[4]], data.frame(forecast10state.predictions.fall))

#----- With coastwide trend water temp
forecast10coast.predictions.spring <- predict(historic10.gam.list[[1]], se.fit=TRUE, newdata=forecasting10.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast10coast.predictions.spring.df <- cbind(forecasting10.data.list[[5]], data.frame(forecast10coast.predictions.spring))

forecast10coast.predictions.fall <- predict(historic10.gam.list[[2]], se.fit=TRUE, newdata=forecasting10.data.list[[4]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast10coast.predictions.fall.df <- cbind(forecasting10.data.list[[6]], data.frame(forecast10coast.predictions.fall))

# List dfs
forecasting10.predictions.list <- list(
  forecast10state.predictions.spring.df = forecast10state.predictions.spring.df,
  forecast10state.predictions.fall.df = forecast10state.predictions.fall.df,
  forecast10coast.predictions.spring.df = forecast10coast.predictions.spring.df,
  forecast10coast.predictions.fall.df = forecast10coast.predictions.fall.df
)
# Save list
saveRDS(forecasting10.predictions.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting10.predictions.list.rds")

forecasting10.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting10.predictions.list.rds")



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
states <- unique(na.omit(forecasting10.data.list[[1]]$State))
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Create an empty list to store plots
fig <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Spring -", i)]] <- ggplot() + 
    # Start with statewide temp
    geom_ribbon(data = subset(forecasting10.predictions.list[[1]], State == i),
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting10.predictions.list[[1]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # # coastwide trend water temp
    # geom_line(data = subset(forecasting10.predictions.list[[3]], State == i), 
    #           aes(x = Year, y = fit, group = State), 
    #           color = pal8[which(states == i)], linewidth = 1.2) +  
    # observed
    geom_line(data = subset(obs[[1]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black", linetype = "dotdash") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # labs(x = " ", y = " ") #Probability of Presence doesn't fit
  # ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig[[paste("Fall -", i)]] <- ggplot() + 
    # state trend water temp
    geom_ribbon(data = subset(forecasting10.predictions.list[[2]], State == i),
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting10.predictions.list[[2]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    # # coastwide trend water temp
    # geom_line(data = subset(forecasting10.predictions.list[[4]], State == i), 
    #           aes(x = Year, y = fit, group = State), 
    #           color = pal8[which(states == i)], linewidth = 1.2) +  
    # observed
    geom_line(data = subset(obs[[2]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "black", linetype = "dotdash") +
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # labs(x = " ", y = " ") +
  # ggtitle(paste("Fall -", i))  # Title with state name
}

combined_plot <- plot_grid(plotlist = fig, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-10yr-bystate-2018-2023.png", width=7, height = 9)





###########################################################################################
###########################################################################################
#----- 3. Combined figure of forecasts from 45 & 10 yr historic datasets
###########################################################################################
###########################################################################################


# Read in forecasts
forecasting45.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting45.predictions.list.rds")
forecasting10.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/forecasting10.predictions.list.rds")



# List of States to plot
states <- c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL")
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent


# Create an empty list to store plots
fig.comb <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Spring -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[1]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0),
               color = "black", size = 0.5) +
    # Observations, Model fit
    geom_line(data = subset(obs[[1]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "gray60", linewidth = 0.8) +
    # 45 yr forecast
    geom_ribbon(data = subset(forecasting45.predictions.list[[1]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[1]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dashed") +  # Use dark color for line
    # 10 yr forecast
    geom_ribbon(data = subset(forecasting10.predictions.list[[1]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting10.predictions.list[[1]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  # Use dark color for line
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    # xlim(c(2018, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # labs(x = " ", y = " ") #Probability of Presence doesn't fit
  # ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig.comb[[paste("Fall -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[2]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0), 
               color = "black", size = 0.5) +
    # Observations, Model fit
    geom_line(data = subset(obs[[2]], State ==i),
              aes(x = Year, y = fit, group = State),
              color = "gray50", linewidth = 0.8) +
    # 45 yr forecast
    geom_ribbon(data = subset(forecasting45.predictions.list[[2]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  
    geom_line(data = subset(forecasting45.predictions.list[[2]], State == i), 
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1, linetype = "dashed") + 
    # 10 yr forecast
    geom_ribbon(data = subset(forecasting10.predictions.list[[2]], State == i),
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  
    geom_line(data = subset(forecasting10.predictions.list[[2]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.2, linetype = "dotted") +  
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    # xlim(c(2018, 2023)) +
    theme(
      # axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # labs(x = " ", y = " ") +
  # ggtitle(paste("Fall -", i))  # Title with state name
}

fig.comb.plot <- plot_grid(plotlist = fig.comb, ncol = 2, byrow = FALSE)
# Print the combined plot
print(fig.comb.plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-45-10yr-bystate-2018-2023.png", width=9, height = 9)
