# forecasting-menhaden-45-10yr-2018-2023.R
######################################
# Janelle L. Morano

# Retrospective forecasting (2018-2023) menhaden presence spring and fall
# 1. 45yr Historic dataset: 1972-2017
# 2. 10 yr historic dataset: 2007-2017
# 3. Combined figure of forecasts from 45 & 10 yr historic datasets
# 4. Forecast of GLM model and combined figure of forecasts

# last updated 12 June 2025 (dropped 10-yr historic training)
###############################################
###############################################


library(tidyverse)
library(mgcv)
library(stats)
library(cowplot)


# Read in saved RDS of menhaden data, 1972-2023
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
names(data.list)
# "alldata.spring" "alldata.fall"


# Read in saved RDS of data for 45yr set, 1972-2017
forecasting45.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.data.list")
names(forecasting45.data.list)
# "historic45.sp"      "historic45.fa"      "future45.lowwt.sp"  "future45.lowwt.fa"  "future45.highwt.sp"
# "future45.highwt.fa"

# # Read in saved RDS of data for 10yr set, 2007-2017
# forecasting10.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.data.list")
# names(forecasting10.data.list)
# # "historic10.sp"      "historic10.fa"      "future10.lowwt.sp"  "future10.lowwt.fa"  "future10.highwt.sp"
# # "future10.highwt.fa"




###########################################################################################
###########################################################################################
#----- 1. 45yr Historic dataset: 1972-2017
###########################################################################################
###########################################################################################

historic45.gam.list <- list()
historic45.gam.summaries <- list()

#----- a. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("full_", name)
  historic45.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp), 
                                  family = binomial(link = "logit"), 
                                  method = "REML", 
                                  data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- b. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("nodepthwt_", name)
  historic45.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re"), 
                                    family = binomial(link = "logit"), 
                                    method = "REML", 
                                    data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}


#----- GLMER: Presence ~ Year * State + (1|Survey) #This does not converge
#----- GLMER: Presence ~ Year + State + (1|Survey) #This does not converge
# (Adding an interaction or a squared Year fits too close to 0/1)

#----- c. GLM: Presence ~ Year + State 
library(lme4)
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("glm_", name)
  historic45.gam.list[[new.name]] = glm(Presence ~ Year + State, 
                                          family = binomial, 
                                        data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- d. GLM: Presence ~ Year + State + WaterTemp
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("glmwt_", name)
  historic45.gam.list[[new.name]] = glm(Presence ~ Year + State + WaterTemp, 
                                        family = binomial, 
                                        data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- e. GLM: Presence ~ Year + State + Depth (Adding depth fits too close to 0/1)
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("glmdp_", name)
  historic45.gam.list[[new.name]] = glm(Presence ~ Year + State + Depth, 
                                        family = binomial, 
                                        data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}

#----- f. GLM: Presence ~ Year + State + WaterTemp + Depth (Adding depth fits too close to 0/1)
for (name in names(forecasting45.data.list)[1:2]) {
  new.name <- paste0("glmwtdp_", name)
  historic45.gam.list[[new.name]] = glm(Presence ~ Year + State + WaterTemp + Depth, 
                                        family = binomial, 
                                        data = forecasting45.data.list[[name]])
  historic45.gam.summaries[[new.name]] <- summary(historic45.gam.list[[new.name]])
}



historic45.gam.summaries[[1]] #a
historic45.gam.summaries[[2]] #b
historic45.gam.summaries[[3]] #c
historic45.gam.summaries[[4]] #d
historic45.gam.summaries[[5]] #e
historic45.gam.summaries[[6]] #f

historic45.gam.summaries[[1]]$dev.expl
# 0.5154162
historic45.gam.summaries[[2]]$dev.expl
# 0.5741607
historic45.gam.summaries[[3]]$dev.expl
# 0.5070307
historic45.gam.summaries[[4]]$dev.expl
# 0.5527195
historic45.gam.summaries[[5]]$dev.expl
# 0.5527195

# Get model coefficients to compare
library(modelsummary)
modelsummary(historic45.gam.list, output = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/gam-historic45.gam.table.docx")


summary(m[[1]])$dev.expl


#----- Save model runs as RDS
saveRDS(historic45.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic45-GAM-results.rds")
saveRDS(historic45.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic45-GAM-summaries.rds")


historic45.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic45-GAM-results.rds")
historic45.gam.summaries <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic45-GAM-summaries.rds")


#----- Cross-validation
# k <- 10
# folds <- sample(rep(1:k, length.out = n))
# 
# glm.errors <- gam.errors <- numeric(k)
# 
# for (i in 1:k) {
#   train.data <- data.list[[1]][folds != i, ]
#   test.data  <- data.list[[1]][folds == i, ]
#   
#   # GAM
#   gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp) + s(Depth), 
#       family = binomial(link = "logit"), 
#       method = "REML", 
#       data = test.data)
#   gam.pred <- predict(gam.fit, newdata = test_data)
#   gam.errors[i] <- mean((test.data$y - gam_pred)^2)
#   
#   # GLM
#   glm_fit <- glm(Presence ~ Year + State + WaterTemp, 
#                  family = binomial, data = test.data)
#   glm.pred <- predict(glm_fit, newdata = test.data)
#   glm.errors[i] <- mean((test_data$y - glm_pred)^2)
#   
# }
# 
# 
# # Compare average MSE
# mean(glm.errors)
# mean(gam.errors)



###########################################################################################
#----- Forecasting predictions, 2018-2023 using glm without temp (c) and with (d) ----------------
###########################################################################################

#----- Without water temp (historic45.gam.list[[5,6]] = "glm_historic45.sp")
forecast45nowt.predictions.spring <- predict(historic45.gam.list[[5]], se.fit=TRUE, newdata=forecasting45.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45nowt.predictions.spring.df <- cbind(forecasting45.data.list[[3]], data.frame(forecast45nowt.predictions.spring))

# MDGill not in original fit, so modify forecasting45.data.list[[4]]
new45.fa <- forecasting45.data.list[[4]] |> filter(Survey != "MDGill")
# Now predict
forecast45nowt.predictions.fall <- predict(historic45.gam.list[[6]], se.fit=TRUE, newdata=new45.fa, type = "response", exclude = "s(Survey)", interval = "predict")
forecast45nowt.predictions.fall.df <- cbind(new45.fa, data.frame(forecast45nowt.predictions.fall))

#----- With low water temp (historic45.gam.list[[7, 8]] = "glm_historic45.sp")
forecast45low.predictions.spring <- predict(historic45.gam.list[[7]], se.fit=TRUE, newdata=forecasting45.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45low.predictions.spring.df <- cbind(forecasting45.data.list[[3]], data.frame(forecast45low.predictions.spring))

# MDGill not in original fit, so modify forecasting45.data.list[[4]]
new45.fa <- forecasting45.data.list[[4]] |> filter(Survey != "MDGill")
# Now predict
forecast45low.predictions.fall <- predict(historic45.gam.list[[8]], se.fit=TRUE, newdata=new45.fa, type = "response", exclude = "s(Survey)", interval = "predict")
forecast45low.predictions.fall.df <- cbind(new45.fa, data.frame(forecast45low.predictions.fall))

#----- With high water temp
forecast45high.predictions.spring <- predict(historic45.gam.list[[7]], se.fit=TRUE, newdata=forecasting45.data.list[[5]], type = "response", exclude = "s(Survey)", interval = "predict")
forecast45high.predictions.spring.df <- cbind(forecasting45.data.list[[5]], data.frame(forecast45high.predictions.spring))

# MDGill not in original fit, so use new45.fa
forecast45high.predictions.fall <- predict(historic45.gam.list[[8]], se.fit=TRUE, newdata=new45.fa, type = "response", exclude = "s(Survey)", interval = "predict")
forecast45high.predictions.fall.df <- cbind(new45.fa, data.frame(forecast45high.predictions.fall))

# List dfs
forecasting45.predictions.list <- list(
  forecast45nowt.predictions.spring.df = forecast45nowt.predictions.spring.df,
  forecast45nowt.predictions.fall.df = forecast45nowt.predictions.fall.df,
  forecast45low.predictions.spring.df = forecast45low.predictions.spring.df,
  forecast45low.predictions.fall.df = forecast45low.predictions.fall.df,
  forecast45high.predictions.spring.df = forecast45high.predictions.spring.df,
  forecast45high.predictions.fall.df = forecast45high.predictions.fall.df
)
# Save list
saveRDS(forecasting45.predictions.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.predictions.list.rds")

forecasting45.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.predictions.list.rds")





###########################################################################################
###########################################################################################
#----- 4. Figure of forecasts from 45 historic dataset
# Using the GLM: Presence ~ Year + State
# Using the GLM: Presence ~ Year + State + WaterTemp
###########################################################################################
###########################################################################################


# Read full data 1972-2023 
data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")

ave.presence.sp <- data.list[[1]] |>
  group_by(State, Year) |>
  summarise(ave.pres = mean(Presence))

ave.presence.fa <- data.list[[2]] |>
  group_by(State, Year) |>
  summarise(ave.pres = mean(Presence))

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
               position = position_jitter(width = 0.2, height = 0.2),
               color = "gray90", size = 0.25) +
    # Observations, Average fit
    geom_line(data = subset(ave.presence.sp, State ==i),
              aes(x = Year, y = ave.pres, group = State),
              color = "gray60", linewidth = 0.25) +
    
    # 45 yr forecast, without temp in model
    geom_ribbon(data = subset(forecasting45.predictions.list[[1]], State == i),
                aes(x = Year,
                    ymin = pmax(0, fit - (1.96 * se.fit)),
                    ymax = pmin(1, fit + (1.96 * se.fit)),
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[1]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.5, linetype = "1111") +  # Use dark color for line
    
    # 45 yr forecast, low temp
    geom_ribbon(data = subset(forecasting45.predictions.list[[3]], State == i),
                aes(x = Year,
                    ymin = pmax(0, fit - (1.96 * se.fit)),
                    ymax = pmin(1, fit + (1.96 * se.fit)),
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[3]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.5, linetype = "3131") +  # Use dark color for line
    
    # 45 yr forecast, high temp
    geom_ribbon(data = subset(forecasting45.predictions.list[[5]], State == i), 
                aes(x = Year, 
                    ymin = pmax(0, fit - (1.96 * se.fit)), 
                    ymax = pmin(1, fit + (1.96 * se.fit)), 
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[5]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.5, linetype = "2121") +  # Use dark color for line
    
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    xlim(c(1972, 2023)) +
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
               position = position_jitter(width = 0.2, height = 0.2),
               color = "gray90", size = 0.25) +
    # Observations, Average fit
    geom_line(data = subset(ave.presence.sp, State ==i),
              aes(x = Year, y = ave.pres, group = State),
              color = "gray60", linewidth = 0.25) +
    
    # 45 yr forecast, without temp in model
    geom_ribbon(data = subset(forecasting45.predictions.list[[2]], State == i),
                aes(x = Year,
                    ymin = pmax(0, fit - (1.96 * se.fit)),
                    ymax = pmin(1, fit + (1.96 * se.fit)),
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[2]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.5, linetype = "1212") +  # Use dark color for line
    
    # # 45 yr forecast, low temp
    # geom_ribbon(data = subset(forecasting45.predictions.list[[4]], State == i),
    #             aes(x = Year,
    #                 ymin = pmax(0, fit - (1.96 * se.fit)),
    #                 ymax = pmin(1, fit + (1.96 * se.fit)),
    #                 group = State),
    #             fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    # geom_line(data = subset(forecasting45.predictions.list[[4]], State == i),
    #           aes(x = Year, y = fit, group = State),
    #           color = pal8[which(states == i)], linewidth = 1.5, linetype = "3131") +  # Use dark color for line
    # 
    # 45 yr forecast, high temp
    geom_ribbon(data = subset(forecasting45.predictions.list[[6]], State == i),
                aes(x = Year,
                    ymin = pmax(0, fit - (1.96 * se.fit)),
                    ymax = pmin(1, fit + (1.96 * se.fit)),
                    group = State),
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(forecasting45.predictions.list[[6]], State == i),
              aes(x = Year, y = fit, group = State),
              color = pal8[which(states == i)], linewidth = 1.5, linetype = "2121") +  # Use dark color for line

    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 8)) +
    ylim(c(0, 1)) +
    xlim(c(1972, 2023)) +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # labs(x = " ", y = " ") +
  # ggtitle(paste("Fall -", i))  # Title with state name
}

fig.comb.plot <- plot_grid(plotlist = fig.comb, ncol = 2, byrow = FALSE)
# Print the combined plot
print(fig.comb.plot)
#ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-45-10yr-GLMHitemp=bystate-2018-2023.png", dpi = 400, width=9, height = 9)



############################################################################
# I'm pretty certain everything from here on can be ignored, but keeping for now

# ###########################################################################################
# ###########################################################################################
# #----- 2. 10 yr historic dataset: 2007-2017
# ###########################################################################################
# ###########################################################################################
# 
# 
# ###########################################################################################
# #----- GAM model fit to historic (2010-2020) data -----------------------------------
# ###########################################################################################
# historic10.gam.list <- list()
# historic10.gam.summaries <- list()
# 
# #----- a. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp) + s(Depth)
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("full_", name)
#   historic10.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp) + s(Depth), 
#                                     family = binomial(link = "logit"), 
#                                     method = "ML", 
#                                     data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# #----- b. Presence ~ s(Year, by = State) + State + s(Survey, bs = "re")
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("nodepthwt_", name)
#   historic10.gam.list[[new.name]] = gam(Presence ~ s(Year, by = State) + State + s(Survey, bs = "re"), 
#                                         family = binomial(link = "logit"), 
#                                         method = "ML", 
#                                         data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# #----- GLMER: Presence ~ Year * State + (1|Survey) #This does not converge
# #----- GLMER: Presence ~ Year + State + (1|Survey) #This does not converge
# 
# #----- c. GLM: Presence ~ Year + State 
# library(lme4)
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("glm_", name)
#   historic10.gam.list[[new.name]] = glm(Presence ~ Year + State, #Year + Year:State
#                                         family = binomial, 
#                                         data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# #----- d. GLM: Presence ~ Year + State + WaterTemp (adding Depth results in probabilities nearly 0 or 1, so dropped)
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("glmwt_", name)
#   historic10.gam.list[[new.name]] = glm(Presence ~ Year + State + WaterTemp, 
#                                         family = binomial, 
#                                         data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# #----- GLM: Presence ~ Year + (Year^2) + Year * State
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("glmY2_", name)
#   historic10.gam.list[[new.name]] = glm(Presence ~ Year + (Year^2) + Year * State, #Year + Year:State
#                                         family = binomial, 
#                                         data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# #----- GLM: Presence ~ Year + (Year^2) + Year * State + WaterTemp (adding Depth results in probabilities nearly 0 or 1, so dropped)
# for (name in names(forecasting10.data.list)[1:2]) {
#   new.name <- paste0("glmwtY2_", name)
#   historic10.gam.list[[new.name]] = glm(Presence ~ Year + (Year^2) + Year * State + WaterTemp, 
#                                         family = binomial, 
#                                         data = forecasting10.data.list[[name]])
#   historic10.gam.summaries[[new.name]] <- summary(historic10.gam.list[[new.name]])
# }
# 
# historic10.gam.summaries[[1]]
# historic10.gam.summaries[[2]]
# 
# historic10.gam.summaries[[1]]$dev.expl
# # 0.4965003
# historic10.gam.summaries[[2]]$dev.expl
# # 0.5510223
# historic10.gam.summaries[[3]]$dev.expl
# # 0.4949526
# historic10.gam.summaries[[4]]$dev.expl
# # 0.5503581
# 
# 
# library(modelsummary)
# historic10.gam.table <- modelsummary(historic10.gam.list, output = "data.frame")
# write.csv(historic10.gam.table, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/gam-historic10.gam.table.csv")
# 
# # Print list of model formulas (only the spring models since the falls are duplicates)
# for (i in seq(1, length(historic10.gam.list), by = 2)) {
#   cat("Model", i, "formula:\n")
#   print(formula(historic10.gam.list[[i]]))
#   cat("\n")
# }
# 
# 
# #----- Save model runs as RDS
# saveRDS(historic10.gam.list, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic10-GAM-results.rds")
# saveRDS(historic10.gam.summaries, file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic10-GAM-summaries.rds")
# 
# historic10.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic10-GAM-results.rds")
# historic10.gam.summaries <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic10-GAM-summaries.rds")
# 
# modelsummary(historic10.gam.list, output = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/gam-historic10.gam.table.docx")
# 
# 
# 
# ###########################################################################################
# #----- Forecasting predictions, 2018-2023 ----------------------------------------------
# ###########################################################################################
# 
# # Using Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + s(WaterTemp) + s(Depth)
# #----- With low water temp
# forecast10low.predictions.spring <- predict(historic10.gam.list[[1]], se.fit=TRUE, newdata=forecasting10.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
# forecast10low.predictions.spring.df <- cbind(forecasting10.data.list[[3]], data.frame(forecast10low.predictions.spring))
# 
# # MDGill not in original fit, so modify forecasting10.data.list[[4]]
# new10.fa <- forecasting10.data.list[[4]] |> filter(Survey != "MDGill")
# # Now predict
# forecast10low.predictions.fall <- predict(historic10.gam.list[[2]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# forecast10low.predictions.fall.df <- cbind(new10.fa, data.frame(forecast10low.predictions.fall))
# 
# #----- With high water temp
# forecast10high.predictions.spring <- predict(historic10.gam.list[[1]], se.fit=TRUE, newdata=forecasting10.data.list[[5]], type = "response", exclude = "s(Survey)", interval = "predict")
# forecast10high.predictions.spring.df <- cbind(forecasting10.data.list[[5]], data.frame(forecast10high.predictions.spring))
# 
# # MDGill not in original fit, so use new10.fa
# forecast10high.predictions.fall <- predict(historic10.gam.list[[2]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# forecast10high.predictions.fall.df <- cbind(new10.fa, data.frame(forecast10high.predictions.fall))
# 
# 
# # List dfs
# forecasting10.predictions.list <- list(
#   forecast10low.predictions.spring.df = forecast10low.predictions.spring.df,
#   forecast10low.predictions.fall.df = forecast10low.predictions.fall.df,
#   forecast10high.predictions.spring.df = forecast10high.predictions.spring.df,
#   forecast10high.predictions.fall.df = forecast10high.predictions.fall.df
# )
# # Save list
# saveRDS(forecasting10.predictions.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.predictions.list.rds")
# 
# forecasting10.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.predictions.list.rds")
# 
# 
# 



# ###########################################################################################
# ###########################################################################################
# #----- 3. Combined figure of forecasts from 45 & 10 yr historic datasets
# # Using the 1st GAM model
# ###########################################################################################
# ###########################################################################################
# 
# 
# # Read in forecasts
# forecasting45.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.predictions.list.rds")
# forecasting10.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.predictions.list.rds")
# 
# # Read full data 1972-2023 
# data.list <- readRDS("/Users/janellemorano/Git/menhaden-dist-ms/data/menhaden.data.list.rds")
# 
# ave.presence.sp <- data.list[[1]] |>
#   group_by(State, Year) |>
#   summarise(ave.pres = mean(Presence))
# 
# ave.presence.fa <- data.list[[2]] |>
#   group_by(State, Year) |>
#   summarise(ave.pres = mean(Presence))
# 
# # List of States to plot
# states <- c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL")
# states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))
# 
# # Color palette for North to South States AKA Dark to Light
# pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
# pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent
# 
# 
# # Create an empty list to store plots
# fig.comb <- list()
# 
# 
# # Spring
# for (i in states) {
#   # Plot using dark color palette for geom_line and light color palette for geom_ribbon
#   fig.comb[[paste("Spring -", i)]] <- ggplot() + 
#     # Observations, Presence data
#     geom_point(data = subset(data.list[[1]], State ==i),
#                aes(x = Year, y = Presence),
#                position = position_jitter(width = 0.2, height = 0.2),
#                color = "gray80", size = 0.25) +
#     # Observations, Average fit
#     geom_line(data = subset(ave.presence.sp, State ==i),
#               aes(x = Year, y = ave.pres, group = State),
#               color = "gray60", linewidth = 0.5) +
#     # # 45 yr forecast, low temp
#     # geom_ribbon(data = subset(forecasting45.predictions.list[[1]], State == i), 
#     #             aes(x = Year, 
#     #                 ymin = pmax(0, fit - (1.96 * se.fit)), 
#     #                 ymax = pmin(1, fit + (1.96 * se.fit)), 
#     #                 group = State),
#     #             fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
#     # geom_line(data = subset(forecasting45.predictions.list[[1]], State == i), 
#     #           aes(x = Year, y = fit, group = State), 
#     #           color = pal8[which(states == i)], linewidth = 1.5, linetype = "3131") +  # Use dark color for line
#     # 45 yr forecast, high temp
#     geom_ribbon(data = subset(forecasting45.predictions.list[[3]], State == i), 
#                 aes(x = Year, 
#                     ymin = pmax(0, fit - (1.96 * se.fit)), 
#                     ymax = pmin(1, fit + (1.96 * se.fit)), 
#                     group = State),
#                 fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
#     geom_line(data = subset(forecasting45.predictions.list[[3]], State == i), 
#               aes(x = Year, y = fit, group = State), 
#               color = pal8[which(states == i)], linewidth = 1.5, linetype = "2121") +  # Use dark color for line
#     # # 10 yr forecast, low temp
#     # geom_ribbon(data = subset(forecasting10.predictions.list[[1]], State == i),
#     #             aes(x = Year, 
#     #                 ymin = pmax(0, fit - (1.96 * se.fit)), 
#     #                 ymax = pmin(1, fit + (1.96 * se.fit)), 
#     #                 group = State),
#     #             fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
#     # geom_line(data = subset(forecasting10.predictions.list[[1]], State == i),
#     #           aes(x = Year, y = fit, group = State),
#     #           color = pal8[which(states == i)], linewidth = 1.5, linetype = "1111") +  # Use dark color for line
#     # 10 yr forecast, high temp
#     geom_ribbon(data = subset(forecasting10.predictions.list[[3]], State == i),
#                 aes(x = Year, 
#                     ymin = pmax(0, fit - (1.96 * se.fit)), 
#                     ymax = pmin(1, fit + (1.96 * se.fit)), 
#                     group = State),
#                 fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
#     geom_line(data = subset(forecasting10.predictions.list[[3]], State == i),
#               aes(x = Year, y = fit, group = State),
#               color = pal8[which(states == i)], linewidth = 1.5, linetype = "1212") +  # Use dark color for line
#     theme_classic() +
#     theme(legend.position = "none") +
#     theme(text = element_text(size = 8)) +
#     ylim(c(0, 1)) +
#     xlim(c(2018, 2023)) + #1972, 2023
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank()
#     )
#   # labs(x = " ", y = " ") #Probability of Presence doesn't fit
#   # ggtitle(paste("Spring -", i))  # Title with state name
# }
# 
# 
# # Fall
# for (i in states) {
#   # Plot using dark color palette for geom_line and light color palette for geom_ribbon
#   fig.comb[[paste("Fall -", i)]] <- ggplot() + 
#     # Observations, Presence data
#     geom_point(data = subset(data.list[[2]], State ==i),
#                aes(x = Year, y = Presence),
#                position = position_jitter(width = 0.2, height = 0.2), 
#                color = "gray80", size = 0.5) +
#     # Observations, Average fit
#     geom_line(data = subset(ave.presence.fa, State ==i),
#               aes(x = Year, y = ave.pres, group = State),
#               color = "gray60", linewidth = 0.5) +
#     # # 45 yr forecast, low temp
#     # geom_ribbon(data = subset(forecasting45.predictions.list[[2]], State == i), 
#     #             aes(x = Year, 
#     #                 ymin = pmax(0, fit - (1.96 * se.fit)), 
#     #                 ymax = pmin(1, fit + (1.96 * se.fit)), 
#     #                 group = State),
#     #             fill = pal8lite[which(states == i)]) +  
#     # geom_line(data = subset(forecasting45.predictions.list[[2]], State == i), 
#     #           aes(x = Year, y = fit, group = State),
#     #           color = pal8[which(states == i)], linewidth = 1.5, linetype = "3131") + 
#     # 45 yr forecast, high temp
#     geom_ribbon(data = subset(forecasting45.predictions.list[[4]], State == i), 
#                 aes(x = Year, 
#                     ymin = pmax(0, fit - (1.96 * se.fit)), 
#                     ymax = pmin(1, fit + (1.96 * se.fit)), 
#                     group = State),
#                 fill = pal8lite[which(states == i)]) +  
#     geom_line(data = subset(forecasting45.predictions.list[[4]], State == i), 
#               aes(x = Year, y = fit, group = State),
#               color = pal8[which(states == i)], linewidth = 1.5, linetype = "2121") + 
#     # # 10 yr forecast, low temp
#     # geom_ribbon(data = subset(forecasting10.predictions.list[[2]], State == i),
#     #             aes(x = Year, 
#     #                 ymin = pmax(0, fit - (1.96 * se.fit)), 
#     #                 ymax = pmin(1, fit + (1.96 * se.fit)), 
#     #                 group = State),
#     #             fill = pal8lite[which(states == i)]) +  
#     # geom_line(data = subset(forecasting10.predictions.list[[2]], State == i),
#     #           aes(x = Year, y = fit, group = State),
#     #           color = pal8[which(states == i)], linewidth = 1.5, linetype = "1111") +  
#     # 10 yr forecast, high temp
#     geom_ribbon(data = subset(forecasting10.predictions.list[[4]], State == i),
#                 aes(x = Year, 
#                     ymin = pmax(0, fit - (1.96 * se.fit)), 
#                     ymax = pmin(1, fit + (1.96 * se.fit)), 
#                     group = State),
#                 fill = pal8lite[which(states == i)]) +  
#     geom_line(data = subset(forecasting10.predictions.list[[4]], State == i),
#               aes(x = Year, y = fit, group = State),
#               color = pal8[which(states == i)], linewidth = 1.5, linetype = "1212") +  
#     theme_classic() +
#     theme(legend.position = "none") +
#     theme(text = element_text(size = 8)) +
#     ylim(c(0, 1)) +
#     xlim(c(2018, 2023)) +
#     theme(
#       axis.text.x = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank()
#     )
#   # labs(x = " ", y = " ") +
#   # ggtitle(paste("Fall -", i))  # Title with state name
# }
# 
# fig.comb.plot <- plot_grid(plotlist = fig.comb, ncol = 2, byrow = FALSE)
# # Print the combined plot
# print(fig.comb.plot)
# #ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-45-10yr-bystate-2018-2023.png", dpi = 400, width=9, height = 9)
# #ggsave(file = "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/figures/fig-forecast-45-10yr-Hi-Lo-bystate-2018-2023.png", dpi = 400, width=9, height = 9)


# 
# #----- First, need to Forecast predictions, 2018-2023
# # Bring in data (again)
# historic45.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic45-GAM-results.rds")
# historic10.gam.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/historic10-GAM-results.rds")
# forecasting45.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting45.data.list")
# # A. use glmwt_historic45 #7,8
# forecasting10.data.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecasting10.data.list")
# # A. use glmwt_historic10 #7, 8 
# # B. AND use glmwtY2_historic10.fa #11, 12
# 
# 
# 
# #----- Now predict to make forecasts
# # MDGill not in original fit, so modify forecasting10.data.list[[4]]
# new45.fa <- forecasting45.data.list[[4]] |> filter(Survey != "MDGill")
# new10.fa <- forecasting10.data.list[[4]] |> filter(Survey != "MDGill")
# 
# #----- A. 7&8
# #-- 45 yr With low water temp
# # spring
# A45low.predictions.spring <- predict(historic45.gam.list[[7]], se.fit=TRUE, newdata=forecasting45.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
# A45low.predictions.spring.df <- cbind(forecasting45.data.list[[3]], data.frame(A45low.predictions.spring))
# # fall
# A45low.predictions.fall <- predict(historic45.gam.list[[8]], se.fit=TRUE, newdata=new45.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# A45low.predictions.fall.df <- cbind(new10.fa, data.frame(A45low.predictions.fall))
# 
# #-- 45 yr With high water temp
# # spring
# A45high.predictions.spring <- predict(historic45.gam.list[[7]], se.fit=TRUE, newdata=forecasting45.data.list[[5]], type = "response", exclude = "s(Survey)", interval = "predict")
# A45high.predictions.spring.df <- cbind(forecasting45.data.list[[5]], data.frame(A45high.predictions.spring))
# # fall
# A45high.predictions.fall <- predict(historic45.gam.list[[8]], se.fit=TRUE, newdata=new45.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# A45high.predictions.fall.df <- cbind(new45.fa, data.frame(A45high.predictions.fall))
# 
# #-- 10 yr With low water temp
# # spring
# A10low.predictions.spring <- predict(historic10.gam.list[[7]], se.fit=TRUE, newdata=forecasting10.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
# A10low.predictions.spring.df <- cbind(forecasting10.data.list[[3]], data.frame(A10low.predictions.spring))
# # fall
# A10low.predictions.fall <- predict(historic10.gam.list[[8]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# A10low.predictions.fall.df <- cbind(new10.fa, data.frame(A10low.predictions.fall))
# 
# #-- 10 yr With high water temp
# # spring
# A10high.predictions.spring <- predict(historic10.gam.list[[7]], se.fit=TRUE, newdata=forecasting10.data.list[[5]], type = "response", exclude = "s(Survey)", interval = "predict")
# A10high.predictions.spring.df <- cbind(forecasting10.data.list[[5]], data.frame(A10high.predictions.spring))
# # fall
# A10high.predictions.fall <- predict(historic10.gam.list[[8]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# A10high.predictions.fall.df <- cbind(new10.fa, data.frame(A10high.predictions.fall))
# 
# #----- B. 11&12
# #-- 10 yr GLM Y^2 With low water temp
# # spring
# B10low.predictions.spring <- predict(historic10.gam.list[[11]], se.fit=TRUE, newdata=forecasting10.data.list[[3]], type = "response", exclude = "s(Survey)", interval = "predict")
# B10low.predictions.spring.df <- cbind(forecasting10.data.list[[3]], data.frame(B10low.predictions.spring))
# # fall
# B10low.predictions.fall <- predict(historic10.gam.list[[12]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# B10low.predictions.fall.df <- cbind(new10.fa, data.frame(B10low.predictions.fall))
# 
# #-- 10 yr With high water temp
# # spring
# B10high.predictions.spring <- predict(historic10.gam.list[[11]], se.fit=TRUE, newdata=forecasting10.data.list[[5]], type = "response", exclude = "s(Survey)", interval = "predict")
# B10high.predictions.spring.df <- cbind(forecasting10.data.list[[5]], data.frame(B10high.predictions.spring))
# # fall
# B10high.predictions.fall <- predict(historic10.gam.list[[12]], se.fit=TRUE, newdata=new10.fa, type = "response", exclude = "s(Survey)", interval = "predict")
# B10high.predictions.fall.df <- cbind(new10.fa, data.frame(B10high.predictions.fall))
# 
# 
# # List dfs
# forecastingGLM.predictions.list <- list(
#   A45low.predictions.spring.df = A45low.predictions.spring.df,
#   A45low.predictions.fall.df = A45low.predictions.fall.df,
#   A45high.predictions.spring.df = A45high.predictions.spring.df,
#   A45high.predictions.fall.df = A45high.predictions.fall.df,
#   A10low.predictions.spring.df = A10low.predictions.spring.df,
#   A10low.predictions.fall.df = A10low.predictions.fall.df,
#   A10high.predictions.spring.df = A10high.predictions.spring.df,
#   A10high.predictions.fall.df = A10high.predictions.fall.df,
#   B10low.predictions.spring.df = B10low.predictions.spring.df,
#   B10low.predictions.fall.df = B10low.predictions.fall.df,
#   B10high.predictions.spring.df = B10high.predictions.spring.df,
#   B10high.predictions.fall.df = B10high.predictions.fall.df
# )
# 
# # Save list
# saveRDS(forecastingGLM.predictions.list, "/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecastingGLM.predictions.list.rds")
# 
# forecastingGLM.predictions.list <- readRDS("/Users/janellemorano/Git/AtlanticMenhaden/4-forecasting/data/forecastingGLM.predictions.list.rds")
# 
# 
# 
# 
# 
