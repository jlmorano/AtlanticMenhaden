# Figure3-GAM-ms.R
######################################
# Janelle L. Morano

# Figure 3. Part A. All state trends overlayed in 2 figs: spring and fall
# Figure 3. Part B. State by state: spring and fall

# last updated 13 May 2025
###############################################
###############################################

setwd()

library(tidyverse)
library(mgcv) #1.9-1
library(cowplot)


###########################################################################################
#----- Load Model Results ------------------------------------------------------------
###########################################################################################

# Remember the models,
# SPRING m7: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + s(WaterTemp)
# FALL m6: Presence ~ s(Year, by = State) + State + s(Survey, bs = "re") + Depth + WaterTemp


# Read in saved RDS of menhaden data, 1972-2023
data.list <- readRDS("~/2-input-data/menhaden.data.list.rds")
names(data.list)
# "alldata.spring" "alldata.fall"

# Presence GAM model output
pgam.list <- readRDS("~/3-output-data/PA-GAM-results.rds")
names(pgam.list)
# [1] "m1_alldata.spring" "m1_alldata.fall"   "m2_alldata.spring" "m2_alldata.fall"   "m3_alldata.spring"
# [6] "m3_alldata.fall"   "m4_alldata.spring" "m4_alldata.fall"   "m5_alldata.spring" "m5_alldata.fall"  
# [11] "m6_alldata.spring" "m6_alldata.fall"   "m7_alldata.spring" "m7_alldata.fall" 

# Presence GAM model predictions
predictions.pgam.list <- readRDS("~/3-output-data/PA-GAM-predictions.rds")
names(predictions.pgam.list)
# "predictions.pa.gam.7.spring.df" "predictions.pa.gam.6.fall.df" 




###########################################################################################
#----- Part A. All state trends overlayed in 2 figs: spring and fall ---------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparent


#----- Spring
f2a1 <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("GME")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("GME")), aes(x=Year, y=fit, group = State), color = pal8[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal8[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal8[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal8[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal8[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal8[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal8[7], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[8]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal8[8], linewidth = 1.2) +
  # Add overall GAM trend
  geom_smooth(method = "gam", data = predictions.pgam.list[[1]], aes(x = Year, y = fit, se = TRUE), color = "black", linewidth = 2, linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,0.8)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Probability of Presence") +
  ggtitle("Spring")


#----- Fall
f2a2 <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("GME")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("GME")), aes(x=Year, y=fit, group = State), color = pal8[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal8[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal8[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal8[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal8[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal8[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal8[7], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal8lite[8]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal8[8], linewidth = 1.2) +
  # Add overall GAM trend
  geom_smooth(method = "gam", data = predictions.pgam.list[[2]], aes(x = Year, y = fit, se = TRUE), color = "black", size = 2, linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,0.8)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Probability of Presence") +
  ggtitle("Fall")


plot_grid(f2a1, f2a2, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "Fig2B-PA-GAM-stateoverlap.png", width=7, height = 4)





###########################################################################################
#----- Part B. State by state: spring and fall ------------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal8 <- c("#440154", "#482878", "#3e4989", "#26828e", "#35b779", "#6ece58", "#bddf26", "#fde725")
pal8lite <- adjustcolor(pal8, alpha.f = 0.2) #make transparentdjustcolor(pal7, alpha.f = 0.5) #make transparent

# List of States to plot
states <- unique(na.omit(predictions.pgam.list[[1]]$State))
states <- factor(states, levels = c("GME", "MA", "RICTNY", "NJ", "DEMD", "VA", "NC", "SCGAFL"))

# Create an empty list to store plots
fig2b <- list()


# Spring
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig2b[[paste("Spring -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[1]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0.2),
               color = "gray90", size = 0.25) +
    # GAM fit
    geom_ribbon(data = subset(predictions.pgam.list[[1]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions.pgam.list[[1]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ylim(c(0, 1)) +
    xlim(c(1972, 2023)) +
    labs(x = " ", y = "Probability of Presence") #+
    # ggtitle(paste("Spring -", i))  # Title with state name
}


# Fall
for (i in states) {
  # Plot using dark color palette for geom_line and light color palette for geom_ribbon
  fig2b[[paste("Fall -", i)]] <- ggplot() + 
    # Observations, Presence data
    geom_point(data = subset(data.list[[2]], State ==i),
               aes(x = Year, y = Presence),
               position = position_jitter(width = 0.2, height = 0.2),
               color = "gray90", size = 0.25) +
    # GAM fit
    geom_ribbon(data = subset(predictions.pgam.list[[2]], State == i), 
                aes(x = Year, ymin = fit - (1.96 * se.fit), ymax = fit + (1.96 * se.fit), group = State), 
                fill = pal8lite[which(states == i)]) +  # Use light color for ribbon
    geom_line(data = subset(predictions.pgam.list[[2]], State == i), 
              aes(x = Year, y = fit, group = State), 
              color = pal8[which(states == i)], linewidth = 1.2) +  # Use dark color for line
    theme_classic() +
    theme(legend.position = "none") +
    theme(text = element_text(size = 6)) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    ylim(c(0, 1)) +
    xlim(c(1972, 2023)) +
    labs(x = " ", y = "Probability of Presence") #+
    #ggtitle(paste("Fall -", i))  # Title with state name
}

combined_plot <- plot_grid(plotlist = fig2b, ncol = 2, byrow = FALSE)
# Print the combined plot
print(combined_plot)
#ggsave(file = "Fig2C-PA-GAM-bystate.png", width=9, height = 9)
