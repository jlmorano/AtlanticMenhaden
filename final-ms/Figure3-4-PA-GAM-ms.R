# Figure3-4-GAM-ms.R
######################################
# Janelle L. Morano

# Visualizations (graphs and maps) from the models run in "Final-Models-Atlantic-menhaden-ms.R" for the Atlantic menhaden manuscript

# last updated 13 August 2024
###############################################
###############################################

###########################################################################################
#-Table of Contents ------------------------------------------------------------
#----- Data Prep (GAM model results of federal and state surveys)
#       (Line 26)
#----- Presence GAM: Presence ~ s(Year, by = State) + s(Bottemp/Surftemp/WaterTemp) + State
#       (Line 159)

# Table of model coefficients
###########################################################################################


library(tidyverse)
library(mgcv)
library(cowplot)


###########################################################################################
#----- Load Model Results ------------------------------------------------------------
###########################################################################################

# Survey Data
data.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/data.list.rds")
# Prediction data
preddata.pgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-summaries.rds")
# Presence GAM model output
pgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-results.rds")
# Presence GAM model output summaries only
pgam.summaries <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-summaries.rds")
# Presence GAM model predictions
predictions.pgam.list <- readRDS("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/PA-GAM-predictions.rds")




###########################################################################################
#----- Figure 3. Visualize predictions: Presence GAM ----------------------------------
###########################################################################################
# Color palette for North to South States AKA Dark to Light
pal7 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26", "#fde725")
pal7lite <- adjustcolor(pal7, alpha.f = 0.5) #make transparent


#----- Federal Spring
f3a <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[1]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = "Presence") + 
  ggtitle("Spring (Federal)")


#----- Federal Fall
f3b <- ggplot() +  
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[2]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +  
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = " ")+
  ggtitle("Fall (Federal)")


#----- State Spring
f3c <- ggplot() + 
  # geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  # geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[3]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[3]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring (State)")


#----- State Fall
f3d <- ggplot() +  
  # geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("MA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal7lite[1]) +
  # geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("NJ")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("DEMD")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("VA")), aes(x=Year, ymin = fit-se.fit, ymax = fit+se.fit, group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("NC")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[4]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(2*se.fit), ymax = fit+(2*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[4]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +  
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = " ") +
  ggtitle("Fall (State)")


plot_grid(f3a, f3b, f3c, f3d, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig3-PA-GAM-separate.png", width=7, height = 7)




###########################################################################################
#----- Figure 4. All Data (federal and state combined) ----------------------------------
###########################################################################################

#----- Spring
f4a <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[5]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[5]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Spring (Federal and State)")


#----- Fall
f4b <- ggplot() + 
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("MA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[1]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("MA")), aes(x=Year, y=fit, group = State), color = pal7[1], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("RICTNY")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[2]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("RICTNY")), aes(x=Year, y=fit, group = State), color = pal7[2], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("NJ")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[3]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("NJ")), aes(x=Year, y=fit, group = State), color = pal7[3], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("DEMD")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[4]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("DEMD")), aes(x=Year, y=fit, group = State), color = pal7[4], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("VA")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[5]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("VA")), aes(x=Year, y=fit, group = State), color = pal7[5], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("NC")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[6]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("NC")), aes(x=Year, y=fit, group = State), color = pal7[6], linewidth = 1.2) +
  geom_ribbon(data = subset(predictions.pgam.list[[6]], State %in% c("SCGAFL")), aes(x=Year, ymin = fit-(1.96*se.fit), ymax = fit+(1.96*se.fit), group = State), fill = pal7lite[7]) +
  geom_line(data = subset(predictions.pgam.list[[6]], State %in% c("SCGAFL")), aes(x=Year, y=fit, group = State), color = pal7[7], linewidth = 1.2) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0,1)) +
  xlim(c(1972, 2021)) +
  labs(x= " ", y = "Presence") +
  ggtitle("Fall (Federal and State)")


plot_grid(f4a, f4b, labels=c("A", "B"), ncol = 2, nrow = 1)
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig4-PA-GAM-alldata.png", width=7, height = 4)



###########################################################################################
#----- Table X. Model coefficients ----------------------------------
###########################################################################################

library(modelsummary)

pa.gam.table <- modelsummary(pgam.list, output = "data.frame")
