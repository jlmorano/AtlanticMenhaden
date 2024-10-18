# Figure7-sdmTMB-indexbyregion.R
######################################
# Janelle L. Morano

# Objectives:
# Use model fit already generated to predict and index
# by state-region


# last updated 23 August 2024
###############################################
###############################################

library(tidyverse)


ind.sp <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/index.spring.byregion.csv", header = TRUE)
ind.sp$region <- factor(ind.sp$region, levels = c("MA", "NYCTRI", "NJ", "DEMD", "VA", "NC"))
                        

ind.fa <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/2024 output/index.fall.byregion.csv", header = TRUE)



# Simple plot
ggplot(ind.sp, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region))
# 
# ggplot(ind.fa, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
#   geom_ribbon(alpha = 0.3) +
#   geom_line(aes(colour = region))


pal6 <- c("#440154", "#414487", "#2a788e", "#22a884", "#7ad151", "#bddf26")
pal6lite <- adjustcolor(pal6, alpha.f = 0.5) #make transparent

# Spring
a <- ggplot() + 
  geom_line(data = subset(ind.sp, region %in% c("MA")), aes(x=year, y=est), color = pal6[1], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("MA")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[1]) +
  geom_line(data = subset(ind.sp, region %in% c("NYCTRI")), aes(x=year, y=est), color = pal6[2], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("NYCTRI")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[2]) +
  geom_line(data = subset(ind.sp, region %in% c("NJ")), aes(x=year, y=est), color = pal6[3], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("NJ")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[3]) +
  geom_line(data = subset(ind.sp, region %in% c("DEMD")), aes(x=year, y=est), color = pal6[4], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("DEMD")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[4]) +
  geom_line(data = subset(ind.sp, region %in% c("VA")), aes(x=year, y=est), color = pal6[5], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("VA")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[5]) +
  geom_line(data = subset(ind.sp, region %in% c("NC")), aes(x=year, y=est), color = pal6[6], linewidth = 1) +
  geom_ribbon(data = subset(ind.sp, region %in% c("NC")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[6]) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  ylim(c(0, 100000)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Biomass (kg/km^2)") + 
  ggtitle("Spring")


# Fall
b <- ggplot() + 
  geom_line(data = subset(ind.fa, region %in% c("MA")), aes(x=year, y=est), color = pal6[1], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("MA")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[1]) +
  geom_line(data = subset(ind.fa, region %in% c("NYCTRI")), aes(x=year, y=est), color = pal6[2], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("NYCTRI")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[2]) +
  geom_line(data = subset(ind.fa, region %in% c("NJ")), aes(x=year, y=est), color = pal6[3], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("NJ")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[3]) +
  geom_line(data = subset(ind.fa, region %in% c("DEMD")), aes(x=year, y=est), color = pal6[4], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("DEMD")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[4]) +
  geom_line(data = subset(ind.fa, region %in% c("VA")), aes(x=year, y=est), color = pal6[5], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("VA")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[5]) +
  geom_line(data = subset(ind.fa, region %in% c("NC")), aes(x=year, y=est), color = pal6[6], linewidth = 1) +
  geom_ribbon(data = subset(ind.fa, region %in% c("NC")), aes(x=year, ymin = lwr, ymax = upr), fill = pal6lite[6]) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 12)) +
  xlim(c(1972, 2023)) +
  labs(x= " ", y = "Biomass (kg/km^2)") + 
  ggtitle("Fall")

library(cowplot)
plot_grid(a, b, labels=c("A", "B"), nrow = 1)
#ggsave(file = "/Users/janellemorano/DATA/Atlantic_menhaden_modeling/final-ms/Summer2024figures/Fig7-index-by-region.png", width=7, height = 4)
