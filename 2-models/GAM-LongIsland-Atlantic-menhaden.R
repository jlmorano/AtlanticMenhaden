# GAM-LongIsland-Atlantic-menhaden.R
######################################
# Janelle L. Morano
# Using NYDEC Western Long Island Sound survey data
# And CTLISTS data
# GAM model

# last updated 27 October 2022
###############################################
###############################################

wli <- read.csv("/Users/janellemorano/DATA/NY Western Long Island Seine Survey/WLI_menhaden_catch_data.csv", header = TRUE)
colnames(wli)
# Convert to date
wli <- wli %>%
  mutate(date = lubridate::mdy(DATE))
min(wli$date)
# "1984-05-09"
max(wli$date)
# "2021-10-21"

# Water temp = H2O

#### GAM of Count by smooth(Year), H2O
####################################################################
library(mgcv)

wli.gam = gam(Menhaden.Total ~ s(year) + H2O, data = wli)
summary(wli.gam)
plot(wli.gam, main = "WLI")

# Plot with mgcViz
library(mgcViz)
b <- getViz(wli.gam)
plot( sm(b, 1)) +
  l_fitLine(colour = "red") +
  l_ciLine(colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  # scale_y_break(c(50, 3600)) +
  theme_classic()


#####
ct <- read.csv("/Users/janellemorano/DATA/CT Long Island Sound survey/CTLISTS_menhaden.csv", header = TRUE)
colnames(ct)

ct.gam = gam(TotalCount ~ s(Year) + BotTemp, data = ct)
summary(ct.gam)
plot(ct.gam, main = "CTLISTS")

