# Visualize model output

# Center of gravity
cog_spring <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20230111 Spring/CenterofGravity_spring.csv", header = TRUE)
#This not a current file for fall!!!
cog_fall <- read.csv("/Users/janellemorano/MODEL_OUTPUT/Atlantic-menhaden-distribution-model-20220401 Fall gamma/CenterofGravity_fall.csv", header = TRUE)

# #Conversion of data frame to sf object NOT CORRECT
# library(sf)
# cog_spring_sf <- st_as_sf(x = cog_spring,                         
#                   coords = c("easting", "northing"),
#                   crs = "+proj=utm +zone=18")
# #Projection transformation
# sfc = st_transform(cog_spring_sf, crs = "+proj=longlat +datum=WGS84")
# #Convert it to data frame
# cog_spring_sf <- as.data.frame(sfc)

range(cog_spring$northing)

# Spring
ggplot(cog_spring, aes(x=year, y=northing)) +
  geom_line(color = "#7AD151FF") +
  xlim(1972, 2022) +
  # ylim(-7.5, 7.5) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Northing") +
  ggtitle("Spring")
# Fall
ggplot(cog_fall, aes(x=year, y=northing)) +
  geom_line(color = "#414487FF") +
  xlim(1972, 2022) +
  # ylim(-7.5, 7.5) +
  theme_classic() +
  theme(legend.position = "none") +
  theme(text = element_text(size = 14)) +
  labs(x= " ", y = "Northing") +
  ggtitle("Spring")
