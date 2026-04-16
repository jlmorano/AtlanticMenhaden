# landings

land <- read.csv("/Users/janellemorano/Downloads/FOSS_landings.csv", header = FALSE)
land <- land[-1,]
names(land) <- land[1,]
land <- land[-1,]
colnames(land)



ggplot(subset(land, State %in% c(VIRGINIA)), aes(x = Year, y = Pounds, color = State)) +
  geom_point()
