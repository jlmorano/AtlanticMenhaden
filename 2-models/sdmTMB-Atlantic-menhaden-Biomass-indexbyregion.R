# Testing index by region
library(sdmTMB)
library(tidyverse)
library(tictoc)


#----- SPRING --------------------------
# Take model fit
fit.sp <- readRDS("D:/MODEL_OUTPUT/twar.fit.spring.rds")

# Add "region" to the prediction grid
nd.grid.yrs.spring <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-spring_NEFSC-NEAMAP-2.rds")

# Change column names to match lowercase of dataset
newgrid <- nd.grid.yrs.spring %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")

# Adding rough region by latitude for now
newgrid <- newgrid |>
   mutate(region = case_when(latitude <=36.5547 ~ "NC",
                             latitude >36.5547 & latitude <=38.0000 ~ "VA",
                             latitude >38.0000 & latitude <=38.7817 ~ "DEMD",
                             latitude >38.7817 & latitude <=40.4678 ~ "NJ",
                             latitude >40.4678 & latitude <=41.4205 ~ "NYCTRI",
                             latitude >41.4205 ~ "MA"))
unique(newgrid$region)
# "NC"     "VA"     "DEMD"   "NJ"     "NYCTRI" "MA" 


#----- Make predictions
tic()
preds <- newgrid |>
   split(newgrid$region) |>
   lapply(function(x) predict(fit.sp, newdata = x, return_tmb_object = TRUE)) 
time <- toc()
#681.76 sec elapsed

# Save and then move it to DATA storage on Virtual PC
saveRDS(preds, file = "D:/MODEL_OUTPUT/twar.predictions.spring.byregion.rds" )

# Read in
preds <- readRDS(file = "D:/MODEL_OUTPUT/twar.predictions.spring.byregion.rds")

# Run index by region
tic()
preds <- newgrid |>
  split(grids$region) |> 
  lapply(function(x) predict(fit, newdata = x, return_tmb_object = TRUE))
toc()
# missed value

saveRDS(preds, file = "D:/MODEL_OUTPUT/twar.predictions.spring.byregion.rds")
preds <- readRDS("D:/MODEL_OUTPUT/twar.predictions.spring.byregion.rds")

# Transform "preds" into regional  indices by region
# But I don't know what area identifies
inds <- purrr::map_dfr(preds, get_index, area = 4, .id = "region")

write.csv(inds, "D:/MODEL_OUTPUT/index.spring.byregion.csv")

ggplot(inds, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region))




#----- FALL --------------------------
# Take model fit
fit.fa <- readRDS("D:/MODEL_OUTPUT/twar.fit.fall.rds")

# Add "region" to the prediction grid
nd.grid.yrs.fall <- readRDS("D:/DATA/Atlantic_menhaden_modeling/1-extrapolation-grids/grid-by-years-fall_NEFSC-NEAMAP-2.rds")

# Change column names to match lowercase of dataset
newgrid.fa <- nd.grid.yrs.fall %>% 
  janitor::clean_names(case = c("lower_camel")) %>%
  rename("X" = "x",
         "Y" = "y")

# Adding rough region by latitude for now
newgrid.fa <- newgrid.fa |>
   mutate(region = case_when(latitude <=36.5547 ~ "NC",
                             latitude >36.5547 & latitude <=38.0000 ~ "VA",
                             latitude >38.0000 & latitude <=38.7817 ~ "DEMD",
                             latitude >38.7817 & latitude <=40.4678 ~ "NJ",
                             latitude >40.4678 & latitude <=41.4205 ~ "NYCTRI",
                             latitude >41.4205 ~ "MA"))
unique(newgrid.fa$region)
#"NC"     "VA"     "DEMD"   "NJ"     "NYCTRI" "MA"   

#----- Make predictions
tic()
preds.fa <- newgrid.fa |>
   split(newgrid.fa$region) |>
   lapply(function(x) predict(fit.fa, newdata = x, return_tmb_object = TRUE)) 
toc()
#886 sec elapsed


# Save and then move it to DATA storage on Virtual PC
saveRDS(preds.fa, file = "D:/MODEL_OUTPUT/twar.predictions.fall.byregion.rds" )
preds.fa <- readRDS("D:/MODEL_OUTPUT/twar.predictions.fall.byregion.rds")

# Transform "preds" into regional  indices by region
# But I don't know what area identifies
tic()
inds.fa <- purrr::map_dfr(preds.fa, get_index, area = 4, .id = "region")
toc()
#29399.48 sec elapsed

write.csv(inds.fa, "D:/MODEL_OUTPUT/index.fall.byregion.csv")

ggplot(inds.fa, aes(year, est, ymin = lwr, ymax = upr, fill = region)) +
  geom_ribbon(alpha = 0.3) +
  geom_line(aes(colour = region))

