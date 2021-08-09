#### Diary of putting together a basic VAST model for menhaden.
# This is code that explains problems I encountered before finalizing the steps in the basic menhaden model in VAST markdown
# This code can be run from start to end to run lines that are working, but all steps that didn't work are kept in for documentation. Thus, it is repetitive. Clean code is kept elsewhere. 

# Load package
library(VAST)
sessionInfo()

## Data Setup

# For right now, I'm using a dataset with abundance and biomass of Atlantic menhaden that came from Kevin Friedland at NOAA Narragansett. It is NEFSC bottom trawl data that has been standardized for biomass in ways that I don't currently know. So these are data for playing around with, but not to draw any kind of scientific conclusions from until I've had a chance to prepare my own valid dataset. 

# Use NEFSC bottom trawl survey data to try here
# data <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/dat.csv", header = TRUE)

# But my data are not formatted correctly to fit into VAST, at least in the simple model example.
# see `?load_example` for list of stocks with example data 
# that are installed automatically with `FishStatsUtils`. 
# example = load_example( data_set="EBS_pollock" )


# The example data is a list of sampling data, region (as a character), strata.limits (as a character), covariates (Year and area swept)

# I think it's ok that it's not exactly like the example, but I am missing Area swept. So I'm going to fudge it and add 0.01 to every row.
# data$AreaSwept_km2 <- 0.01

# FIT THE MODEL
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects. This doesn't have any covariates (depth or bottom temp) yet.
# fit = fit_model( "settings" = settings,
#                  "Lat_i" = data$LAT,
#                  "Lon_i" = data$LON,
#                  "t_i" = data$YEAR, #time
#                  "b_i" = data$ABUNDANCE, #catch
#                  "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
#                  "v_i"= data$SVVESSEL,
#                  "input_grid" = user_region)

# Getting these errors with the basic model:
# * NA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluationNA/NaN function evaluation 
# * Error in optimHess(parameter_estimates$par, fn = fn, gr = gr) : gradient in optim evaluated to length 1 not 110

# which suggests that at least one year has no zeros, so let's check
# library(tidyr)
# data %>%
#   group_by(YEAR) %>%
#   summarise(max = max(ABUNDANCE), min = min(ABUNDANCE))

# Right, of course! These data only have positive abundance. There are no zeros because I only grabbed the menhaden data. So I went back to the orginal data source (survdat) and grabbed the sample locations, keeping all columns, but turning them into zero abundances, etc.

# So now I need to merge my menhaden data with the sampling site data to have zeros and positive catches. I went back and created a new dataset ('create_FULLsurvey_menhaden_test_data.R'). So, now I'm going to work with that in the same model listed above.

data <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/survdat.menhaden.csv", header = TRUE)
head(data)
# and it worked!

# But then I ran it again and got errors referring to kappa1 and kappa2. 
# Turns out I needed to increase the number of knots, so I changed it from 100 to 250. And then it worked!

# I am missing Area swept. So I'm going to fudge it and add 0.01 to every row.
data$AreaSwept_km2 <- 0.01

# Select model settings
settings = make_settings( n_x = 250, 
                          Region = 'User', 
                          purpose = "index2", 
                          #strata.limits = example$strata.limits, #this was in the simple model example but not sure how it would interface here with a user-defined grid
                          knot_method = 'grid',
                          ObsModel= c(2,0),#, 1st value encounter probabilities = Poisson (7) lognormal (1), 2nd value catch rate= 0 default (the order is incorrect on the VAST input Google doc)),
                          bias.correct = FALSE )
#need to look more into next step
settings$FieldConfig[2,] <- 0 ## turn off temporal components with 0

# This can be done before or after setting the model settings, but you need to read the extrapolation grid data to 'user_region'.
user_region <- readRDS('/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/user_region.rds')

# Fit the model
# The model is predicting the encounter probability in the first step and the positive catch rate at each location in the second step where each are a function of the Year, Lat/Lon location, abundance, area swept, and vessel effects. This doesn't have any covariates (depth or bottom temp) yet.
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$LAT,
                 "Lon_i" = data$LON,
                 "t_i" = data$YEAR, #time
                 "b_i" = data$ABUNDANCE, #catch
                 "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
                 "v_i"= data$SVVESSEL,
                 "input_grid" = user_region)

# Plot the results
# First, set the working directory to save the plots to a spot where I won't GIT the output because of size limitations.
setwd("/Users/janellemorano/Git/VAST output_DONOTGIT/_currentrun")
plot( fit )

