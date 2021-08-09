##### Adding covariates to Basic Menhaden model
# following https://github.com/James-Thorson-NOAA/VAST/wiki/Specify-covariates-and-visualize-responses

# Load packages
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects

# Load data set
data <- read.csv("/Users/janellemorano/Git/Reference-R-scripts/VAST_exploration/survdat.menhaden.csv", header = TRUE)
head(data)
# I am missing Area swept in these data, so I'm going to fudge it and add 0.01 to every row for now.
data$AreaSwept_km2 <- 0.01

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 250,
                          Region = 'User',
                          purpose = "index2",
                          knot_method = 'grid', #not in example, but in my basic model settings
                          use_anisotropy=FALSE,
                          bias.correct=FALSE,
                          fine_scale=TRUE )

# Define formula for the covariates
# In this case I'm demonstrating how to use a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
X1_formula = ~ bs( log(DEPTH), degree=2, intercept=FALSE)

# Interaction
#X2_formula = ~ poly(log(BOT_DEPTH), degree=2) + poly( BOT_TEMP, degree=2 )

# If all covariates as "static" (not changing among years),
#  then set Year = NA to cause values to be duplicated internally for all values of Year
# If using a mix of static and dynamic covariates,
#  then duplicate rows for static covariates for every value of Year
# Here, all covariates are static, so I'm using the first approach.
# Set Year to NA because covariate depth is static
data$YEAR = NA

# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
data$DEPTH = data$DEPTH / 100


# Run model
fit = fit_model( "settings" = settings,
                 Lat_i = example$sampling_data[,'Lat'],
                 Lon_i = example$sampling_data[,'Lon'],
                 t_i = example$sampling_data[,'Year'],
                 b_i = example$sampling_data[,'Catch_KG'],
                 a_i = example$sampling_data[,'AreaSwept_km2'],
                 X1_formula = X1_formula,
                 X2_formula = X2_formula,
                 covariate_data = example$covariate_data )

fit = fit_model( "settings" = settings,
                 "Lat_i" = data$LAT,
                 "Lon_i" = data$LON,
                 "t_i" = data$YEAR, #time
                 "b_i" = data$ABUNDANCE, #catch
                 "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
                 "v_i"= data$SVVESSEL,
                 X1_formula = X1_formula,
                 covariate_data = data,
                 "input_grid" = user_region)

