##### Adding covariates to Basic Menhaden model
# following https://github.com/James-Thorson-NOAA/VAST/wiki/Specify-covariates-and-visualize-responses

# Load packages
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects

# Set working directory to save model output
setwd("/Users/janellemorano/MODEL_OUTPUT/_currentrun")

# Load data set
data <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/survdat.menhaden.csv", header = TRUE)
head(data)
# I am missing Area swept in these data, so I'm going to fudge it and add 0.01 to every row for now.
data$AreaSwept_km2 <- 0.01


# Make covariate data
# Habitat covariates must be in a separate df and must include columns `Lat`, `Lon`, and `Year`
covariate_data = data.frame(
  "Lat" = data$LAT,
  "Lon" = data$LON,
  "Year" = data$YEAR,
  "DEPTH" = data$DEPTH,
  "BOTTEMP" = data$BOTTEMP
)

# check if there are any NAs in DEPTH
sum(is.na(covariate_data$DEPTH))
# Yes, there are. The model won't run if there are NA values for a covariate
# Just remove them for now
covariate_data <- na.omit(covariate_data) 

# If all covariates as "static" (not changing among years),
#  then set Year = NA to cause values to be duplicated internally for all values of Year
# If using a mix of static and dynamic covariates,
#  then duplicate rows for static covariates for every value of Year
# For Static variables (variables that don't change year to year), set year = NA
covariate_data$Year = NA

# Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
covariate_data$DEPTHscale = covariate_data$DEPTH / 100

# User Defined extrapolation grid
user_region <- readRDS('/Users/janellemorano/DATA/Atlantic_menhaden_modeling/user_region.rds')

# Make settings (turning off bias.correct to save time for example)
settings = make_settings( n_x = 500,
                          Region = 'User',
                          purpose = "index2",
                          knot_method = 'grid', #not in example, but in my basic model settings
                          ObsModel = c(2, 1),
                          use_anisotropy=FALSE,
                          bias.correct=FALSE,
                          fine_scale=TRUE )

# Define formula for the covariates
# In this case I'm demonstrating how to use a basis-spline with
# three degrees of freedom to model a nonlinear effect of log-transformed bottom depth,
# based on example developed by Nicholas Ducharme-Barth.
X1_formula = ~ bs( log(DEPTHscale), degree=2, intercept=FALSE)


# Interaction
#X2_formula = ~ poly(log(BOT_DEPTH), degree=2) + poly( BOT_TEMP, degree=2 )

# Model formula
# as a cheat, for simplicity right here, each intercept parameter (ie Beta, Epsilon) is written as "s" sort of in the style of a GAM smoother
# P(i) = s(c_i, t_i) + s(Lat_i, Lon_i

# Run model
fit = fit_model( "settings" = settings,
                 "Lat_i" = data$LAT,
                 "Lon_i" = data$LON,
                 "t_i" = data$YEAR, #time
                 "b_i" = data$ABUNDANCE, #catch
                 "a_i" = data$AreaSwept_km2, #area swept, This is FAKED and NOT REAL
                 "X1_formula" = X1_formula,
                 "covariate_data" = covariate_data,
                 "input_grid" = user_region)

