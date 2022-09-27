# Deviance Explainedd

# Load package
library(VAST)

# Load model fit
load( "/Users/janellemorano/Git/AtlanticMenhaden/model-output/Atlantic-menhaden-distribution-model-20220401_output.RData" )

# Deviance of model fits
# fit.spring
fit.spring$Report$deviance
# fit.fall
fit.fall$Report$deviance

###### Null model, Spring
run_dir = paste0(getwd(),"/spring/null/")
dir.create(run_dir,recursive=TRUE)

# Make settings (turning off bias.correct to save time for example)
settings.null <- settings
settings.null$FieldConfig = matrix( c(0,0,0,0,"IID","IID"), byrow=TRUE, ncol=2 )
settings.null$RhoConfig[c("Beta1","Beta2")] = 3

# Run model
fit.spring.null = fit_model( "settings" = settings.null,
                        "Lat_i" = data.spring$Lat,
                        "Lon_i" = data.spring$Lon,
                        "t_i" = data.spring$Year, #time
                        "b_i" = as_units(data.spring$Biomass, "kg"),
                        "a_i" = as_units(data.spring$Areasw, "km^2"), #area swept
                        "X1_formula" = X1_formula, #depth
                        "X2_formula" = X2_formula, #bottemp
                        "covariate_data" = covariate_data.spring,
                        "input_grid" = user_region,
                        getsd = FALSE,
                        newtonsteps = 0,
                        working_dir = run_dir)

###### Calculate percent-deviance-explained
1 - fit.spring$Report$deviance/fit.spring.null$Report$deviance
# 0.7148041


###### Null model, Fall
run_dir = paste0(getwd(),"/fall/null/")
dir.create(run_dir,recursive=TRUE)

# Make settings (turning off bias.correct to save time for example)
# Same as above, shouldn't need to repeat
# settings.null <- settings
# settings.null$FieldConfig = matrix( c(0,0,0,0,"IID","IID"), byrow=TRUE, ncol=2 )
# settings.null$RhoConfig[c("Beta1","Beta2")] = 3

# Run model
fit.fall.null = fit_model( "settings" = settings.null,
                      "Lat_i" = data.fall$Lat,
                      "Lon_i" = data.fall$Lon,
                      "t_i" = data.fall$Year, #time
                      "b_i" = as_units(data.fall$Biomass, "kg"),
                      "a_i" = as_units(data.fall$Areasw, "km^2"), #area swept
                      "X1_formula" = X1_formula, #depth
                      "X2_formula" = X2_formula, #bottemp
                      "covariate_data" = covariate_data.fall,
                      "input_grid" = user_region,
                      getsd = FALSE,
                      newtonsteps = 0,
                      working_dir = run_dir)

###### Calculate percent-deviance-explained
1 - fit.fall$Report$deviance/fit.fall.null$Report$deviance
# 0.6659635
