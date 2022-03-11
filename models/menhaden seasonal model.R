#menhaden seasonal model

#Goal: 

# Add to your dataset a column that identifies each season and year combination as a factor
# Then create a set of dummy data that is a duplicate but adds a column "dummy" as TRUE
# 
# matrix of year, season
# for each year, the season available

# Model with year as a fixed effect (RhoConfig, Beta = 3, Epsilon = 4 )
# 3 = autocorrelation is a fixed effect among years
# 4 = autocorrelation follows an AR1 process (i.e. lag of 1 year)
