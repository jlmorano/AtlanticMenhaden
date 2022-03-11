# Compare Survdat data used in model to GMRI data

# Menhaden model data from Survdat from Kevin Friedland
survdat <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/survdat.menhaden.csv")
sapply(survdat, function(x) length(unique(x)))

survdat_bio <- read.csv("/Users/janellemorano/DATA/Atlantic_menhaden_modeling/menhaden_sampledata.csv")
sapply(survdat_bio, function(x) length(unique(x)))

# Compare with data from GMRI
gmri <- read_csv("/Users/janellemorano/DATA/NEFSC-Survdat/survdat_menhaden_2019_fromGMRI.csv")
sapply(gmri, function(x) length(unique(x)))
min(gmri$est_year)
# 1970
max(gmri$est_year)
# 2019

# Check abundance and biomass
max(gmri$abundance)
# 198
max(survdat$ABUNDANCE)
# NA
max(survdat_bio$Count)
# NA

max(gmri$biomass)
# 56.5
max(survdat$BIOMASS)
# NA
max(survdat_bio$Weight)
# 666.87

plot(gmri$est_year, gmri$biomass)
plot(gmri$est_year, gmri$abundance)
plot(survdat$YEAR, survdat$BIOMASS)
