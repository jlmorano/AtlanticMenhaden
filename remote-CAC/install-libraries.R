install.packages("devtools")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("janitor")
install.packages("sf")
install.packages("terra")
install.packages("lubridate")
install.packages("Rgraphviz")
install.packages("tictoc")

install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
options(repos=c(
  inlabruorg = "https://inlabru-org.r-universe.dev",
  INLA = "https://inla.r-inla-download.org/R/testing",
  CRAN = "https://cran.rstudio.com"))
install.packages("fmesher")


install.packages("sdmTMB", dependencies = TRUE)

library(devtools)
# Install package
install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")
# Load package
library(VAST)
# Install TMB from CRAN
install.packages("TMB")
# Install INLA using currently recommended method
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
# Install FishStatsUtils from CRAN
install_github("james-thorson/FishStatsUtils@main", INSTALL_opts="--no-staged-install")