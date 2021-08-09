# AtlanticMenhaden
Spatio-temporal distribution of Atlantic menhaden.

IMPORTANT: All data are stored on my local DATA drive.
All VAST output are stored on local DATA drive and not added to GIT for space reasons.

Each model phase listed below has the following:
1. R code for running a current or operating model.
1. Rmd for explanation.
1. DIARY for the sequential steps I took and errors encountered.

## Create Data & User Region
Create_FULLsurvey and Create_menhaden_test_data for creating data used to get these models to run. Data are not cleaned and should not be used to make ecological interpretations.

User_region is the extrapolation grid created following code in the VAST exploration project.

## Basic Menhaden
Model of Atlantic menhaden spatial distribution using VAST's index of abundance standardization model example. Designed and executed to get working in VAST. Data are dummy data and should not be used for ecological interpretation.

## Basic Menhaden with covariates
Builds on Basic Menhaden model in VAST by adding covariates.