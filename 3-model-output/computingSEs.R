# Computing SEs by simulation from the joint precision matrix

##### How can FishStatsUtils::simulate_data be modified?
# https://github.com/James-Thorson-NOAA/FishStatsUtils/blob/main/R/simulate_data.R


# Predicted densities from simulate_data()
?FishStatsUtils::simulate_data


##### What about how sdmTMB::gather_sims
# https://pbs-assess.github.io/sdmTMB/reference/gather_sims.html
# Or here
# https://github.com/pbs-assess/sdmTMB#calculating-uncertainty-on-spatial-predictions


#############
# Adapted code shared from Jim Thorson

#### 1. Grab the parameter values (etc) from the tmb object within the VAST fit output
# Obj = objects output by fit under fit$tmb_list$Obj
Obj <- fit.spring$tmb_list$Obj
# fit$tmb_list$Obj is a list with the following objects:
# ~$par = model parameters and their values
# ~$fn = function, but not sure exactly what it's doing, maybe a random walk
# ~$gr = gradient = set of starting slopes; function that I don't understand what it's doing
# ~$he = hessian function; for computing hessian?
# ~$hessian = hessian output, maybe?
# ~$method = character code for method of what?
# ~$retape = ??
# ~$env
# ~$report = function to generate list of parameter values from 'par'
# ~$sim = function, not sure what's being simulated
# ~$control = ??


#### 2. Minimizing (optimizing) something here
# Optimize...but what? Sampling what?
###* Optimizing what here? The start values, but then what's the function? It looks like the optimizing values are being pulled in the Opt = line and then SDs are being generated with those Opt values...but I don't know why or exactly how it looks in the end
####* I don't understand nlminb(), but it appears to select parameters in a matrix to initiate with for optimization, minimizing the function (minimizing the difference like minimizing the sum of squares?), but then I don't know what the gradient represents
Opt = nlminb( start=Obj$par, obj=Obj$fn, gr=Obj$gr ) 
#nlminb(startp, objective function); like TMB but not as efficient; SE via bootstrap; b means bounded

# Parameter values are now different, but I don't know why...

#### 3. Get joint precision report
Opt$SD = sdreport( Obj, getJointPrecision=TRUE )


#### 4. Generate samples

# where is mu and prec coming from?

# Function to generate samples
#* taking 500 realizations of nodes
#* mean var-cor
#* generate grid
sample_SE = function( variable_name, n_samples = 500, mu, prec ){
  # Sample from GMRF using sparse precision
  #* GMRF = generalized ... random factor???
  #* is this on the nodes or the grid?
  #* this function could come out of this sample_SE loop
  #* 
  #* function that generates the precision
  rmvnorm_prec <- function(mu, prec, n.sims) {
    #* rename the zs so not overwriting
    z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
    L <- Matrix::Cholesky(prec, super=TRUE)
    z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z #Lt= lower triangular
    z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
    z <- as.matrix(z)
    return(mu + z)
  }
  u_zr = rmvnorm_prec( mu=mu, prec=prec, n.sims=n_samples)
  # Calculate REPORTed variable for each sample
  for( rI in 1:n_samples ){
    Var = Obj$report( par=u_zr[,rI] )[[variable_name]]  #* Var = variable?
    if(is.vector(Var)) Var = as.array(Var)
    if(rI==1) Var_zr = Var
    if(rI>=2){
      Var_zr = abind::abind( Var_zr, Var, along=length(dim(Var))+1 )
    }
  }
  # Return value
  return( apply(Var_zr, MARGIN=1:(length(dim(Var_zr))-1), FUN=sd) )
}
#* what is coming out is the standard deviation
#* read in mu and var-cov of nodes, use to generate realization of the nodes and repeat it

###* Function above: Sampling from the precision matrix with n_samples = number of samples, mu mean, prec = ?? variance, but from where, the sdreport?



