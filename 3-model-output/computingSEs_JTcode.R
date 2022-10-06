# Suggested by Jim Thorson and modified by Janelle Morano



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


# Optimize
Opt = nlminb( start=Obj$par, obj=Obj$fn, gr=Obj$gr ) 
 #nlminb(startp, objective function); like TMB but not as efficient; SE via bootstrap; b means bounded; optimization to minimize (sum of squares maybe?) using the parameters, the function that may be the model, and gr=gradient=slope

# TRY OPTIM instead of nlminb. Can we generate a Hessian matrix to get the cross correlations?

# Parameter values are now different, because of the optimization process

#* Get joint precision report
Opt$SD = sdreport( Obj, getJointPrecision=TRUE )


# Function to generate samples
#* taking 500 realizations of nodes
#* mean var-cor
#* generate grid

#* sample n values with a given mu and a given joint precision matrix to generate a standard error (called whatever you want "variable_name") of those samples
sample_SE = function( variable_name, n_samples = 500, mu, prec ){
  #
  # Sample from GMRF using sparse precision
  #* function to generate a complete precision matrix using the sparse precision matrix, mu, and number of samples given
  #* testing...mu = Obj$env$last.par.best, 
  #* WHAT IS MU REPRESENTING HERE? The optimized parameters? The diagonal?
  #*           n.sims = 100,
  #*           prec=Opt$SD$jointPrecision
  #*  ... rmvnorm_prec(mu, prec, n.sims)         
  rmvnorm_prec <- function(mu, prec, n.sims) {
    #* rename the zs so not overwriting
    z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims) 
      ## matrix of samples of mu where the number of rows = the number of mu values 
      ## number of columns = number of simulations, where the numbers (WHAT NUMBERS?) are randomly selected following a normal distribution. 
    L <- Matrix::Cholesky(prec, super=TRUE) 
      ## Cholesky decomp of precision matrix (precision matrix is a square matrix)
    
    #* take inverse of the transpose of the Cholesky decomposition of the precision matrix z matrix of mu and number of simulations (WHAT DOES THIS REPRESENT?) and multiply it by  TO GET WHAT? (I don't understand what the "system =" does)
    z2 <- Matrix::solve(L, z, system = "Lt") ## solving ax = b; z = Lt^-1 %*% z 
    # lt <- Matrix::solve(L, z, system = "Lt")
    z3 <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
    # pt <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
    z4 <- as.matrix(z3)
    return(mu + z4) #temp set to "ok"
  }
  u_zr = rmvnorm_prec( mu=mu, prec=prec, n.sims=n_samples)
  
  # Calculate REPORTed variable for each sample
  for( rI in 1:n_samples ){
    # Var = Obj$report( par=u_zr[,rI] )[[variable_name]]  #* NOT SURE WHAT'S HAPPENING HERE.. Function that takes a parameter and turns it into a list?? IT LOOKS LIKE IT'S TRYING TO GRAB A VALUE OR VARIABLE FROM THE PAR OBJECT FROM THE FIT BUT BASED ON THE SAMPLED PREC MATRIX; u_zr[,rI] = column seq through n_samples,
    Var = Obj$report( par=u_zr[,1] )[[yhat_g]] #this is for testing
    
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



#* Generate SEs
SE_g = sample_SE( variable_name="yhat_g", mu=Obj$env$last.par.best, prec=Opt$SD$jointPrecision )

# ###*trying function and applying to previous fit data and fudging this
# load("/Users/janellemorano/Git/AtlanticMenhaden/model-output/Atlantic-menhaden-distribution-model-20220401_output.RData")
# jointp <- as.matrix(fit.spring$parameter_estimates$SD$jointPrecision)
# ###*Obj below...trying to figure out what is needed for sdreport()
# Opt$SD = sdreport( Obj, getJointPrecision=TRUE )
# SE_g = sample_SE( variable_name="yhat_g", mu=fit.spring$Obj$env$last.par.best, prec=Opt$SD$jointPrecision )