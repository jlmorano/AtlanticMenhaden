# Suggested by Jim Thorson

# Optimize
Opt = nlminb( start=Obj$par, obj=Obj$fn, gr=Obj$gr )
Opt$SD = sdreport( Obj, getJointPrecision=TRUE )

# Function to generate samples
sample_SE = function( variable_name, n_samples = 500, mu, prec ){
  # Sample from GMRF using sparse precision
  rmvnorm_prec <- function(mu, prec, n.sims) {
    z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
    L <- Matrix::Cholesky(prec, super=TRUE)
    z <- Matrix::solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
    z <- Matrix::solve(L, z, system = "Pt") ## z = Pt    %*% z
    z <- as.matrix(z)
    return(mu + z)
  }
  u_zr = rmvnorm_prec( mu=mu, prec=prec, n.sims=n_samples)
  # Calculate REPORTed variable for each sample
  for( rI in 1:n_samples ){
    Var = Obj$report( par=u_zr[,rI] )[[variable_name]]
    if(is.vector(Var)) Var = as.array(Var)
    if(rI==1) Var_zr = Var
    if(rI>=2){
      Var_zr = abind::abind( Var_zr, Var, along=length(dim(Var))+1 )
    }
  }
  # Return value
  return( apply(Var_zr, MARGIN=1:(length(dim(Var_zr))-1), FUN=sd) )
}

SE_g = sample_SE( variable_name="D_gct", mu=Obj$env$last.par.best, prec=Opt$SD$jointPrecision )
