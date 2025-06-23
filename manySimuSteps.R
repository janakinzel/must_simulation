# nsim: number of simulation steps
# n: number of individuals
# bh: list of constant baseline hazard for each transition
# betas: list with vector of coefficients for each transition
# X_lowdim: covariates with non-zero effect on any transition, nrow=n, ncol=length of beta vectors
many_SimuSteps <- function(nsim,n,bh,betas){
  count_nonzero01 <- rep(0,length(betas[[1]]))
  count_nonzero02 <- rep(0,length(betas[[1]]))
  count_nonzero12 <- rep(0,length(betas[[1]]))
  
  for(i in 1:nsim){
    X_lowdim <- matrix(rnorm(100*9),nrow=100)
    sim <- simulateIDMLowdim(n,bh,betas,X_lowdim)
    nonzero <- analyze_simu(sim)
    count_nonzero01 <- count_nonzero01 + nonzero[[1]]
    count_nonzero02 <- count_nonzero02 + nonzero[[2]]
    count_nonzero12 <- count_nonzero12 + nonzero[[3]]
  }
  return(list(count_nonzero01/nsim,count_nonzero02/nsim,count_nonzero12/nsim))
}