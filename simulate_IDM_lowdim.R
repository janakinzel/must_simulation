#devtools::install_github("janakinzel/simIDM@cox") # modified version supporting hazards from Cox model
library(simIDM)


# n: number of individuals
# bh: list of constant baseline hazard for each transition
# betas: list with vector of coefficients for each transition
# X_lowdim: covariates with non-zero effect on any transition, nrow=n, ncol=length of beta vectors
simulate_IDM_lowdim <- function(n,bh,betas,X_lowdim){
  transition <- list(simIDM::cox_transition(h01=bh[[1]],h02=bh[[2]],h12=bh[[3]]))
  
  times <- simIDM::getOneClinicalTrial(nPat=n,transitionByArm=transition,dropout=list(rate=0,time=1),
                                       betas=betas,X=X_lowdim)
  covariables <- data.frame(id=1:n,X_lowdim)
  df <- merge(times,covariables,by="id")
  df <- df[,-c(6,7,8,9)]
  return(df)
}