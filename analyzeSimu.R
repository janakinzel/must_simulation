library(survival)

# low-dimensional analysis: standard Cox model on every transition separately
# goal: see whether the "right" non-zero variables for each transition are found in low-dim

# sim: a dataset resulting from the function simulate_IDM_lowdim (containing the transition times and the covariates with non-zero effect on any transition)
analyzeSimu <- function(sim){
  # Cox models on subsets
  covariates <- setdiff(names(sim), c("id","from","to","entry","exit"))
  formula_to1 <- as.formula(paste("Surv(entry,exit,to==1)~", paste(covariates,collapse="+")))
  formula_to2 <- as.formula(paste("Surv(entry,exit,to==2)~", paste(covariates,collapse="+")))
  
  cox01 <- coxph(formula_to1,sim,subset=from==0) # transition 0 -> 1
  cox02 <- coxph(formula_to2,sim,subset=from==0) # transition 0 -> 2
  cox12 <- coxph(formula_to2,sim,subset=from==1) # transition 1 -> 2
  
  # extract covariates with significant effects
  coef01 <- as.data.frame(summary(cox01)$coefficients)
  coef02 <- as.data.frame(summary(cox02)$coefficients)
  coef12 <- as.data.frame(summary(cox12)$coefficients)
  
  sign01 <- ifelse(coef01$`Pr(>|z|)`<0.05,1,0)
  sign02 <- ifelse(coef02$`Pr(>|z|)`<0.05,1,0)
  sign12 <- ifelse(coef12$`Pr(>|z|)`<0.05,1,0)
  
  
  return(list(sign01,sign02,sign12))
}