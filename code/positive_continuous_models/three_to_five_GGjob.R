library(gamlss.dist)
library(gamboostLSS)
source("code/formulae/three_to_five_formula.R")
dt_fault_trans <- 
  readRDS("res/data_for_modelling/three_to_five_faults_only/dt_fault_trans.rds")
dt_fault_val_wg <- 
  readRDS("res/data_for_modelling/three_to_five_faults_only/dt_fault_validation_weights.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, mstop = 0, 
                      risk = "oobag")

mod <- gamboostLSS(formula = full_f[1:3], 
                   data = dt_fault_trans, method = "noncyclic", 
                   families = as.families(GG(), stabilization = "none"))
# Use update() due to a bug, which neglected the out-of-bag weights
object <- update(mod, 
                 weights = 1-dt_fault_val_wg, 
                 oobweights = dt_fault_val_wg,
                 risk = "oobag",
                 mstop = 1, trace = TRUE)
converging <- TRUE
iter_no_improv <- 50
error <- FALSE
while(converging){
  old_min_risk <- min(attr(object, "combined_risk")())
  cat("\nold_min_risk = ", old_min_risk)
  old_iter <- mstop(object)
  new_iter <- old_iter + iter_no_improv
  tryCatch(
    expr = {object[new_iter, return = FALSE]}, 
    error = \(e){
      object[old_iter, return = FALSE]
      error <<- TRUE
    })
  if(error){
    cat("\nERROR TRIGGERED\n", "LAST Iter:", old_iter)
    return(-old_min_risk)
  } else {
    new_min_risk <- min(attr(object, "combined_risk")())
    if(old_min_risk == new_min_risk){
      cat("\nOLD_MIN_RISK = NEW_MIN_RISK\n", "LAST LogLik:", -old_min_risk)
      converging <- FALSE
      saveRDS(object, "res/positive_continuous_models/three_to_five_GG_gam.rds")
    }
  }  
}
which.min(attr(object, "combined_risk")()) - 4
min(attr(object, "combined_risk")())
min(attr(object, "combined_risk")())/sum(dt_fault_val_wg)



# mstop = 5975
# LAST LogLik: -2235.718
# -2235.718/1720 = -1.299836