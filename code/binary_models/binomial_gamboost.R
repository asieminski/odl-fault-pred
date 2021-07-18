rm(list = ls())
library(mboost)
source("code/formula_full.R")
nb <- readRDS("res/nb_data.rds")
dt_trans <- readRDS("res/data_for_modelling/all_data/dt_trans.rds")
dt_validation_weights <- readRDS("res/data_for_modelling/all_data/dt_validation_weights.rds")
ctrl <- boost_control(trace = TRUE, nu = 0.3, mstop = 1, risk = "oobag")
zero_form <- full_f$sigma |> 
  deparse() |> 
  paste(collapse = "") |>  
  gsub(pattern = "faultCount", replacement = "zeroFault") |> 
  as.formula()
mod <- gamboost(zero_form, data = dt_trans, 
                family = Binomial(type = "glm"), 
                control = ctrl, 
                weights = 1-dt_validation_weights,
                oobweights = dt_validation_weights)
risk(mod)
beepr::beep()
# Check if everything works ok
# -dbinom(x = (dt_trans$zeroFault == "zero")[as.logical(dt_validation_weights)],
#        size = 1,
#        prob = fitted(mod, type = "response")[as.logical(dt_validation_weights)],
#        log = TRUE) |> sum()

optimise_iter <- function(object){
  converging <- TRUE
  iter_no_improv <- 50
  error <- FALSE
  while(converging){
    old_min_risk <- min(risk(object))
    old_iter <- mstop(object)
    new_iter <- old_iter + iter_no_improv
    tryCatch(
      expr = {object[new_iter, return = FALSE]}, 
      error = \(e){
        object[old_iter, return = FALSE]
        error <<- TRUE
      })
    if(error){
      cat("\nERROR TRIGGERED\n", "LAST LogLik:", -old_min_risk)
      return(-old_min_risk)
    } else {
      new_min_risk <- min(risk(object))
      if(old_min_risk == new_min_risk){
        cat("\nOLD_MIN_RISK = NEW_MIN_RISK\n", "LAST LogLik:", -old_min_risk)
        converging <- FALSE
        return(object)
      }
    }  
  }
}

mod_optimised <- optimise_iter(mod)
mstop(mod_optimised) <- which.min(risk(mod_optimised)) - 1
saveRDS(mod_optimised, "res/bernoulli_models/bern_gam_mod_holdout_p3.rds")
beepr::beep()
# 2966.499

#saveRDS(mod, "res/bernoulli_models/bern_gam_mod_16000.rds")
# Cross-validate

#saveRDS(cvm, "res/bernoulli_models/bern_gam_cv_16000.rds")


