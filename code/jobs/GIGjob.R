library(gamlss.dist)
library(gamboostLSS)
source("code/formula_full.R")
nb <- readRDS("res/nb_data.rds")
dt_fault_trans <- 
  readRDS("res/data_for_modelling/faults_only/dt_fault_trans.rds")
dt_fault_val_wg <- 
  readRDS("res/data_for_modelling/faults_only/dt_fault_validation_weights.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, mstop = 0, 
                      risk = "oobag")

mod <- gamboostLSS(formula = full_f[1:3], 
                   data = dt_fault_trans, method = "noncyclic", 
                   families = as.families(GIG(), stabilization = "MAD"))
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
      saveRDS(object, "res/positive_continuous_models/GIGmod.rds")
    }
  }  
}



# GGmod <- optimise_gamboostLSS(form = full_f[1:3], dist = GG())
# saveRDS(GGmod, "res/positive_continuous_models/GGmod.rds")
# GIGmod <- optimise_gamboostLSS(form = full_f[1:3], dist = GIG())
# saveRDS(GIGmod, "res/positive_continuous_models/GIGmod.rds")
# BCTomod <- optimise_gamboostLSS(form = full_f, dist = BCTo())
# saveRDS(BCTomod, "res/positive_continuous_models/BCTomod.rds")
# BCCGomod <- optimise_gamboostLSS(form = full_f, dist = BCCGo())
# saveRDS(BCCGomod, "res/positive_continuous_models/BCCGomod.rds")
# 

# risk(new_mod)
# oob <- as.logical(dt_fault_val_wg)
# -dGA(x = dt_fault_trans$faultCount[oob],
#   mu = fitted(new_mod, type = "response")$mu[oob],
#   sigma = fitted(new_mod, type = "response")$sigma[oob], log = TRUE) |>
#   sum()

# risk(mod)
# #saveRDS(mod, "res/positive_continuous_models/generalised_gamma1.rds")
# beepr::beep()
# mstop(mod) <- 2e4
# #saveRDS(mod, "res/positive_continuous_models/generalised_gamma20000.rds")
# #7hrs 11mins
# gamboostLSS::boost
# cl <- makeCluster(4) 
# clusterExport(cl = cl, varlist = c("nb"))
# myApply <- function(X, FUN, ...) {
#   myFun <- function(...) {
#     library("gamboostLSS") 
#     library("gamlss.dist")
#     FUN(...)
#   }
#   parLapply(cl = cl, X, myFun, ...)
# }
# cvm <- cvrisk(mod, folds = focv_folds, papply = myApply,
#               trace = TRUE)
# stopCluster(cl)
# #saveRDS(cvm, "res/positive_continuous_models/cv_generalised_gamma20000.rds")
