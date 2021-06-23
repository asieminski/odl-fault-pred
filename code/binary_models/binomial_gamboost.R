rm(list = ls())
library(mboost)
source("code/formula_full.R")
nb <- readRDS("res/nb_data.rds")
dt <- readRDS("res/cen_sc_ready_train_1_2.rds")
cv_folds <- readRDS("res/cv_folds.rds")
ctrl <- boost_control(trace = TRUE, nu = 0.3, mstop = 1)
zero_form <- full_f$sigma |> 
  deparse() |> 
  paste(collapse = "") |>  
  gsub(pattern = "faultCount", replacement = "zeroFault") |> 
  as.formula()
mod <- gamboost(zero_form, data = dt, 
                family = Binomial(type = "glm"), 
                control = ctrl)
mstop(mod) <- 16000
#saveRDS(mod, "res/bernoulli_models/bern_gam_mod_16000.rds")
# Cross-validate
cl <- makeCluster(4) 
clusterExport(cl = cl, varlist = c("nb"))
myApply <- function(X, FUN, ...) {
  myFun <- function(...) {
    FUN(...)
  }
  parLapply(cl = cl, X, myFun, ...)
}
cvm <- cvrisk(mod, folds = cv_folds, papply = myApply)
stopCluster(cl)
#saveRDS(cvm, "res/bernoulli_models/bern_gam_cv_16000.rds")


