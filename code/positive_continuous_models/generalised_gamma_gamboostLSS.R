library(gamlss.dist)
library(gamboostLSS)
source("code/formula_full.R")
nb <- readRDS("res/nb_data.rds")
fodt <- readRDS("res/fault_only_cen_sc_ready_train_1_2.rds")
focv_folds <- readRDS("res/fault_only_cv_folds.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, mstop = 1)
mod <- gamboostLSS(full_f, data = fodt, method = "noncyclic", 
                   families = as.families(GG()), control = ctrl)
#saveRDS(mod, "res/positive_continuous_models/generalised_gamma1.rds")
beepr::beep()
mstop(mod) <- 2e4
saveRDS(mod, "res/positive_continuous_models/generalised_gamma20000.rds")
#7hrs 11mins

cl <- makeCluster(4) 
clusterExport(cl = cl, varlist = c("nb"))
myApply <- function(X, FUN, ...) {
  myFun <- function(...) {
    library("gamboostLSS") 
    library("gamlss.dist")
    FUN(...)
  }
  parLapply(cl = cl, X, myFun, ...)
}
cvm <- cvrisk(mod, folds = focv_folds, papply = myApply,
              trace = TRUE)
stopCluster(cl)
#saveRDS(cvm, "res/positive_continuous_models/cv_generalised_gamma20000.rds")
