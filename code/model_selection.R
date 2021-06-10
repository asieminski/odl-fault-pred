# Should I stratify based on the date for cross-validation?

library(gamlss.dist)
library(gamboostLSS)
library(stabs)
source("code/formula.R")
nb <- readRDS("res/nb_data.rds")
dt <- readRDS("res/cen_ready_train_1_2.rds")
cv_folds <- readRDS("res/cvm_25.rds")

# as.families(ZAIG())
# as.families(ZAGA())

ctrl <- boost_control(trace = TRUE, nu = 0.5, mstop = 1000)

mean_mod <- gamboostLSS(full_f, data = dt,
            method = "noncyclic", families = as.families(ZAGA()), 
            control = ctrl)

# Based on the example from mboost::cvm documentation
cl <- makeCluster(4) 
clusterExport(cl = cl, 
              varlist = c("nb"))

myApply <- function(X, FUN, ...) {
  myFun <- function(...) {
    library("gamboostLSS") 
    library("gamlss.dist")
    FUN(...)
  }
  ## further set up steps as required
  parLapply(cl = cl, X, myFun, ...)
}
cvm <- cvrisk(mean_mod, folds = cv_folds, papply = myApply)
stopCluster(cl)
plot(cvm)
mean_mod[710] |> plot()
## selected base-learners:
lapply(coef(hurdle), names)

stb <- stabsel(mean_mod, mstop = 20000,
        q = 35, PFER = 2,
        papply = myApply)

cvm <- cvrisk(mean_mod, folds = cv_folds, papply = lapply)
mstop(cvm)
plot(cvm)
plot(mean_mod)

summary(simple_mod)
par(mfrow = c(2,2))
plot(simple_mod)
print(simple_mod)
coef(simple_mod)


