library(gamlss.dist)
library(gamboostLSS)
library(BayesX)
source("code/formula_full.R")
nb <- readRDS("res/nb_data.rds")
dt_fault_trans <- 
  readRDS("res/data_for_modelling/faults_only/dt_fault_trans.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, 
                      mstop = 5146, risk = "inbag")

mod <- gamboostLSS(formula = full_f, 
                   data = dt_fault_trans, method = "noncyclic", 
                   families = as.families(BCTo(), stabilization = "MAD"))
# Due to a software bug gamboostLSS ignores the boost_control 
#margument and always sets m_stop to 100
# so this is a necessary workaround
mstop(mod) <- 5146
saveRDS(mod, "res/final_models/1_2_day_forecasts/final_pos_cont_BCTo_5146.rds")
# LL = -493.953

