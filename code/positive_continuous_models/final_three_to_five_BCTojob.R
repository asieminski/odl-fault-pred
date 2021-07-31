library(gamlss.dist)
library(gamboostLSS)
source("code/formulae/three_to_five_formula.R")
dt_fault_trans <- 
  readRDS("res/data_for_modelling/three_to_five_faults_only/dt_fault_trans.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, 
                      mstop = 1, risk = "inbag")

mod <- gamboostLSS(formula = full_f,
                   data = dt_fault_trans, method = "noncyclic", 
                   families = as.families(BCTo(), stabilization = "MAD"))
# Due to a software bug gamboostLSS ignores the boost_control 
#margument and always sets m_stop to 100
# so this is a necessary workaround
mstop(mod) <- 588
saveRDS(mod, "res/final_models/3_4_5_day_forecasts/final_pos_cont_BCTo_588.rds")
# GG stabilisation = "none" failed to converge
# GG stabilisation - "MAD" failed to converge

# 8682.044
# 1.301461
