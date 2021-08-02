rm(list = ls())
library(mboost)
library(BayesX)
source("code/formulae/three_to_five_formula.R")
dt_trans <- readRDS("res/data_for_modelling/three_to_five_all_data/dt_trans.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, 
                      mstop = 0, risk = "inbag")
zero_form <- full_f$sigma |> 
  deparse() |> 
  paste(collapse = "") |>  
  gsub(pattern = "faultCount", replacement = "zeroFault") |> 
  as.formula()
mod <- gamboost(zero_form, data = dt_trans, 
                family = Binomial(type = "glm"), 
                control = ctrl)
mstop(mod) <- 2168
saveRDS(mod, "res/final_models/3_4_5_day_forecasts/bern_gam_2168_p3.rds")
# LL = -6084.51
# avgLL = -0.5847751