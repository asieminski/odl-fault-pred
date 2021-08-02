rm(list = ls())
library(mboost)
source("code/formula_full.R")
library(BayesX)
nb <- readRDS("res/nb_data.rds")
dt_trans <- readRDS("res/data_for_modelling/all_data/dt_trans.rds")

ctrl <- boost_control(trace = TRUE, nu = 0.3, 
                      mstop = 4098, risk = "inbag")
zero_form <- full_f$sigma |> 
  deparse() |> 
  paste(collapse = "") |>  
  gsub(pattern = "faultCount", replacement = "zeroFault") |> 
  as.formula()
mod <- gamboost(zero_form, data = dt_trans, 
                family = Binomial(type = "glm"), 
                control = ctrl)

saveRDS(mod, "res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")
# inbag risk 11594.94
