library(tidyverse)
library(gamboostLSS)
library(gamlss.dist)
mod <- read_rds("res/final_models/1_2_day_forecasts/final_pos_cont_BCTo_5146.rds")
mod_bern <- read_rds("res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")
all_data <- read_rds("res/data_for_modelling/all_data/dt_trans.rds")
faults_only <- read_rds("res/data_for_modelling/faults_only/dt_fault_trans.rds")


augment_zadj <- function(mod, mod_bern, all_data, faults_only){
  all_augmented <- all_data |> 
    mutate(xi0 = fitted(mod_bern, type = "response")) 
  
  faults_augmented <- bind_cols(faults_only, fitted(mod, type = "response") |> 
                             data.frame() |> 
                             as_tibble())|> 
    dplyr::mutate(llBCT = dBCTo(faultCount, 
                                mu = mu, sigma = sigma, 
                                nu = nu, tau = tau, log = TRUE),
                  BCTcdf = pBCTo(faultCount, 
                                 mu = mu, sigma = sigma, 
                                 nu = nu, tau = tau)) |> 
    dplyr::select(dateOfForecast, regionCode, faultDate, 
                  mu, sigma, nu, tau, 
                  llBCT, BCTcdf)
  
  all_with_ll <- all_augmented |> 
    left_join(faults_augmented) |> 
    mutate(LL = ifelse(faultCount == 0,
                       log(xi0), log(1-xi0) + llBCT),
           CDF = ifelse(faultCount == 0,
                        xi0, xi0 + (1-xi0)*BCTcdf))
}
write_rds(augment_zadj(mod, mod_bern, all_data, faults_only), 
          "res/augmented_data/augmented_1_2_data.rds")

mod <- read_rds("res/final_models/3_4_5_day_forecasts/final_pos_cont_BCTo_588.rds")
mod_bern <- read_rds("res/final_models/3_4_5_day_forecasts/bern_gam_2168_p3.rds")
all_data <- read_rds("res/data_for_modelling/three_to_five_all_data/dt_trans.rds")
faults_only <- read_rds("res/data_for_modelling/three_to_five_faults_only/dt_fault_trans.rds")

write_rds(augment_zadj(mod, mod_bern, all_data, faults_only), 
          "res/augmented_data/augmented_3_5_data.rds")


# library(DataExplorer)
# plot_scatterplot(all_with_ll, by = "LL")
# 
# 
# 
# 
# 
# 
# base_learners <- mod |> 
#   pluck("sigma") |> 
#   varimp() |> 
#   data.frame() |> 
#   pluck("blearner")
# 
# is_linear <- function(base_learner){
#   str_detect(base_learner, "bols")&
#   !str_detect(base_learner, "risk")&
#   !str_detect(base_learner, "lightningCat")
#   }
# 
# mod_to_varimp <- function(mod){
#   mod |> 
#     varimp() |> 
#     data.frame() |> 
#     mutate(term_type = ifelse(is_linear(blearner),
#                            "linear", "smooth"),
#            percent_reduction = reduction/sum(reduction)*100)
# }
# 
# varimps <- list()
# length(varimps) <- length(mod)
# for(i in 1:length(mod)){
#   varimps[[i]] <-
#     mod_to_varimp(mod[[i]]) |> 
#     mutate(id = i,
#            parameter = names(mod)[i])
# }
# varimps <- do.call("rbind", varimps)
