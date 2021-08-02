library(tidyverse)
library(gamboostLSS)
library(gamlss.dist)

augment_zadj <- function(mod_pos, mod_bern, data_pos, all_data){
  # Augment all data with bernoulli model
  all_augmented <- all_data |> 
    mutate(xi0 = fitted(mod_bern, type = "response")) 
  # Augment data with positive counts with BCTo model
  faults_augmented <- bind_cols(data_pos, fitted(mod_pos, type = "response") |> 
                                  data.frame() |> 
                                  as_tibble())|> 
    dplyr::mutate(llBCT = dBCTo(faultCount, 
                                mu = mu, sigma = sigma, 
                                nu = nu, tau = tau, log = TRUE)) |> 
    dplyr::select(dateOfForecast, regionCode, faultDate, mu, sigma, nu, tau, llBCT)
  # Join them and compute log likelihood
  all_with_ll <- all_augmented |> 
    left_join(faults_augmented) |> 
    mutate(LL = ifelse(faultCount == 0,
                       log(xi0), log(1-xi0) + llBCT))
  
}

augment_new_zadj <- function(mod_pos, mod_bern, data_pos, all_data){
  # Augment all data with bernoulli model
  all_augmented <- all_data |> 
    mutate(xi0 = predict(mod_bern, type = "response", newdata = all_data)) 
  # Augment data with positive counts with BCTo model
  faults_augmented <- bind_cols(data_pos, predict(mod_pos, type = "response", newdata = data_pos) |> 
                                  data.frame() |> 
                                  as_tibble())|> 
    dplyr::mutate(llBCT = dBCTo(faultCount, 
                                mu = mu, sigma = sigma, 
                                nu = nu, tau = tau, log = TRUE)) |> 
    dplyr::select(dateOfForecast, regionCode, faultDate, mu, sigma, nu, tau, llBCT)
  # Join them and compute log likelihood
  all_with_ll <- all_augmented |> 
    left_join(faults_augmented) |> 
    mutate(LL = ifelse(faultCount == 0,
                       log(xi0), log(1-xi0) + llBCT))
  
}
