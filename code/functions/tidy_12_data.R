# Tidy up 1-2 day data
tidy_12_data <- function(df, norm_fit){
  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  breaks <- 0:9*45 - 22.5
  df$wind_dir_factor <- cut(df$wind_direction, breaks = breaks, labels = directions)
  
  dt <- df |> 
    dplyr::select(-scheduledForecast, -snow_height) |> #sF is useless; s_h has too many NAs 
    mutate_at(c("icing", "day", "regionCode"), as.factor) |> #Binary variables as factors
    mutate_if(is.character, as.factor) |> #Character variables as factors 
    mutate( #Ordinal variables as ordered factors
      Intercept = 1,
      risk = factor(risk, ordered = TRUE, 
                    levels = c("Green", "Amber", "Red")),
      lightningCat = as.ordered(lightningCat),
      dir_X_regionCode = factor(stringr::str_c(wind_dir_factor, regionCode)),
      zeroFault = factor(ifelse(faultCount == 0, "zero", "positive"))
    )
  
  dt_trans <- norm_transform(dt, norm_fit = norm_fit)
  
  return(dt_trans)
}
library(tidyverse)
library(lubridate)
source("code/functions/scale_df.R")
val_data <- readRDS("data/validationData/validationData_1_2.rds")
norm_fit <- read_rds("res/data_for_modelling/all_data/dt_fit.rds")
old_lightning <- readRDS("res/data_for_modelling/all_data/dt_trans.rds") |> 
  pluck("lightningCat")
# Change data such that it fits the training data format
val_cen <- val_data |>   
  as_tibble() |> 
  mutate_at(c("dateOfForecast", "faultDate"), as.Date) |> 
  tidy_12_data(norm_fit) |> 
  mutate(lightningCat = factor(lightningCat, 
                               levels = levels(old_lightning), 
                               ordered = TRUE
                               ))
# Load models
mod_bern <- readRDS("res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")
mod_pos <- readRDS("res/final_models/1_2_day_forecasts/final_pos_cont_BCTo_5146.rds")
source("code/functions/augment_zadj.R")
old_df <- readRDS("res/data_for_modelling/all_data/dt_trans.rds")
# No variables are outside of range of its predecessors
df_summ <- mutate(val_cen, id = "test") |> 
  bind_rows(mutate(old_df, id = "train")) |> 
  group_by(id) |> 
  summarise(across(where(is.numeric), .fns = list(mean = mean))) 

# This package cannot produce a simple prediction on new data
# 
library(mboost)

# output_df <- val_cen |> filter(faultCount < 0)
# for(i in 1:(2*87)){
#   
#   cat("\niter", i, "\n")
#   
#   ind_upper <- i*30
#   ind_lower <- 1 + (i-1)*30 
#   df <- val_cen |> 
#     slice(ind_lower:ind_upper)
#   tryCatch(new_df_batch <- 
#     augment_new_zadj(mod_pos = mod_pos,
#              mod_bern = mod_bern, 
#              data_pos = df |> filter(faultCount > 0),
#              all_data = df))
#   output_df <- bind_rows(output_df, new_df_batch)
# }

base_learners <- names(mod_bern$baselearner)
f_x <- rep(mod_bern$offset, nrow(val_cen))
for(i in 1:length(base_learners)){
  print(i)
  f_x <- f_x + 
    as.vector(predict(mod_bern, newdata = val_cen,
                      type = "link", 
                      which = base_learners[i]))
}

xi0 <- exp(f_x)/(1+exp(f_x))
# xi0[1:10] 
# predict(mod_bern, type = "response", newdata = head(val_cen, 10))
K <- length(mod_pos)
f_x <- list()
length(f_x) <- K
for(i in 1:K){
  print(i)
  base_learners <- names(mod_pos[[i]]$baselearner)
  f_x[[i]] <- rep(mod_pos[[i]]$offset, nrow(val_cen))
  
  for(j in 1:length(base_learners)){
    print(j)
    f_x[[i]] <- f_x[[i]] + 
      as.vector(predict(mod_pos[[i]], newdata = val_cen,
                        type = "link", 
                        which = base_learners[j]))
  }
}

for(i in c(1, 2, 4)){
  f_x[[i]] <- exp(f_x[[i]])
}
# predict(mod_pos, type = "response", newdata = head(val_cen))
# lapply(f_x, head)
names(f_x) <- c("mu", "sigma", "nu", "tau")
aug_nz_val_cen <- as_tibble(f_x) |> 
  bind_cols(val_cen)
aug_val_cen <- val_cen |> 
  mutate(xi0 = xi0)
aug_both <- left_join(aug_val_cen, aug_nz_val_cen)

aug_ll <- bind_rows(
  # Zero counts
  aug_both |> 
    filter(faultCount == 0) |> 
    mutate(LL = log(xi0)),
  # Positive counts
  aug_both |> 
    filter(faultCount > 0 ) |> 
    mutate(LL = log(1-xi0) + 
             dBCTo(faultCount, 
                   mu = mu,
                   sigma = sigma,
                   nu = nu,
                   tau = tau, 
                   log = TRUE)
         )
  )


aug_ll$LL |> mean() #-0.5624573

# 
# aug <- readRDS("res/test_data_augmented.rds") 
# 
# ll_zero <- aug |> 
#   filter(faultCount == 0) |> 
#   pluck("xi0") |> log() |> sum()
# 
# ll_all <- aug$LL |> sum()
# 
# ll_notzero <- (1 - aug |> 
#   filter(faultCount != 0) |> 
#   pluck("xi0")) |> log() |> sum()
# 
# (ll_all - ll_zero - ll_notzero)/sum(aug$faultCount != 0)
# 
# saveRDS(aug_ll, "res/1_2_test_data_augmented.rds")
# 


