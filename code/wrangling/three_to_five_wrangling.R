rm(list = ls())
library(tidyverse)
source("code/functions/scale_df.R")
d5 <- read_rds("data/trainingData/trainingData_3_5.rds") |>
  as_tibble()



# lightning <-  lightningCat
# NULL <- risk
# wind_direction <- wind_dir_factor
dt <- d5 |> 
  select(-scheduledForecast, -snow_height) |> #sF is useless; s_h has too many NAs 
  mutate_at(c("icing", "day", "regionCode"), as.factor) |> #Binary variables as factors
  mutate_if(is.character, as.factor) |> #Character variables as factors 
  mutate( #Ordinal variables as ordered factors
    Intercept = 1,
    lightning = as.ordered(lightning),
    dir_X_regionCode = factor(stringr::str_c(wind_direction, regionCode)),
    zeroFault = factor(ifelse(faultCount == 0, "zero", "positive")),
    day = as.ordered(day)
  ) |> 
  mutate_at(c("dateOfForecast", "faultDate"), as.Date) 
last_train_day <- max(dt$faultDate) - 365

# Full df normalisation
dt_fit <- norm_fit(dt, omit = c("Intercept", "faultCount"))
dt_trans <- norm_transform(dt, norm_fit = dt_fit)
dt_validation_weights <- as.integer(dt$faultDate > last_train_day)
# Save data
path_all_data <- "res/data_for_modelling/three_to_five_all_data/"
write_rds(dt, str_c(path_all_data, "dt.rds"))
write_rds(dt_fit, str_c(path_all_data, "dt_fit.rds"))
write_rds(dt_trans, str_c(path_all_data, "dt_trans.rds"))
write_rds(dt_validation_weights, 
          str_c(path_all_data, "dt_validation_weights.rds"))
# Faults only normalisation
dt_fault <- dt |> 
  filter(faultCount != 0)
dt_fault_fit <- norm_fit(dt_fault, omit = c("Intercept", "faultCount"))
dt_fault_trans <- norm_transform(dt_fault, norm_fit = dt_fault_fit)
dt_fault_validation_weights <- as.integer(dt_fault$faultDate > last_train_day)
# Save data
path_faults_only <- "res/data_for_modelling/three_to_five_faults_only/"
write_rds(dt_fault, str_c(path_faults_only, "dt_fault.rds"))
write_rds(dt_fault_fit, str_c(path_faults_only, "dt_fault_fit.rds"))
write_rds(dt_fault_trans, str_c(path_faults_only, "dt_fault_trans.rds"))
write_rds(dt_fault_validation_weights, 
          str_c(path_faults_only, "dt_fault_validation_weights.rds"))
