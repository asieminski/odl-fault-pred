source("code/functions/augment_zadj.R")
library(gamboostLSS)
mod_pos <- read_rds("res/final_models/1_2_day_forecasts/final_pos_cont_BCTo_5146.rds")
mod_bern <- read_rds("res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")

all_data <- read_rds("res/data_for_modelling/all_data/dt_trans.rds")
faults_only <- read_rds("res/data_for_modelling/faults_only/dt_fault_trans.rds")

augment_zadj(mod_pos = mod_pos,
             mod_bern = mod_bern, 
             data_pos = faults_only,
             all_data = all_data
              )
