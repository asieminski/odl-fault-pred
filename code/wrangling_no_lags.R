library(tidyverse)
library(mboost)
source("code/functions/scale_df.R")
d2 <- read_rds("data/trainingData/trainingData_1_2.rds") |>
  as_tibble()
# Load the mrf smoothing matrix
nb <- read_csv("data/nb_data.csv") |> 
  select(-X1) |> 
  as.matrix()
row.names(nb) <- colnames(nb)
class(nb) <- "gra"

directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
breaks <- 0:9*45 - 22.5
d2$wind_dir_factor <- cut(d2$wind_direction, breaks = breaks, labels = directions)

dt <- d2 |> 
  select(-scheduledForecast, -snow_height) |> #sF is useless; s_h has too many NAs 
  mutate_at(c("icing", "day", "regionCode"), as.factor) |> #Binary variables as factors
  mutate(faultCount = d2$faultCount) |> #Except for the response variable
  mutate_if(is.character, as.factor) |> #Character variables as factors 
  mutate( #Ordinal variables as ordered factors
    Intercept = 1,
    risk = factor(risk, ordered = TRUE, 
                  levels = c("Green", "Amber", "Red")),
    lightningCat = as.ordered(lightningCat),
    dir_X_regionCode = factor(stringr::str_c(wind_dir_factor, regionCode)),
    zeroFault = factor(ifelse(faultCount == 0, "zero", "positive"))
  ) |> 
  mutate_at(c("dateOfForecast", "faultDate"), as.Date) 

# REMOVED LAGGED VARIABLES
# |> 
#   group_by(regionCode, day) |> 
#   mutate(lag1_faultCount = lag(faultCount, 1L),
#          lag2_faultCount = lag(faultCount, 2L)) |> #Add Lagged fault counts
#   ungroup()

# # NOT NEEDED
# # Add faultID for faults with the same region
# dt_id <- dt |> 
#   group_by(faultDate, region, regionCode) |> 
#   nest() |> 
#   ungroup() |> 
#   mutate(faultID = factor(1:n())) |> 
#   unnest(data)

# USE THE CENTERING FUNCITON INSTEAD
# # Centre numerical predictors
# dt_cen_sc <- dt_id |> 
#   mutate(
#     across(
#       c(temp_max:snow_depth, 
#         lag1_faultCount, 
#         lag2_faultCount), 
#       \(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
#     )
#   ) |> 
#   mutate(lag1_faultCount = ifelse(
#     day == '2', lag1_faultCount, 0
#   ))

# big_cen_sc_fit <- dt_id |> 
#   norm_fit(omit = c("faultCount")) 
# dt_cen_sc2$lag1_faultCount <-  ifelse(
#     dt_cen_sc2$day == '2', dt_cen_sc2$lag1_faultCount, 0)

last_train_day <- max(dt$faultDate) - 365

# Full df normalisation
dt_fit <- norm_fit(dt, omit = c("Intercept", "faultCount"))
dt_trans <- norm_transform(dt, norm_fit = dt_fit)
dt_validation_weights <- as.integer(dt$faultDate > last_train_day)
  # Save data
  path_all_data <- "res/data_for_modelling/all_data/"
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
  path_faults_only <- "res/data_for_modelling/faults_only/"
  write_rds(dt_fault, str_c(path_faults_only, "dt_fault.rds"))
  write_rds(dt_fault_fit, str_c(path_faults_only, "dt_fault_fit.rds"))
  write_rds(dt_fault_trans, str_c(path_faults_only, "dt_fault_trans.rds"))
  write_rds(dt_fault_validation_weights, 
            str_c(path_faults_only, "dt_fault_validation_weights.rds"))

# NO CYCLICAL SPLINES WILL BE IMPLEMENTED
# wind_direction_boundry_knots <- 
#   (c(0, 360) - mean(dt_id$wind_direction))/sd(dt_id$wind_direction)

# VALIDATION WILL NOT BE BASED ON CV
# set.seed(1)
# cv_folds <- cv(rep(1, nrow(dt_cen_sc)),
#                type = "kfold", B = 4, 
#                strata = factor(dt_cen_sc$faultDate))

# # Save data
# write_rds(nb, "res/nb_data.rds")
# write_rds(cv_folds, "res/cv_folds.rds")
# write_rds(dt_id, "res/ready_train_1_2.rds")
# write_rds(dt_cen_sc, "res/cen_sc_ready_train_1_2.rds")
# write_rds(wind_direction_boundry_knots, "res/wind_direction_boundry_knots.rds")

# fault_only_dt_cen_sc <- dt_id |> 
#   filter(faultCount != 0) |> 
#   mutate(
#     across(
#       c(temp_max:snow_depth, 
#         lag1_faultCount, 
#         lag2_faultCount), 
#       \(x) (x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
#     )
#   ) |> 
#   mutate(lag1_faultCount = ifelse(
#     day == '2', lag1_faultCount, 0
#   ))
# write_rds(fault_only_dt_cen_sc, "res/fault_only_cen_sc_ready_train_1_2.rds")
# fault_only_cv_folds <- cv_folds[dt$faultCount != 0, ]
# 
# write_rds(fault_only_cv_folds, "res/fault_only_cv_folds.rds")
