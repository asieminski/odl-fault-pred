library(tidymodels)
library(tidyverse)
library(doParallel)
cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl, cores = detectCores()-1)
# Create CV folds for tidymodels tuning
# Load data
dt_trans <- readRDS("res/data_for_modelling/all_data/dt_trans.rds")
# Leave only the relevant predictors and the response
dt <- dt_trans |> 
  select(-dateOfForecast, -region, -faultDate, 
         -faultCount, -Intercept, -dir_X_regionCode, -day, -wind_dir_factor)

val_weights <- 
  readRDS("res/data_for_modelling/all_data/dt_validation_weights.rds")
# Create a list with indices for analysis and assessment
indices <- list()
is_analysis <- !as.logical(val_weights)
indices[[1]] <- list(analysis = as.integer(which(is_analysis)), 
                     assessment = as.integer(which(!is_analysis)))
# Convert to rsample format
splits <- lapply(indices, make_splits, data = dt)
folds <- manual_rset(splits, stringr::str_c("Split ", 1))

xgbmodel <- parsnip::boost_tree(
  mode = "classification",
  trees = tune(), #nrounds
  learn_rate = tune(), #eta
  sample_size = tune(), #subsample
  mtry = tune(), #colsample_bytree
  min_n = tune(), #min_child_weight
  tree_depth = tune(), #max_depth
  loss_reduction = tune()
) |>
  set_engine("xgboost")
             # , objective = "binary:logistic",
             # lambda=0, alpha=1 ,verbose=1, 
             # eval_metric = "logloss")

preproc <-
  recipe(zeroFault ~ ., data = dt) |> 
  step_ordinalscore(risk, lightningCat) |> 
  step_dummy(regionCode, icing)

xgbwflow <-
  workflow() |>
  add_model(xgbmodel) |>
  add_recipe(preproc)
# Was set to 10 at 19:39 - run 23 till 20:28
max_iter <- 230
# Binomial boost was 0.550211
set.seed(1)
search_res <- xgbwflow |> 
  tune_bayes(
    resamples = folds,
    param_info = finalize(parameters(xgbmodel), dt),
    initial = 15,
    iter = max_iter,
    # How to measure performance?
    metrics = metric_set(mn_log_loss),
    control = control_bayes(no_improve = 30, seed = 1, 
                            verbose = TRUE, save_pred = TRUE)
  )
# Iter @31 using holdout data:
# mtry=5, trees=1351, min_n=39, tree_depth=2, learn_rate=0.0119, loss_reduction=6.61e-09,
# sample_size=0.995
# mn_log_loss=0.5671 (@iter 31)
# so out-of-sample risk = 2975.574

# Using 5 fold CV:
# Stopped after 63 iterations
# mn_log_loss=0.542 (@iter 33)
# mtry=5, trees=869, min_n=8, tree_depth=2,
# learn_rate=0.0965, loss_reduction=0.000207,
# sample_size=0.446
write_rds(search_res, "res/bernoulli_models/xgb_230iter.rds")
rm(list = ls())

