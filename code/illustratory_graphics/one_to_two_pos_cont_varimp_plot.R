# Load packages
library(tidyverse)
library(gamboostLSS)
library(gamlss.dist)
# Load models and data
mod <- read_rds("res/final_models/1_2_day_forecasts/final_pos_cont_BCTo_5146.rds")
mod_bern <- read_rds("res/final_models/1_2_day_forecasts/bern_gam_4098_p3.rds")
all_data <- read_rds("res/data_for_modelling/all_data/dt_trans.rds")
faults_only <- read_rds("res/data_for_modelling/faults_only/dt_fault_trans.rds")

# Augment data with preds, density, and cumulative density
augment_zadj <- function(mod, mod_bern, all_data, faults_only){
  all_augmented <- all_data |> 
    mutate(xi0 = fitted(mod_bern, type = "response")) 
  
  faults_augmented <- bind_cols(faults_only, fitted(mod, type = "response") |> 
                             data.frame() |> 
                             as_tibble())|> 
    dplyr::mutate(llBCT = dBCTo(faultCount, 
                                mu = mu, sigma = sigma, 
                                nu = nu, tau = tau, log = TRUE)) |> 
    dplyr::select(dateOfForecast, regionCode, faultDate, mu, sigma, nu, tau, llBCT)
  
  all_with_ll <- all_augmented |> 
    left_join(faults_augmented) |> 
    mutate(LL = ifelse(faultCount == 0,
                       log(xi0), log(1-xi0) + llBCT))
}

# Save augmented data for 3-5 day forecast
write_rds(augment_zadj(mod, mod_bern, all_data, faults_only), 
          "res/augmented_data/augmented_1_2_data.rds")
# Save augmented data for 3-5 day forecasts
mod <- read_rds("res/final_models/3_4_5_day_forecasts/final_pos_cont_BCTo_588.rds")
mod_bern <- read_rds("res/final_models/3_4_5_day_forecasts/bern_gam_2168_p3.rds")
all_data <- read_rds("res/data_for_modelling/three_to_five_all_data/dt_trans.rds")
faults_only <- read_rds("res/data_for_modelling/three_to_five_faults_only/dt_fault_trans.rds")
write_rds(augment_zadj(mod, mod_bern, all_data, faults_only), 
          "res/augmented_data/augmented_3_5_data.rds")

# Load augmented data
short_data <- read_rds("res/augmented_data/augmented_1_2_data.rds")
source("code/functions/param_risk_reduction.R")
param_ll_increases <- param_risk_reduction(mod)

# Save variable names in LATEX format
variables <- mod |> 
  pluck("sigma") |> 
  varimp() |> 
  data.frame() |> 
  mutate_at("variable", as.character) |> 
  distinct(variable) |> 
  arrange(variable) |> 
  mutate(tidy_var_names = c("Day", 
                            "Wind direction $\\times$ Region",
                            "Wind direction $\\times$ Region $\\times$ Wind gust max.",
                            "Wind direction $\\times$ Region $\\times$ Wind mean",
                            "Icing", 
                            "Intercept", 
                            "Lightning category",
                            "Rain max.", 
                            "Rain min.", 
                            "Region",
                            "Region $\\times$ Wind gust max.",
                            "Region $\\times$ Wind mean",
                            "Risk",
                            "Temperature max.", 
                            "Temperature min.", 
                            "Wind direction",
                            "Wind direction $\\times$ Wind gust max.",
                            "Wind direction $\\times$ Wind mean",
                            "Wind gust max.", 
                            "Wind mean"
                            ))
# Create a table with keys
base_learners <- mod |> 
  pluck("sigma") |> 
  varimp() |> 
  data.frame() |> 
  pluck("blearner")
# Include informaiton about whether the term is smooth or linear
is_linear <- function(base_learner){
  str_detect(base_learner, "bols")&
  !str_detect(base_learner, "risk")&
  !str_detect(base_learner, "lightningCat")
  }
# Funciton for computing variable importance and term type
mod_to_varimp <- function(mod){
  mod |> 
    varimp() |> 
    data.frame() |> 
    mutate(term_type = ifelse(is_linear(blearner),
                           "linear", "smooth"),
           percent_reduction = reduction/sum(reduction)*100)
}
# Execute to change the data
varimps <- list()
length(varimps) <- length(mod)
for(i in 1:length(mod)){
  varimps[[i]] <-
    mod_to_varimp(mod[[i]]) |> 
    mutate(id = i,
           parameter = names(mod)[i])
}
varimps <- do.call("rbind", varimps)


# Use variable importances to create data for plotting
# N for the nonzero data
n_pos <- sum(short_data$faultCount > 0)
# New names, compute aggregated likelihoods, remove intercept,
# and useless predictors
varimps_with_names <- varimps |> 
  left_join(variables) |> 
  filter(reduction > 0) |> 
  group_by(parameter) |> 
  mutate(param_reduction = sum(reduction),
         param_total_reduction = n_pos*param_reduction,
         facetting_parameter = 
           str_c("$\\",parameter, ": \\Delta\\ell_{\\", 
                 parameter, "}\\approx", 
                 round(param_total_reduction/1000,0), ",000$"),
         total_reduction = reduction * n_pos) |> 
  group_by(parameter, variable) |> 
  mutate(variable_total_reduction = sum(total_reduction)) |> 
  ungroup() |> 
  filter(variable != "Intercept")

# Helper funcitons for ordering within facets
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}
scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}
# Plot variable importances
(massive_varimp_plot <- varimps_with_names |> 
ggplot(aes(x = total_reduction/1000, 
           y = reorder_within(tidy_var_names,
                              variable_total_reduction,
                              facetting_parameter), 
           fill = term_type))+
  geom_col(alpha = .7, color = "black")+
  facet_wrap(~facetting_parameter, nrow = 4, 
             scales = "free_y")+
  scale_y_reordered()+
  labs(x = "$\\Delta\\ell$ in $000$'s", y = "", legend = "")+
  theme_minimal()+
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  
  scale_fill_manual(name = "Term type", 
                    values = c("darkgrey", "firebrick"))
)  
# Save
tikzDevice::tikz(file = "res/figures/bcto_varimp_plot.tex", 
                 width = 5, height = 9)
print(massive_varimp_plot)
dev.off()


wind_gust_df <- mod$nu |> 
  model.frame() |> 
  as.data.frame() |> 
  dplyr::select(wind_gust_max)
y_hat <- predict(mod$nu,
          newdata = wind_gust_df, 
          which = "bols(wind_gust_max, intercept = FALSE)")
wind_gust_df <- read_rds("res/data_for_modelling/faults_only/dt_fault.rds") |> 
  dplyr::select(wind_gust_max)

lm(y_hat ~ wind_gust_df$wind_gust_max)
short_data
p_partial <- wind_gust_df |> 
  mutate(y_hat = y_hat) |> 
  ggplot(aes(x = wind_gust_max, y = y_hat))+
    geom_line(colour = "firebrick") +
    geom_rug(aes(x = wind_gust_max, y = NULL), colour = "firebrick") +
    theme_minimal()+
    labs(x = "Wind gust max.", y = "$\\hat{\\nu}$") +
    annotate("text", label = "$\\hat{\\nu} \\approx -1.46 + 0.04 \\times$ Wind gust max.",
             x = 37.5, y = 1.5, colour = 'firebrick')+
    geom_density(aes(y = ..density..*30 -1), colour = "grey") +
    annotate("text", label = "Scaleless density \napproximation",
             x = 82, y = -0.3, colour = "grey")
p_partial
tikzDevice::tikz(file = "res/figures/partial_nu_wind_gust_1_2.tex", 
                 width = 5, height = 3)
print(p_partial)
dev.off()  
