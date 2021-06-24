library(tidyverse)
library(mboost)
library(gamlss.dist)
mbern <- read_rds("res/bernoulli_models/bern_gam_mod_16000.rds")
cvbern <- read_rds("res/bernoulli_models/bern_gam_cv_16000.rds")
mstop(mbern) <- mstop(cvbern)
dt_cen <- read_rds("res/cen_sc_ready_train_1_2.rds")
dt_cen_all <- dt_cen |> 
  mutate(prob_no_fault = fitted(mbern, type = "response")[[1]],
         llbern = dbinom(
           as.numeric(faultCount != 0),
           prob = prob_no_fault, size = 1, log = TRUE))



mgg <- read_rds("res/positive_continuous_models/generalised_gamma20000.rds")
cvgg <- read_rds("res/positive_continuous_models/cv_generalised_gamma20000.rds")
dt_cen_fault <- read_rds("res/fault_only_cen_sc_ready_train_1_2.rds")

dt_GG <- dt_cen_fault |> 
  bind_cols(as.data.frame(fitted(mgg, type = "response"))) |> 
  mutate(llGG = dGG(faultCount, mu = mu, 
                             sigma = sigma, nu = nu, 
                             log = TRUE)) |> 
  dplyr::select(regionCode, faultDate, day, llGG)

dt_all_ll <- dt_cen_all |> 
  left_join(dt_GG) |> 
  mutate(ll_zadj_GG = 
           ifelse(faultCount == 0, llbern,
                  log(1-prob_no_fault)+llGG))
dt_all_ll$ll_zadj_GG |> sum()
lapply(risk(mgg), min)
?recipes::recipes
varimp(mgg$mu) |> plot()
library(gamlss.inf)
gen.Zadj(family = "GG")
plot(dGGZadj(0:3e4*1e-4, mu = 2, sigma = 1, nu = 1, xi0 = 0.5))
