library(tidyverse)
# Load norm_transform funciton
source("code/functions/scale_df.R")
test <- readRDS("data/testPeriodDecJan_1_2.rds")
test |> 
  as_tibble() |> 
  mutate_at("dateOfForecast", lubridate::dmy) |> summary()
# 2 Dec 2019 till 27 Jan 2020
short_data <- readRDS("res/augmented_data/augmented_1_2_data.rds")
short_data$dateOfForecast |> summary()
# 17 sep is the last date 2019

val_data <- readRDS("data/validationData/validationData_1_2.rds")
val_data |> as_tibble() |> mutate_at("dateOfForecast", as.Date) |> summary()
# Last date is 19 sep and fault counts are here

test_data <- readRDS("data/decJanTestingData/testPeriodDecJan_1_2.rds")
test_data$dateOfForecast

df <- readRDS("res/data_for_modelling/all_data/dt_trans.rds")
df |> 
  filter(day == "1") |> 
  select(faultDate, faultCount, region) |> 
  arrange(faultDate) |> 
  filter(faultDate > as.Date("2017-09-19"),
         region %in% c("Western Isles", "Shetland")) 

df |> group_by(region, regionCode) |> 
  summarise() |> 
  arrange(region)
