library(tidyverse)

d2 <- read_rds("data/trainingData/trainingData_1_2.rds") |>
  as_tibble()

nb <- read_csv("data/nb_data.csv") |> 
  select(-X1) |> 
  as.matrix()
row.names(nb) <- colnames(nb)
class(nb) <- "gra"

# Why do data for the 3-5 day forecast not have failure counts?
# What is the difference between snow height and depth?
# WHY ARE THE DIFFERENCES IN DATES 0 OR 1 INSTEAD OF 1 OR 2?
# abs(as.Date(d2$dateOfForecast) - as.Date(d2$faultDate)) |> table()
# d2$day |> table()

dt <- d2 |> 
  select(-scheduledForecast, -snow_height) |> #sF is useless; s_h has too many NAs 
  mutate_at(c("icing", "day", "regionCode"), as.factor) |> #Binary variables as factors
  mutate_if(is.numeric, \(x) x-mean(x)) |> #Centre numeric predictors...
  mutate(faultCount = d2$faultCount) |> #Except for the response variable
  mutate_if(is.character, as.factor) |> #Character variables as factors 
  mutate( #Ordinal variables as ordered factors
    Intercept = 1,
    risk = factor(risk, ordered = TRUE, 
                  levels = c("Green", "Amber", "Red")),
    lightningCat = as.ordered(lightningCat)
  ) |> 
  mutate_at(c("dateOfForecast", "faultDate"), as.Date)

# Add faultID for faults with the same region
dt_id <- dt |> 
  group_by(faultDate, region, regionCode) |> 
  nest() |> 
  ungroup() |> 
  mutate(faultID = factor(1:n())) |> 
  unnest(data)

# Centre numerical predictors
dt_cen <- dt_id |> 
  mutate(across(c(temp_max:snow_depth), \(x) x-mean(x))) 

# Create a resampling matrix
set.seed(1)
# Initialise
n_resamples <- 25
n_clusters <- dt_cen$faultID |> 
  as.numeric() |> 
  max()
cvm <- matrix(0, nrow = nrow(dt_cen), ncol = n_resamples)
# Create an indicator matrix, where 
# 1 means the sample is included
# 0 means it's excluded.
for(i in 1:n_resamples){
  included_clusters <- sample(unique(dt_cen$faultID),
                              size = n_clusters / 2,
                              replace = FALSE)
  cvm[,i] <- dt_cen$faultID %in% included_clusters |> 
    as.numeric()
}

# Save data
write_rds(nb, "res/nb_data.rds")
write_rds(dt_id, "res/ready_train_1_2.rds")
write_rds(dt_cen, "res/cen_ready_train_1_2.rds")
write_rds(cvm, "res/cvm_25.rds")



# library(mboost)
# library(BayesX)
# gamboost(faultCount~ bmrf(regionCode,bnd=nb), data = dt)

# # Check which columns contain only integers
# is_int <- function(df){
#   df |>
#     select_if(is.numeric) |>
#     mutate_all(\(x) (x %% 1) != 0) |>
#     summarise_all(\(x) sum(x, na.rm = TRUE) == 0)
# }
# 
# # Why are these columns not in the 3-5 days data?
# colnames(d2)[!colnames(d2) %in% colnames(d5)]
# colnames(d5)[!colnames(d5) %in% colnames(d2)]
# 
# # "wind_gust_max" "wind_gust_min" in d2 but just "wind_gust" in d5
# d2[,c("wind_gust_max", "wind_gust_min")] |> summary()
# d5[,"wind_gust"] |> summary()
# 
# # "lightningCat" in d2 but "lightning" in d5
# d2[,"lightningCat"] |> table()
# d5[,"lightning"] |> table()
# 
# bind_rows(is_int(d2), is_int(d5))
# 
# d2$wind_mean
# d5$wind_mean
# # Why is d2's wind mean a real number but d5's and integer?
# 
# # Cyclic spline of the wind direction variable?
# mean(d2$faultCount!=0)
# rem <- d2$faultCount[d2$faultCount!=0]%%1
# whole <- d2$faultCount[d2$faultCount!=0]%/%1
# table(whole)
# rem/whole |> hist()
# d2 |>
#   mutate(frac = faultCount %% 1 == 0) |>
#   count(faultCount, frac) |>
#   arrange(frac, -faultCount )







