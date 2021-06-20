library(tidyverse)

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
    dir_X_regionCode = stringr::str_c(wind_dir_factor, regionCode)
  ) |> 
  mutate_at(c("dateOfForecast", "faultDate"), as.Date) |> 
  group_by(regionCode, day) |> 
  mutate(lag1_faultCount = lag(faultCount, 1L),
         lag2_faultCount = lag(faultCount, 2L)) |> #Add Lagged fault counts
  ungroup()

# Add faultID for faults with the same region
dt_id <- dt |> 
  group_by(faultDate, region, regionCode) |> 
  nest() |> 
  ungroup() |> 
  mutate(faultID = factor(1:n())) |> 
  unnest(data)

# Centre numerical predictors
dt_cen_sc <- dt_id |> 
  mutate(
    across(
      c(temp_max:snow_depth, 
        lag1_faultCount, 
        lag2_faultCount), 
      \(x) x-mean(x)/sd(x)
      )
    ) |> 
  mutate(lag1_faultCount = ifelse(
    day == '2', lag1_faultCount, 0
    ))

# Create a resampling matrix
set.seed(1)
# Initialise
n_resamples <- 25
n_clusters <- dt_cen_sc$faultDate |> 
  unique() |> 
  length()

cvm <- matrix(0, nrow = nrow(dt_cen), ncol = n_resamples)
# Create an indicator matrix, where
# 1 means the sample is included
# 0 means it's excluded.
for(i in 1:n_resamples){
  included_clusters <- sample(unique(dt_cen$faultDate),
                              size = n_clusters / 2,
                              replace = FALSE)
  cvm[,i] <- dt_cen$faultDate %in% included_clusters |>
    as.numeric()
}

wind_direction_boundry_knots <- 
  (c(0, 360) - mean(dt_id$wind_direction))/sd(dt_id$wind_direction)

# Save data
write_rds(nb, "res/nb_data.rds")
write_rds(dt_id, "res/ready_train_1_2.rds")
write_rds(dt_cen_sc, "res/cen_sc_ready_train_1_2.rds")
write_rds(cvm, "res/cvm_25.rds")
write_rds(wind_direction_boundry_knots, "res/wind_direction_boundry_knots.rds")





