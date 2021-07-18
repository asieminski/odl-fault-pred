# Code structure inspired by Thomas et al. 2017 supplementary materials
# Requires: nb, wind_direction_bk
# the package does not allow for centering of cyclic smooths, but they tend to a flat line
# anyway under lambda-> inf
mu_f <- faultCount ~ 
  
  # -- Main effects --#
  # Intercept
  bols(Intercept, intercept = FALSE) +

  # Categorical variables
  bols(icing, intercept = FALSE) + # Binary
  bols(wind_dir_factor, intercept = FALSE, df = 1) + # Unordered
  bols(risk, intercept = FALSE, df = 1) + # Ordinal
  bols(lightningCat, intercept = FALSE, df = 1) + # Ordinal

  # Continuous Variables
  bols(wind_gust_max, intercept = FALSE) +
  bbs(wind_gust_max, center = TRUE, df = 1) +
  bols(wind_mean, intercept = FALSE) +
  bbs(wind_mean, center = TRUE, df = 1) +
  # Low temperature can lead to icing and higher line density
  bols(temp_min, intercept = FALSE) +
  bbs(temp_min, center = TRUE, df = 1) +
  # Temperature can expand the length of the lines making them more vulnerable
  bols(temp_max, intercept = FALSE) +
  bbs(temp_max, center = TRUE, df = 1) +
  # Not sure how can rain affect the distribution lines
  bols(rain_min, intercept = FALSE) +
  bbs(rain_min, center = TRUE, df = 1) +
  bols(rain_max, intercept = FALSE) +
  bbs(rain_max, center = TRUE, df = 1) +
 
  # Wind-related interactions
  # Gust X Direction #
  bols(wind_gust_max, by = wind_dir_factor,
       intercept = FALSE, df = 1) +
  bbs(wind_gust_max, by = wind_dir_factor,
      center = TRUE, df = 1) +
  # Gust X Region
  bols(wind_gust_max, by = regionCode, intercept = FALSE, df = 1) +
  bbs(wind_gust_max, by = regionCode, center = TRUE, df = 1) +

  # Mean X Direction #
  bols(wind_mean, by = wind_dir_factor,
       intercept = FALSE, df = 1) +
  bbs(wind_mean, by = wind_dir_factor,
      center = TRUE, df = 1) +
  # Mean X Region
  bols(wind_mean, by = regionCode, intercept = FALSE, df = 1) +
  bbs(wind_mean, by = regionCode, center = TRUE, df = 1) +
 
  # Direction X Region
  bols(dir_X_regionCode, intercept = FALSE, df = 1) +

  # Direction X Gust X Region
  bols(wind_gust_max, by = dir_X_regionCode,
       intercept = FALSE, df = 1) +
  bbs(wind_gust_max, by = dir_X_regionCode,
      center = TRUE, df = 1) +

  # Direction X Mean X Region
  bols(wind_mean, by = dir_X_regionCode,
       intercept = FALSE, df = 1) +
  bbs(wind_mean, by = dir_X_regionCode,
      center = TRUE, df = 1) +

  # # -- Random effects --#
  bols(regionCode, intercept = FALSE, df = 1) +
  bmrf(regionCode, bnd = nb, center = TRUE, df = 1)

nu_f <- #faultCount ~ bols(Intercept, intercept = FALSE)
  mu_f
# Assuming the weather forecast is unbiased, 
# the day of the forecast should only affect the error variance
sigma_f <- #faultCount ~ bols(Intercept, intercept = FALSE)
  mu_f |>
    deparse() |>
    paste(collapse="") |>
    paste("+ bols(day, intercept = FALSE, df = 1)") |>
    as.formula()

tau_f <- sigma_f

full_f <- list(mu = mu_f, sigma = sigma_f, nu = nu_f, tau = tau_f)



