# Code structure inspired by Thomas et al. 2017 supplementary materials
# Requires: nb, wind_direction_bk
# the package does not allow for centering of cyclic smooths, but they tend to a flat line
# anyway under lambda-> inf
mu_f <- faultCount ~ 
  
  # -- Main effects --#
  # intercept
  bols(Intercept, intercept = FALSE) +

  # Binary variables
  bols(icing, intercept = FALSE) +

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


  # Continuous Cyclic
  bols(wind_direction, intercept = FALSE) +
  bbs(wind_direction, center = TRUE, df = 1) +

  # Ordinal variables
  bols(risk, intercept = FALSE, df = 1) +
  bols(lightningCat, intercept = FALSE, df = 1) +


  # Gust X Direction #
  #bols(wind_gust_max, intercept = FALSE) +  #Already added above
  #bols(wind_direction, intercept = FALSE) + #Already added above
  bols(wind_direction, by = wind_gust_max, intercept = FALSE) +
  bspatial(wind_direction, wind_gust_max, center = TRUE, df = 1) +

  # Direction X Region
  #bols(wind_direction, intercept = FALSE) + #Already added above
  #bols(regionCode, intercept = FALSE, df = 1) + #Already added above
  bols(wind_direction, by = regionCode, intercept = FALSE, df = 1) +
  bbs(wind_direction, by = regionCode, center = TRUE, df = 1) +

  # Gust X Region
  #bols(wind_gust_max, intercept = FALSE) +  #Already added above
  #bols(regionCode, intercept = FALSE, df = 1) + #Already added above
  bols(wind_gust_max, by = regionCode, intercept = FALSE, df = 1) +
  bbs(wind_gust_max, by = regionCode, center = TRUE, df = 1) +

  # Direction X Gust X Region
  # All main effects and two-way linear interactions were added above
  bols(wind_direction, by = wind_gust_max, intercept = FALSE, df = 1) %X% 
    bols(regionCode, intercept = FALSE, df = 1) +
  bspatial(wind_gust_max, wind_direction, center = TRUE, df = 1) %X% 
    bols(regionCode, intercept = FALSE, df = 1) +
  
  # # -- Random effects --#
  bols(regionCode, intercept = FALSE, df = 1) +
  bmrf(regionCode, bnd = nb, center = TRUE, df = 1)

nu_f <- mu_f
# Assuming the weather forecast is unbiased, 
# the day of the forecast should only affect the error variance
sigma_f <- mu_f |> 
  deparse() |> 
  paste(collapse="") |> 
  paste("+ bols(day, intercept = FALSE, df = 1)") |> 
  as.formula()

full_f <- list(mu = mu_f, sigma = sigma_f, nu = nu_f)



