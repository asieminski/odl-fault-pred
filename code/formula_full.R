# Code structure inspired by Thomas et al. 2017 supplementary materials
# Requires: nb, wind_direction_bk
# the package does not allow for centering of cyclic smooths, but they tend to a flat line
# anyway under lambda-> inf
cr <- TRUE
it <- FALSE
com <- 1

mu_f <- faultCount ~ 
  
  # -- Main effects --#
  # intercept
  bols(Intercept, intercept = it) +
  
  # Binary variables
  bols(icing, intercept = it) + 
  
  # Continuous Variables
  bols(wind_gust_max, intercept = it) + 
  bbs(wind_gust_max, center = cr, df = com) +
  bols(wind_mean, intercept = it) + 
  bbs(wind_mean, center = cr, df = com) +
  # Low temperature can lead to icing and higher line density
  bols(temp_min, intercept = it) +
  bbs(temp_min, center = cr, df = com) +
  # Temperature can expand the length of the lines making them more vulnerable
  bols(temp_max, intercept = it) +
  bbs(temp_max, center = cr, df = com) +
  
  # Not sure how can rain affect the distribution lines
  bols(rain_min, intercept = it) +
  bbs(rain_min, center = cr, df = com) +
  
  bols(rain_max, intercept = it) +
  bbs(rain_max, center = cr, df = com) +
  
    
  # Continuous Cyclic
  bols(wind_direction, intercept = it) +
  bbs(wind_direction, center = cr, df = com) +
  
  # Ordinal variables
  bols(risk, intercept = it, df = com) +
  bols(lightningCat, intercept = it, df = com) +
  
  
  # Gust X Direction #
  #bols(wind_gust_max, intercept = it) +  #Already added above
  #bols(wind_direction, intercept = it) + #Already added above
  bols(wind_direction, by = wind_gust_max, intercept = it) +
  bspatial(wind_direction, wind_gust_max, center = cr, df = com) +
    
  # Direction X Region
  #bols(wind_direction, intercept = it) + #Already added above
  #bols(regionCode, intercept = it, df = com) + #Already added above
  bols(wind_direction, by = regionCode, intercept = it, df = com) +
  bbs(wind_direction, by = regionCode, center = cr, df = com) +
  
  # Gust X Region
  #bols(wind_gust_max, intercept = it) +  #Already added above
  #bols(regionCode, intercept = it, df = com) + #Already added above
  bols(wind_gust_max, by = regionCode, intercept = it, df = com) +
  bbs(wind_gust_max, by = regionCode, center = cr, df = com) +
    
  # Direction X Gust X Region
  # All main effects and two-way linear interactions were added above
  bols(wind_direction, by = wind_gust_max, 
       intercept = it, df = com) %X% bols(regionCode, intercept = it, df = com) +
  bspatial(wind_direction, wind_gust_max, by = regionCode,
           center = cr, df = com) +
  
  # -- Random effects --#
  bols(regionCode, intercept = it, df = com) +
  bmrf(regionCode, bnd = nb, center = cr, df = com)

nu_f <- mu_f
# Assuming the weather forecast is unbiased, 
# the day of the forecast should only affect the error variance
sigma_f <- mu_f |> 
  deparse() |> 
  paste(collapse="") |> 
  paste("+ bols(day, intercept = it, df = com)") |> 
  as.formula()

full_f <- list(mu = mu_f, sigma = sigma_f, nu = nu_f)



