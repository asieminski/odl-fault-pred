# Code structure inspired by Thomas et al. 2017 supplementary materials
# Requires: nb, wind_direction_bk
mu_f <- faultCount ~ 
  
  # -- Main effects --#
  # intercept
  bols(Intercept, intercept = FALSE) +
  
  # Binary variables
  bols(icing, intercept = FALSE) + 
  
  # Continuous Variables
  bols(wind_gust_max, intercept = FALSE) + 
  bbs(wind_gust_max, center = TRUE, df = 1) +
  # Continuous Cyclic
  bols(wind_direction, intercept = FALSE) +
  bbs(wind_direction, center = TRUE, df = 1,
      cyclic = TRUE, boundary.knots = wind_direction_bk) +
  
  # Ordinal variables
  bols(risk, intercept = FALSE, df = 1) +
  bols(lightningCat, intercept = FALSE, df = 1) +
  
  # -- Random effects --#
  bols(regionCode, intercept = FALSE, df = 1) +
  bmrf(regionCode,bnd=nb, center = TRUE, df = 1)
  
  # -- Two-way interactions --#
  # Wind_direction X Region
    # lin-lin
    bols(wind_direction, by = regionCode, intercept = FALSE) +
    # smooth-lin
    bbs(wind_direction, by = regionCode, center = TRUE, df = 1,
      cyclic = TRUE, boundary.knots = wind_direction_bk) +
  # Wind_direction X Wind_Gust
    # lin-lin
    bols(wind_direction, by = wind_gust_max, intercept = FALSE) + 
    # smooth-lin
    bbs(wind_direction, by = wind_gust_max, center = TRUE, df = 1,
        cyclic = TRUE, boundary.knots = wind_direction_bk) +
    # lin-smooth
    bbs(wind_gust_max, by = wind_direction, center = TRUE, df = 1) + 
    # smooth-smooth
    bbs(wind_direction, center = TRUE, df = 1, 
      cyclic = TRUE, boundary.knots = wind_direction_bk
      ) %X% bbs(wind_gust_max, center = TRUE, df = 1)
  # Region X Wind_Gust
    bols()
    bbs(wind_gust_max, by = wind_direction, center = TRUE, df = 1) + 
  
    
sigma_f <- faultCount ~ bols(Intercept, intercept = FALSE) #+ bols(day, intercept = FALSE)
nu_f <- faultCount ~ bols(Intercept, intercept = FALSE) #+ bols(icing, intercept = FALSE)
full_f <- list(mu = mu_f, sigma = sigma_f, nu = nu_f)



