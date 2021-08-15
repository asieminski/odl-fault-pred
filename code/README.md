This repository contains the code for the MSc dissertation "Forecasting overhead distribution line failures using weather data and gradient-boosted location,scale, and shape models." authored by Antoni Sieminski and supervised by Dr Carl Donovan at the University of St Andrews in 2021.

The *wrangling* folder contains scripts for preparing the 1-2 days and 3-5 days training and testing data for modelling. It also establishes a split into training and validation data.

The *functions* folder contains functions to scale and centre data in a reversible way, which allowed us to easily reapply the same scaling and centring rules to the new data. It also contains functions to augment data with parameter estimates, log likelihood, and quantile residuals.

The *formulae* folder contains formulas for the models. An additional file formula_full.R was placed outside of the folder, so that all of the scripts were still compatible with it.

The *binary_models*, *jobs* and *positive_continuous_models* folders all contain scripts for training and tuning the hyperparameters of the models presented in this study. Much of the code here is unnecessarily repetitive due to unintelligible software errors.

The *binary_models* folder contains binary models including xgboost and gamboost trained on 1-2 days forecasts and a refitted gamboost model trained on training and validation data. It also contains a gamboost model fitted on 3-5 days training data and refitted on training+validation data.

The *positive_continuous_models* and *jobs* folders contain a variety of gamboostLSS models for different distributions and an blackboostLSS model (based solely on conditional inference trees) for the Box-Cox t distribution (all trained on 1-2 days training data). The *positive_continuous_models* folder also contains the Box-Cox t gamboostLSS models refitted on training+validation data for 1-2 days and 3-5 days forecasts.

The *illustratory_graphics* folder contains scripts for the analysis of the results and creating graphics from them.






