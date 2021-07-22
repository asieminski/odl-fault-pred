fit_bb <- function(x){
  library(gamlss.dist)
  library(gamboostLSS)
  library(partykit)
  library(GA)
  dt_trans <- readRDS("res/data_for_modelling/faults_only/dt_fault_trans.rds") |> 
    dplyr::select(-dateOfForecast, -region, -faultDate, 
           -zeroFault, -Intercept, -dir_X_regionCode, -wind_dir_factor)
  dt_validation_weights <- 
    readRDS("res/data_for_modelling/faults_only/dt_fault_validation_weights.rds")
  
  max_depth <- binary2decimal(x[1:2]) + 1 # max_depth from 1 to 4
  mtry <- binary2decimal(x[3:5]) +1 # mtry from 1 to 8
  shrinkage <- 0.01
  min_bucket <- binary2decimal(x[6:7]) * 200 + 50 
  cat(
    "\n=======================================\n",
    "max_depth =", max_depth, 
    "mtry =", mtry, 
    "shrinkage =", shrinkage,
    "\n=======================================\n"
    )
  error <- FALSE
  tree_ctrl <-  partykit::ctree_control(
    teststat = "quad", 
    testtype = "Teststatistic", # Raw test statistic is used
    mincriterion = 0, # The tree is always split based on the test stat
    minsplit = 200, # Min obs on node to be considered for splitting
    minbucket = min_bucket, # Min obs on terminal node
    maxdepth = max_depth,
    saveinfo = FALSE,
    mtry = mtry, # Number of columns samples
    maxvar = 3, # Maximum number of interactions
    stump = FALSE)
  
  bctrl <- boost_control(mstop = 50, nu = shrinkage,
                         risk = "oobag", trace = TRUE)
  
  object <- blackboostLSS(
    faultCount ~ .,
    data = dt_trans,
    method = "noncyclic",
    families = as.families(BCTo(), stabilization = "MAD"),
    control = bctrl,
    tree_control = tree_ctrl,
    weights = (1-dt_validation_weights),
    oobweights = dt_validation_weights
  )
  
  converging <- TRUE
  iter_no_improv <- 50
  while(converging){
    old_min_risk <- min(attr(object, "combined_risk")())
    old_iter <- mstop(object)
    new_iter <- old_iter + iter_no_improv
    tryCatch(
      expr = {object[new_iter, return = FALSE]}, 
      error = \(e){
        object[old_iter, return = FALSE]
        error <<- TRUE
      })
    if(error){
      cat("\nERROR TRIGGERED\n")
      return(-old_min_risk)
    } else {
      new_min_risk <- min(attr(object, "combined_risk")())
      cat(
        "\n=======================================\n",
        "max_depth=", max_depth, "mtry=", mtry, 
        "\nBest result so far: Risk=", new_min_risk,
        "\n=======================================\n")
      if(old_min_risk == new_min_risk){
        cat("\nOLD_MIN_RISK = NEW_MIN_RISK\n")
        converging <- FALSE
        return(-old_min_risk)
      }
    }  
  }
  
}
#max_depth = 1 mtry = 7 dist= 2 shrinkage = 0.013 
#max_depth = 1 mtry = 5 dist= 3 shrinkage = 0.011

# Set mstop to the iteration with the lowest risk
# Note that the first 3 risks are offset values
# mstop(object) <- which.min(attr(object, "combined_risk")()) - 3
# 
# library(doParallel)
# cl <- makeCluster(detectCores(),
#                   type = "PSOCK")
# clusterExport(cl, varlist = c("dt", "fodt", "fit_bb"))
# clusterEvalQ(cl = cl, expr = {
#              library(gamlss.dist)
#              library(gamboostLSS)
#              library(partykit)
#              library(GA)})
# 
# registerDoParallel(cl)

# gig no err
# 3, 6, GG
# Approx 4 hrs for 6 sequential runs on an old laptop
tic <- Sys.time()
GA_7_10 <- GA::ga(type = 'binary',fitness = fit_bb, nBits = 7,
       maxiter = 3, popSize = 8, seed = 1, keepBest = TRUE, 
       parallel = TRUE, monitor = TRUE)
toc <- Sys.time()
print(toc - tic)
saveRDS(GA_7_10, "res/positive_continuous_models/GA_3_8.rds")
summary(GA_7_10)

# GA settings: 
# Type                  =  binary 
# Population size       =  8 
# Number of generations =  3 
# Elitism               =  1 
# Crossover probability =  0.8 
# Mutation probability  =  0.1 

# > max_depth
# [1] 2
# > mtry
# [1] 7
# > min_bucket
# [1] 450



