# norm_fit()
# Finds means and standard deviations for all numeric variables in 
# ..a dataset except for the ones specified in the "omit" argument.
# Inputs:
# df -- a data.frame object with numeric columns
# omit -- a character vector with varaible names to be unaffected by the funciton
# na.rm -- logical, specifies if NA values should be removed for the scaling
#
# norm_transform()
# Scales and centres all numeric variables in a dataset.
# Inputs:
# df -- a data.frame to be transformed
# norm_fit -- a named list with two vectors of equal length: "means" and "sds"
# ..both with variable names as their values
# Examples
# df <- readRDS("data/trainingData/trainingData_1_2.rds")
# (nf <- norm_fit(df))
# (df_sc <- norm_transform(df, nf))
norm_fit <- function(df, omit = NULL, na.rm = TRUE){
  # Add means and sds for each numeric column to the object
  if(!is.null(omit)){
    if(!all(is.character(omit))){
      stop("omit needs to be a character vector")
    }
    omit_cols <- !(colnames(df) %in% omit)
  } else {
    omit_cols <- TRUE
  }
  num_cols <- which(sapply(df, is.numeric) & omit_cols)
  ndf <- df[, num_cols]
  means <- sapply(ndf, mean, na.rm = na.rm)
  sds <- sapply(ndf, sd, na.rm = na.rm)
  norm_info <- list(means = means, sds = sds)
  return(norm_info)
}
norm_transform <- function(df, norm_fit){
  # Scale and centre each column
  for(var in names(norm_fit$means)){
    df[, var] <- 
      (df[, var] - norm_fit$means[var])/
      norm_fit$sds[var]
  }
  return(df)
}
