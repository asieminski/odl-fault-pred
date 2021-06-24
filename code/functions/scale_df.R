# TESTS
df <- readRDS("data/trainingData/trainingData_1_2.rds")

df_sc <- scale_df(df, omit = "faultCount")
attributes(df_sc)

scale_df(df, use_scaling_from = df_sc, omit = "rain_min")
# 
# (df$rain_max == 5) |> mean()
# 
# scale_df(df, )
# means_before <- colMeans(df[, which(sapply(df, is.numeric))])
# sds_before <- (df[, which(sapply(df, is.numeric))]) |>
#   lapply(sd)
# means_after <- colMeans(df[, which(sapply(df, is.numeric))], na.rm = TRUE)
# all(abs(means_after) < 1e-15)
# df <- unscale_df(df)
# colMeans(df[, which(sapply(df, is.numeric))]) - means_before < 1e-16

scale_df <- function(df, use_scaling_from = NULL, na.rm = TRUE,
                     omit = NULL){
  # Check if scale_df was used on the df
  if("scaled_df" %in% class(df)){
    stop("The df has already been scaled.")
    return(df)
  }
  # Indicate that the object has been scaled with this function
  class(df) <- c(class(df), "scaled_df")
  # Add means and sds for each numeric column to the object
  if(!is.null(omit)){
    if(!all(is.character(omit))){
      stop("omit needs to be a character vector")
    }
    omit_cols <- !(colnames(df) %in% omit)
  } else {
    omit_cols <- rep(TRUE, ncol(df))
  }
  # Normalise using the provided data.frame
  if(!is.null(use_scaling_from)){
    # Check if the data.frame has the right characteristics
    if(!("scaled_df" %in% class(use_scaling_from))){
      stop("The supplied data.frame must be of class \"scaled_df\"")
    }
    cat("Scaling using the supplied data.frame\n")
    attr(df, "means") <- attr(use_scaling_from, "means")
    attr(df, "sds") <- attr(use_scaling_from, "sds")
  # If no exemplar provided, estimates means and sds
  } else {
    num_cols <- which(sapply(df, is.numeric) & omit_cols)
    ndf <- df[, num_cols]
    attr(df, "means") <- sapply(ndf, mean, na.rm = na.rm)
    attr(df, "sds") <- sapply(ndf, sd, na.rm = na.rm)
  }
  # Scale and centre each column
  for(num_var in names(attr(df, "means"))){
    df[, num_var] <- 
      (df[, num_var] - attr(df, "means")[num_var])/
      attr(df, "sds")[num_var]
  }
  return(df)
}  

unscale_df <- function(df){
  # Check if scale_df was used on the df
  if(!("scaled_df" %in% class(df))){
    stop("The df object needs to be created with scale_df()")
  }
  # Reverse the scaling and centring
  scaled_cols <- names(attributes(df)$means)
  for(col in scaled_cols){
    df[, col] <-  df[, col] * attributes(df)$sds[col] + 
      attributes(df)$means[col] 
  }
  attr(df, "means") <- NULL
  attr(df, "sds") <- NULL
  
  return(df)
}  
