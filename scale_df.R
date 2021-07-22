# TESTS
# df <- readRDS("data/decJanTestingData/testPeriodDecJan_1_2.rds")
# means_before <- colMeans(df[, which(sapply(df, is.numeric))])
# sds_before <- (df[, which(sapply(df, is.numeric))]) |> 
#   lapply(sd)
# df <- scale_df(df)
# means_after <- colMeans(df[, which(sapply(df, is.numeric))], na.rm = TRUE)
# all(abs(means_after) < 1e-15)
# df <- unscale_df(df)
# colMeans(df[, which(sapply(df, is.numeric))]) - means_before < 1e-16


scale_df <- function(df, na.rm = TRUE){
  # Check if scale_df was used on the df
  if("scaled_df" %in% class(df)){
    warning("The df has already been scaled.")
    return(df)
  }
  # Indicate that the object has been scaled with this functin
  class(df) <- c(class(df), "scaled_df")
  # Add means and sds for each numeric column to the object
  num_cols <- which(sapply(df, is.numeric))
  ndf <- df[, num_cols]
  attr(df, "means") <- sapply(ndf, mean, na.rm = na.rm)
  attr(df, "sds") <- sapply(ndf, sd, na.rm = na.rm)
  # Scale and centre each column
  for(num_var in colnames(ndf)){
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
