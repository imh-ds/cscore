#' Compute Weighted Row Means with Missing Data Handling (Internal)
#'
#' Computes a weighted mean for each row of a data frame or matrix, allowing for
#' missing values. Weights are normalized within each row to ensure they sum to
#' 1 over the non-missing entries.
#'
#' @param df A numeric data frame or matrix. May contain \code{NA} values.
#' @param weights A numeric vector of column weights, of the same length as the
#'   number of columns in \code{df}.
#'
#' @return A numeric vector of weighted means, one for each row of \code{df}.
#'
#' @keywords internal
weighted_row_mean <- function(
    df, 
    weights) {
  
  # Matrix of weights replicated to match shape of df
  w_mat <- matrix(weights, nrow = nrow(df), ncol = ncol(df), byrow = TRUE)
  
  # Mask missing values in df and weights
  mask <- !is.na(df)
  w_masked <- w_mat * mask
  
  # Normalize weights per row (sum only over non-missing weights)
  w_norm <- w_masked / rowSums(w_masked)
  
  # Compute weighted row means with NA-safe multiplication
  rowSums(df * w_norm, na.rm = TRUE)
  
}