#' Normalize Weights with Protection Against Near-Zero Denominators (Internal)
#'
#' Divides each element of \code{w} by \code{mean(abs(w))}, so the normalized
#' weights have mean absolute value equal to 1. Falls back to equal weights
#' when all weights are effectively zero, preventing division by zero or
#' explosion when weights have mixed signs and a near-zero mean.
#'
#' @param w A numeric vector of weights.
#' @return A numeric vector of the same length with \code{mean(abs(w)) == 1}.
#' @keywords internal
safe_normalize <- function(w) {
  denom <- mean(abs(w), na.rm = TRUE)
  if (is.na(denom) || denom < .Machine$double.eps) {
    return(rep(1, length(w)))
  }
  w / denom
}


#' Compute Weighted Row Means with Missing Data Handling (Internal)
#'
#' Computes a weighted mean for each row of a data frame or matrix, allowing for
#' missing values. Weights are normalized within each row using the sum of their
#' absolute values, ensuring numerical stability when weights have mixed signs
#' (e.g., from negative correlation weights caused by reverse-keyed items).
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

  # Normalize per row using sum of absolute weights to prevent near-zero
  # division when weights have mixed signs. For all-positive weights (the
  # typical case) this is identical to the original rowSums(w_masked) behavior.
  row_abs_sums <- rowSums(abs(w_masked))
  row_abs_sums[row_abs_sums < .Machine$double.eps] <- NA_real_
  w_norm <- w_masked / row_abs_sums

  # Compute weighted row sums (weights already normalize to sum-of-abs = 1)
  out <- rowSums(as.matrix(df) * w_norm, na.rm = TRUE)

  # Where all df values are NA, replace with NA
  out[rowSums(mask) == 0] <- NA_real_

  out

}
