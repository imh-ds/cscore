#' Calculate Median Composite Scores
#'
#' @description Calculate the composite score for the median family of weighting
#'   schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include \code{c("median",
#'   "median_decay", "median_gauss")}
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A required string denoting the name of the composite variable.
#' @param decay_rate A numeric value reflecting the decay rate (i.e.,
#'   sensitivity) of the distance-to-median weighting schema. The default value
#'   is set to 0.5. This argument is only relevant if \code{weight =
#'   "median_decay"}.
#' @param sigma A numeric value reflecting the sigma value for the Gaussian
#'   function in the distance-to-median weighting schema. The default value is
#'   set to 0.5. This argument is only relevant if \code{weight =
#'   "median_gauss"}.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#'
#' @returns If \code{return_metrics = FALSE}, an array of the composite score is
#'   returned. If \code{return_metrics = TRUE}, a list is returned consisting
#'   of:
#' \itemize{
#'  \item \code{composite_score}: An array with the calculated composite
#'  variable.
#'  \item \code{composite_metrics}: A matrix loadings and weights
#'  of the indicators.
#'  \item \code{composite_validity}: A matrix of composite reliability and
#'  validity metrics.
#' }
#'
#' @keywords internal
calc_median_composite <- function(
    data,
    var,
    weight,
    digits = 3,
    name = NULL,
    decay_rate = 0.5,
    sigma = 0.5,
    return_metrics
) {
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  # -- INPUT VALIDATION -- #

  # All indicators must be numeric
  non_numeric <- vapply(df, Negate(is.numeric), logical(1))
  if (any(non_numeric)) {
    stop(
      "Non-numeric indicator(s) detected: ",
      paste(names(df)[non_numeric], collapse = ", "),
      ". All indicators must be numeric for median-family composite scoring.",
      call. = FALSE
    )
  }
  
  # -- CONDITIONAL COMPOSITE CALCULATION -- #
  
  # IF UNWEIGHTED ----
  if(weight == "median"){
    
    # Calculate the median of each row
    composite_score <- apply(df, 
                             1, 
                             median, 
                             na.rm = TRUE)
    
  }
  
  
  
  # IF DISTANCE-WEIGHTED ----
  if(weight %in% c("median_decay", "median_gauss")){

    # Calculate the median of each row
    medians <- apply(df,
                     1,
                     stats::median,
                     na.rm = TRUE)

    # Raw distance of each item from the respondent's median
    distances <- sweep(df,
                       1,
                       medians,
                       FUN = "-")

    # Scale distances by each respondent's observed item range so that
    # decay_rate and sigma are scale-invariant (a value of 0.5 means the
    # same thing on a 1-5 Likert scale as on a 1-100 scale). The range is
    # computed from non-missing items; rows with a single valid item (range 0)
    # are protected from division by zero.
    row_ranges <- apply(df, 1, function(x) {
      x_obs <- x[!is.na(x)]
      if (length(x_obs) <= 1) return(1)
      r <- diff(range(x_obs))
      if (r == 0) 1 else r
    })
    distances_scaled <- distances / row_ranges

    if(weight == "median_decay"){

      distance_weights <- exp(-decay_rate * abs(distances_scaled))

    }

    if(weight == "median_gauss"){

      distance_weights <- exp(-(distances_scaled^2) / (2 * sigma^2))

    }

    # Properly normalize weights with NA-safe row-wise renormalization:
    # zero out weights for missing items so they are excluded from both the
    # numerator and the denominator, matching the behavior of weighted_row_mean.
    mask <- !is.na(as.matrix(df))
    w_mat <- as.matrix(distance_weights)
    w_mat[!mask] <- 0
    row_weight_sums <- rowSums(w_mat)
    row_weight_sums[row_weight_sums == 0] <- NA_real_
    w_norm <- w_mat / row_weight_sums

    df_mat <- as.matrix(df)
    df_mat[!mask] <- 0
    composite_score <- rowSums(df_mat * w_norm, na.rm = FALSE)
    composite_score[rowSums(mask) == 0] <- NA_real_

    # Retain the weight matrix for metrics reporting
    weights <- w_norm

  }
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    return(composite_score)
    
  }
  
  
  # -- IF CALCULATING COMPOSITE, RELIABILITY, & VALIDITY -- #
  if(return_metrics == TRUE){
    
    # If median composite scoring, there are no weights, so assign 1
    if(weight == "median"){
      
      mweights <- rep(1, ncol(df))
      
    }
    
    # If distance-to-median composite scoring, summarise the respondent-level
    # weight matrix to sample-wide column means for metrics reporting.
    if(weight %in% c("median_decay", "median_gauss")){

      mweights <- colMeans(weights, na.rm = TRUE)
      mweights <- safe_normalize(mweights)

    }
    
    metrics <- calc_metrics(df = df,
                            composite_score = composite_score,
                            weights = mweights,
                            digits = digits,
                            name = name)
    
    return(metrics)
    
  }
  
}
