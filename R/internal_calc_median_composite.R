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
    
    # Calculate the distance of each variable from the median
    distances <- sweep(df, 
                       1, 
                       medians, 
                       FUN = "-")
    
    if(weight == "median_decay"){
      
      # Calculate the weights based on the distance from the median using exponential decay function
      distance_weights <- exp(-decay_rate * abs(distances))
      
    }
    
    if(weight == "median_gauss"){
      
      # Calculate the weights based on the distance from the median using gaussian function
      distance_weights <- exp(-(distances^2) / (2 * sigma^2))
      
    }

    
    # Normalize weights
    weights <- distance_weights / rowMeans(distance_weights,
                                           na.rm = T)
    
    # Calculate the weighted composite variable
    composite_score <- rowMeans(df * weights,
                                na.rm = TRUE)

    
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
    
    # If distance-to-median composite scoring, get sample-wide weights since 
    # weights are respondent-level unique
    if(weight %in% c("median_decay", "median_gauss")){
      
      mweights <- colMeans(weights, na.rm = T)
      mweights <- mweights / mean(mweights)
    
    }
    
    metrics <- calc_metrics(df = df,
                            composite_score = composite_score,
                            weights = mweights,
                            digits = digits,
                            name = name)
    
    return(metrics)
    
  }
  
}
