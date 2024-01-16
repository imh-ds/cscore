#' DELETE - Calculate Average Unweighted Composite Score
#'
#' @description Create a composite score by specifying the indicators that go
#' into its respective composite variable. Unweighted composite scores are
#' calculated by taking the straight average of the indicators.
#'
#' See help documentation of \code{?average_score} for the calculation of the
#' average score composite.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of column names associated with the indicators
#'   comprising a composite variable.
#'
#' @return An array of the calculated composite score.
#'
#' @examples
#'
#' data(grit)
#'
#' # Specify a vector of indicators
#' extraversion <- sprintf("e%01d", seq(10))
#'
#' # Calculate correlation-weighted composite score
#' calc_average_composite(data = grit, var = extraversion)
#'
#' @export
calc_average_composite <- function(data,
                                   var,
                                   name = NULL,
                                   digits = 3,
                                   return_metrics = FALSE) {
  
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  
  
  # -- COMPOSITE WEIGHTS CALCULATION -- #
  
  # Set equal weights
  weights <- rep(1, ncol(df))
  
  
  
  # -- COMPOSITE CALCULATION -- #
  
  # Calculate unweighted composite score
  composite_score <- rowMeans(sweep(df,
                                    2,
                                    weights,
                                    "*"),
                              na.rm = T)
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    return(composite_score)
    
  }
  
  
  # -- IF CALCULATING COMPOSITE, RELIABILITY, & VALIDITY -- #
  if(return_metrics == TRUE){
    
    metrics <- calc_metrics(df = df,
                            composite_score = composite_score,
                            weights = weights,
                            digits = digits,
                            name = name)
    
    return(metrics)
    
  }
  
  
}
