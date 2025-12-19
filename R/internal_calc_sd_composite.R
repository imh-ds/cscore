#' Calculate Standard Deviation Composite Scores
#'
#' @description Calculate the composite score for the standard deviation family
#'   of weighting schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include \code{c("upweight",
#'   "downweight")}
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A required string denoting the name of the composite variable.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#'
#' @return If \code{return_metrics = FALSE}, an array of the composite score is
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
calc_sd_composite <- function(
    data,
    var,
    weight,
    digits = 3,
    name = NULL,
    return_metrics
) {
  
  # -- DATA PREPARATION -- #
  
  # Select the numeric variables
  df <- data[, var]

  
  # -- CONDITIONAL COMPOSITE CALCULATION -- #
  
  # IF SD IS UPWEIGHTED ----
  if(weight == "sd_upweight") {
    
    sd_weights <- apply(df,
                        2, 
                        function(x) stats::sd(x, na.rm = TRUE))
    
  }

  # IF SD IS DOWNWEIGHTED ----
  if(weight == "sd_downweight") {
    
    sd_weights <- 1 / apply(df, 
                            2, 
                            function(x) stats::sd(x, na.rm = TRUE))
    
  }
  
  # Normalize weights
  weights <- sd_weights / mean(sd_weights)
  
  # Calculate SD composite score
  composite_score <- weighted_row_mean(df, weights)
  
  
  
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
