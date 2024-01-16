#' DELETE - Calculate Regression-Weighted Composite Score
#' 
#' @description 
#' 
#' Create a composite score by specifying the indicators that go into each
#' respective composite variable. Regression-weighted composite scores are
#' calculated by fitting a linear model where the correlation-weighted composite
#' scores are regressed on the indicators. The indicators are multiplied by
#' their respective regression weights to calculate the predicted values in the
#' linear model, which represent the regression-weighted composite scores.
#' 
#' See documentation for \code{?regression_score} for information on the
#' calculation of correlation weights, loadings, reliability, and validity.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of column names associated with the indicators
#'   comprising a composite variable.
#' @param name The name of composite variable. This argument is only relevant if
#'   \code{return_metrics = TRUE}.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' 
#' @return If \code{return_metrics = FALSE}, a dataframe identical to the input
#'   dataframe, with additional columns appended at the end, is returned. These
#'   new columns represent the calculated composite scores. If
#'   \code{return_metrics = TRUE}, a list containing the following dataframes is
#'   returned:
#'  \itemize{
#'  \item{\strong{Data}: }{A dataframe with the composite variables appended as
#'  new variables.}
#'  \item{\strong{Metrics}: }{A matrix of indicator loadings and weights 
#'  metrics.}
#'  \item{\strong{Validity}: }{A matrix of composite reliability and validity
#'  metrics.}
#' }
#' 
#' @examples
#' 
#' data(grit)
#'
#' # Specify a vector of indicators
#' extraversion <- sprintf("e%01d", seq(10))
#'
#' # Calculate correlation-weighted composite score
#' calc_regression_composite(data = grit,
#'                           var = extraversion)
#' 
#' # Calculate correlation-weighted composite score and metrics
#' calc_regression_composite(data = grit,
#'                           var = extraversion,
#'                           name = "extraversion",
#'                           digits = 3,
#'                           return_metrics = TRUE)
#' 
#' @export
calc_regression_composite <- function(data,
                                      var,
                                      name,
                                      digits = 3,
                                      return_metrics = FALSE) {
  
  
  # -- CALCULATE WEIGHTS & COMPOSITE -- #
  calc <- calc_weights(data = data,
                       var = var,
                       weight = "regression")
  
  # Get dataframe
  df <- calc[["df"]]
  
  # Get composite score
  composite_score <- calc[["composite_score"]]
  
  # Get weights
  weights <- calc[["weights"]]
  
  
  
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
