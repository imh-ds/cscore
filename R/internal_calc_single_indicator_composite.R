#' Calculate Single Indicator Composite
#'
#' @description Return the indicator as its own composite.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column name.
#' @param name A required string denoting the name of the composite variable.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#'
#' @keywords internal
calc_single_indicator <- function(
    data,
    var,
    name,
    digits = 3,
    return_metrics = FALSE
) {
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  if(isFALSE(return_metrics)) {
    
    data[[var]]
    
  } else {
    
    rounding <- paste0("%.", digits, "f")
    
    range <- paste0(
      "[",
      sprintf(rounding, 1),
      ", ",
      sprintf(rounding, 1),
      "]"
    )
    
    # Create composite score
    composite_score <- data[[var]]
    
    # Compile variable reliability and discriminant validity
    composite_validity <- data.frame(
      composite = name,
      alpha = 1,
      rhoc = 1,
      ave = 1,
      loading_range = range,
      weight_range = range
    )
    
    # Compile weights and loadings
    composite_metrics <- data.frame(
      composite = name,
      indicator = var,
      weights  = 1,
      loadings = 1
    )
    
    # Compile AVE and Composite Reliability
    list(composite_score    = composite_score,
         composite_metrics  = composite_metrics,
         composite_validity = composite_validity)
    
  }
  
}
