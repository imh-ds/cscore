#' DELETE - Calculate Standard Deviation Upweighted Composite Score
#' 
#' @description
#' Create a composite score by specifying the indicators that go into
#' each respective composite variable. Standard Deviation upweighted composite
#' scores are calculated by using the indicator's standard deviation as the
#' weights. The indicators are multiplied by their standard deviations
#' normalized across all indicators. Standard deviations are therefore 
#' upweighted, i.e., indicators with larger standard deviations are weighted
#' higher than indicators with smaller standard deviations.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param var A required vector of column names associated with the indicators
#' comprising a composite variable.
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
#' calc_sd_upweight_composite(data = grit, var = extraversion)
#' 
#' @export
calc_sd_upweight_composite <- function(data,
                                       var) {
  
  
  # Select the numeric variables
  df <- data[, var]
  
  # Calculate the weights based on the original standard deviations
  sd_weights <- apply(df,
                      2, 
                      function(x) sd(x, na.rm = TRUE))
  
  # Normalize weights
  sd_weights <- sd_weights / mean(sd_weights)
  
  # Calculate SD composite score
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




