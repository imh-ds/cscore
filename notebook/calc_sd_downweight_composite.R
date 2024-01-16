#' DELETE - Calculate Standard Deviation Downweighted Composite Score
#' 
#' @description
#' Create a composite score by specifying the indicators that go into
#' each respective composite variable. Standard Deviation downweighted 
#' composite scores are calculated by using the indicator's standard deviation 
#' as the weights. The indicators are multiplied by the inverse of their 
#' standard deviations normalized across all indicators. Standard deviations 
#' are therefore downweighted, i.e., indicators with larger standard deviations 
#' are weighted lower than indicators with smaller standard deviations.
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
  numeric_vars <- data[, var]
  
  # Calculate the weights based on the original standard deviations
  sd_weights <- 1 / apply(numeric_vars, 2, function(x) sd(x, na.rm = TRUE))
  
  # Normalize weights
  sd_weights <- sd_weights / mean(sd_weights)
  
  # Calculate the composite score
  rowMeans(numeric_vars * sd_weights,
           na.rm = T)
  
  
}




