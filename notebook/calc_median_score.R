#' Calculate Median Composite Score
#' 
#' @description
#' 
#' Create a composite score by specifying the indicators that go into
#' its respective composite variable. Median composite scores are
#' calculated as the median of the indicators.
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
#' # Specify the name of the construct
#' name <- "extraversion"
#' 
#' # Calculate correlation-weighted composite score
#' calc_median_score(data = grit, var = extraversion)
#' 
#' @export
calc_median_score <- function(data, var){
  
  
  # Subset the dataframe to include only the specified variables
  df <- data[, var]
  
  # Calculate the median of each row
  apply(df, 1, median, na.rm = TRUE)
  
  
}
