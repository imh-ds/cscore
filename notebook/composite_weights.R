#' Calculate Composite Score Weights
#' 
#' @description
#' Create composite score weights of scales by specifying the indicators that
#' go into each respective composite variable. Refer to the help documents for
#' `?regression_weights`, `?correlation_weights`, and `?average_weights` to 
#' see how the weights are calculated for each weighting scheme.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' @param weight The weight type to be used in calculating the composite score. 
#' For composite scores emulating reflective measurements, specify as
#' `"correlation"` to calculate the composite score with correlation weights.
#' For composite scores emulating formative measurements, specify as
#' `"regression"` to calculate the composite score with regression weights.
#' For composite scores that are unweighted, specify as `"average"` to
#' calculate the composite scores with averages. The default is set to
#' correlation weights.
#' 
#' @return A dataframe table with the indicator weights for their respective
#' composite.
#' 
#' @examples
#' 
#' data(grit)
#' 
#' # Specify the named list with composite names and their respective indicators
#' varlist <- list(extraversion = sprintf("e%01d", seq(10)),
#'                 neuroticism = sprintf("n%01d", seq(10)),
#'                 agreeableness = sprintf("a%01d", seq(10)),
#'                 conscientiousness = sprintf("c%01d", seq(10)),
#'                 openness = sprintf("o%01d", seq(10)),
#'                 grit = sprintf("gs%01d", seq(12)))
#' 
#' # Calculate correlation-weighted composite scores
#' correlation_weights <- composite_weights(data = grit,
#'                                          varlist = varlist,
#'                                          weight = "correlation")
#' 
#' # Calculate regression-weighted composite scores
#' regression_weights <- composite_weights(data = grit,
#'                                         varlist = varlist,
#'                                         weight = "regression")
#'                                    
#' # Calculate unweighted average composite scores
#' average_weights <- composite_weights(data = grit,
#'                                      varlist = varlist,
#'                                      weight = "average")
#' 
#' @export
composite_weights <- function(data = .,
                              varlist,
                              weight = "correlation"){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  
  # ------------------------------------------------ #
  # -- If unweighted, get average composite score -- #
  # ------------------------------------------------ #
  
  if(weight == "average"){
    
    return_results <- average_weights(data,
                                      varlist)
    
  }
  
  
  
  
  # -------------------------------------------------- #
  # -- If correlation weights, save composite score -- #
  # -------------------------------------------------- #
  
  if(weight == "correlation"){
    
    return_results <- correlation_weights(data,
                                          varlist)
    
  }
  
  
  
  
  # ----------------------------------------------------------------------- #
  # -- If regression weights, run linear model to get regression weights -- #
  # ----------------------------------------------------------------------- #
  
  if(weight == "regression"){
    
    return_results <- regression_weights(data,
                                         varlist)
    
  }
  
  
  # Return
  return(return_results)
  
}
