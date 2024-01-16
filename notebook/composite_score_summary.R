#' Calculate Composite Score Data & Metrics
#' 
#' @description
#' Create composite scores of scales by specifying the indicators that go into
#' each respective composite variable. The function will also return the
#' metrics (i.e., weights & loadings) for the composite variables.
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
#' @return A list of 1. a dataframe identical to the input dataframe, with
#' additional columns appended at the end representing the calculated composite
#' scores and 2. a dataframe table with the indicator weights and loadings for
#' their respective composite.
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
#' correlation_summary <- composite_score_summary(data = grit,
#'                                                varlist = varlist,
#'                                                weight = "correlation")
#' 
#' # Calculate regression-weighted composite scores
#' regression_summary <- composite_score_summary(data = grit,
#'                                               varlist = varlist,
#'                                               weight = "regression")
#'                                    
#' # Calculate unweighted average composite scores
#' average_summary <- composite_score_summary(data = grit,
#'                                            varlist = varlist,
#'                                            weight = "average")
#' 
#' @export
composite_score_summary <- function(data = .,
                                    varlist,
                                    weight = "correlation"){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  
  # ------------------------------------------------ #
  # -- If unweighted, get average composite score -- #
  # ------------------------------------------------ #
  
  if(weight == "average"){
    
    return_results <- average_score_summary(data,
                                            varlist)
    
  }
  
  
  
  
  # -------------------------------------------------- #
  # -- If correlation weights, save composite score -- #
  # -------------------------------------------------- #
  
  if(weight == "correlation"){
    
    return_results <- correlation_score_summary(data,
                                                varlist)
    
  }
  
  
  
  
  # ----------------------------------------------------------------------- #
  # -- If regression weights, run linear model to get regression weights -- #
  # ----------------------------------------------------------------------- #
  
  if(weight == "regression"){
    
    return_results <- regression_score_summary(data,
                                               varlist)
    
  }
  
  
  # Return
  return(return_results)
  
}
