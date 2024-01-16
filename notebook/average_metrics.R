#' Calculate Unweighted Composite Score Metrics (Weights & Loadings)
#' 
#' @description
#' Create composite score weights and loadings of scales by specifying the
#' indicators that go into each respective composite variable. Refer to the
#' help documents `?average_loadings` and `?average_weights` to see how the 
#' loadings and weights are calculated.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
#' @param metric The metric (i.e., weights and loadings) to be returned. For
#' just indicator weights on the composite score, specify as `"weights"`. For
#' just indicator loadings on the composite score, specify as `"loadings"`.
#' The default is to return both using `c("weights", "loadings")`.
#' 
#' @return A dataframe table with the indicator weights and/or loadings for
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
#' average_metrics <- average_metrics(data = grit,
#'                                    varlist = varlist,
#'                                    metric = c("weights", "loadings"))
#' 
#' @export
average_metrics <- function(data = .,
                            varlist,
                            metric = c("weights", "loadings")){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  # If `metric` is specified to both `weights` and `loadings` (the default),
  # return both weights and loadings in a single dataframe
  if(all(metric == c("weights", "loadings"))){
    
    # Convert the weights list to a dataframe
    weights_table <- average_weights(data,
                                     varlist)
    
    # Convert the loadings list to a dataframe
    loadings_table <- average_loadings(data,
                                       varlist)
    
    # Merge loadings and weights together
    metrics_table <- dplyr::inner_join(weights_table,
                                       loadings_table,
                                       by = c("composite", "indicator"))
    
  }
  
  
  # If metric is specified to just `weights`, return only correlation weights
  if(all(metric == "weights")){
    
    metrics_table <- average_weights(data,
                                     varlist)
    
  }
  
  
  # If metric is specified to just `loadings`, return only correlation loadings
  if(all(metric == "loadings")){
    
    metrics_table <- average_loadings(data,
                                      varlist)
    
  }
  
  
  # Return
  return(metrics_table)
  
}
