#' Calculate Composite Score Metrics (Weights & Loadings)
#' 
#' @description
#' Create composite score weights and loadings of scales by specifying the
#' indicators that go into each respective composite variable. Refer to the
#' help documents for `?regression_loadings`, `?correlation_loadings`, and
#' `?average_loadings` to see how the loadings are calculated for each
#' weighting scheme. Refer to the help documents for `?regression_weights`,
#' `?correlation_weights`, and `?average_weights` to see how the weights are
#' calculated for each weighting scheme.
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
#' @param metric The metric (i.e., weights and loadings) to be returned. For
#' just indicator weights on the composite score, specify as `"weights"`. For
#' just indicator loadings on the composite score, specify as `"loadings"`.
#' The default is to return both using `c("weights", "loadings")`.
#' 
#' @return A dataframe table with the indicator metrics for their respective
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
#' correlation_metrics <- composite_metrics(data = grit,
#'                                          varlist = varlist,
#'                                          weight = "correlation",
#'                                          metric = c("weights", "loadings"))
#' 
#' # Calculate regression-weighted composite scores
#' regression_metrics <- composite_metrics(data = grit,
#'                                         varlist = varlist,
#'                                         weight = "regression",
#'                                         metric = c("weights", "loadings"))
#'                                    
#' # Calculate unweighted average composite scores
#' average_metrics <- composite_metrics(data = grit,
#'                                      varlist = varlist,
#'                                      weight = "average",
#'                                      metric = c("weights", "loadings"))
#' 
#' @export
composite_metrics <- function(data = .,
                              varlist,
                              weight = "correlation",
                              metric = c("weights", "loadings")){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  
  # ------------------------------------------------ #
  # -- If unweighted, get average composite score -- #
  # ------------------------------------------------ #
  
  if(weight == "average"){
    
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
    
  }
  
  
  
  
  # -------------------------------------------------- #
  # -- If correlation weights, save composite score -- #
  # -------------------------------------------------- #
  
  if(weight == "correlation"){
    
    # If `metric` is specified to both `weights` and `loadings` (the default),
    # return both weights and loadings in a single dataframe
    if(all(metric == c("weights", "loadings"))){
      
      # Convert the weights list to a dataframe
      weights_table <- correlation_weights(data,
                                           varlist)
      
      # Convert the loadings list to a dataframe
      loadings_table <- correlation_loadings(data,
                                             varlist)
      
      # Merge loadings and weights together
      metrics_table <- dplyr::inner_join(weights_table,
                                         loadings_table,
                                         by = c("composite", "indicator"))
      
    }
    
    
    # If metric is specified to just `weights`, return only correlation weights
    if(all(metric == "weights")){
      
      metrics_table <- correlation_weights(data,
                                           varlist)
      
    }
    
    
    # If metric is specified to just `loadings`, return only correlation loadings
    if(all(metric == "loadings")){
      
      metrics_table <- correlation_loadings(data,
                                            varlist)
      
    }
    
  }
  
  
  
  
  # ----------------------------------------------------------------------- #
  # -- If regression weights, run linear model to get regression weights -- #
  # ----------------------------------------------------------------------- #
  
  if(weight == "regression"){
    
    # If `metric` is specified to both `weights` and `loadings` (the default),
    # return both weights and loadings in a single dataframe
    if(all(metric == c("weights", "loadings"))){
      
      # Convert the weights list to a dataframe
      weights_table <- regression_weights(data,
                                          varlist)
      
      # Convert the loadings list to a dataframe
      loadings_table <- regression_loadings(data,
                                            varlist)
      
      # Merge loadings and weights together
      metrics_table <- dplyr::inner_join(weights_table,
                                         loadings_table,
                                         by = c("composite", "indicator"))
      
    }
    
    
    # If metric is specified to just `weights`, return only correlation weights
    if(all(metric == "weights")){
      
      metrics_table <- regression_weights(data,
                                          varlist)
      
    }
    
    
    # If metric is specified to just `loadings`, return only correlation loadings
    if(all(metric == "loadings")){
      
      metrics_table <- regression_loadings(data,
                                           varlist)
      
    }
    
  }
  
  
  # Return
  return(metrics_table)
  
}
