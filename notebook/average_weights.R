#' Calculate Unweighted Composite Score Weights
#' 
#' @description
#' Create composite score weights of scales by specifying the indicators that
#' go into each respective composite variable. For unweighted composite scores,
#' weights are calculated as `1/k` where `k` is the number of indicators for
#' the composite variable.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#' each column represents a variable and each row represents an observation.
#' @param varlist A required named list of vectors. Each name in the list
#' represents a composite variable, and the corresponding vector contains the
#' column names that are associated with the indicators comprising said
#' composite variable.
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
#' # Calculate unweighted average composite scores
#' average_weights <- average_weights(data = grit,
#'                                    varlist = varlist)
#' 
#' @export
average_weights <- function(data = .,
                            varlist){
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  
  # Create empty list to save composite weights
  composite_weights <- list()
  
  
  for(i in seq_along(varlist)){
    
    
    # -- COMPOSITE WEIGHTS CALCULATION -- #
    
    # Save weights
    composite_weights[[names(varlist)[i]]] <- data.frame(indicator = varlist[[i]],
                                                         weights = 1) %>% 
      
      # Create column to inform which composite the indicators reflect
      dplyr::mutate(composite = names(varlist)[i]) %>% 
      
      # Normalize weights
      dplyr::mutate(weights = weights / sum(weights)) %>% 
      
      # Reorder
      dplyr::select(composite, indicator, weights)

    
  }
  
  
  # Convert the weights list to a dataframe
  weights_table <- purrr::map_df(composite_weights, ~.x)
  
  
  # Return
  return(weights_table)
  
  
}
