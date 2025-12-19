#' Calculate Composite Score Metrics
#'
#' @description Calculate the indicator and composite-level reliability and
#'   validity metrics.
#'
#' @param df A dataframe object. This should be a structured dataset where each
#'   column represents a variable and each row represents an observation.
#' @param composite_score An array representing the composite score.
#' @param weights A vector of weights for the indicator variables.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A string representing the name of the composite score.
#'
#' @returns A list of dataframes consisting of:
#' \itemize{
#'  \item \code{composite_score}: An array with the calculated composite
#'  variable.
#'  \item \code{composite_metrics}: A matrix loadings and weights
#'  of the indicators.
#'  \item \code{composite_validity}: A matrix of composite reliability and
#'  validity metrics.
#' }
#'
#' @keywords internal
calc_metrics <- function(
  df,
  composite_score,
  weights,
  digits,
  name
){
  
  # Get loadings as the correlation between indicator and composite
  df_loadings <- cbind(
    composite_score,
    df
  )
  
  # Get loadings
  loadings <- stats::cor(
    df_loadings,
    use = "pairwise.complete.obs")[,1][-1]
    
    # Get weighted loadings squared
    loadings_squared <- (loadings^2)*weights
    
    # Get weighted errors
    errors <- (1 - loadings^2)*weights
    
    
    # Calculate Cronbach's alpha
    alpha <- ltm::cronbach.alpha(df, na.rm = T)$alpha
    
    # Calculate Average Variance Extracted (AVE)
    ave <- sum(loadings_squared) / (sum(loadings_squared) + sum(errors))
    
    # Calculate Composite Reliability
    rhoc <- sum(loadings*weights)^2 / (sum(loadings*weights)^2 + sum(errors))
    
    # Calculate Loadings Range
    min_loading <- min(loadings)
    max_loading <- max(loadings)
    
    rounding <- paste0("%.", digits, "f")
    
    loading_range <- paste0(
      "[",
      sprintf(rounding, min_loading),
      ", ",
      sprintf(rounding, max_loading),
      "]"
    )
    
    # Calculate Weights Range
    min_weight  <- min(weights)
    max_weight  <- max(weights)
    
    weight_range <- paste0(
      "[",
      sprintf(rounding, min_weight),
      ", ",
      sprintf(rounding, max_weight),
      "]"
    )
    
    
    # Compile variable reliability and discriminant validity
    composite_validity <- data.frame(
      alpha = alpha,
      rhoc = rhoc,
      ave = ave,
      loading_range = loading_range,
      weight_range = weight_range
    ) %>% 
    # Convert rownames to composite column
    tibble::rownames_to_column(var = "composite") %>% 
    # Create column to inform which composite the metrics are for
    dplyr::mutate(composite = name)
    
    # Compile weights and loadings
    composite_metrics <- data.frame(
      weights  = weights,
      loadings = loadings
    ) %>% 
    # Convert rownames to indicator column
    tibble::rownames_to_column(var = "indicator") %>% 
    # Create column to inform which composite the indicators reflect
    dplyr::mutate(composite = name) %>% 
    # Reorder
    dplyr::select(composite,
      indicator,
      loadings,
      weights
    )
      
    # Compile AVE and Composite Reliability
    list(
      composite_score    = composite_score,
      composite_metrics  = composite_metrics,
      composite_validity = composite_validity
    )
        
      
}
      