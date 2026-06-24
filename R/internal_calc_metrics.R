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
  
  # Corrected item-total correlations: each item is correlated with the
  # weighted composite computed from all *other* items, avoiding part-whole
  # inflation that inflates loadings, AVE, and reliability when items are
  # part of the composite they are correlated against.
  loadings <- sapply(seq_len(ncol(df)), function(j) {
    rest_idx <- setdiff(seq_len(ncol(df)), j)
    composite_without_j <- weighted_row_mean(
      df[, rest_idx, drop = FALSE],
      weights[rest_idx]
    )
    stats::cor(df[, j], composite_without_j, use = "pairwise.complete.obs")
  })
  names(loadings) <- colnames(df)
    
    # Get weighted loadings squared
    loadings_squared <- (loadings^2)*weights
    
    # Get weighted errors for AVE (weighted linearly)
    errors_ave <- (1 - loadings^2)*weights
    
    # Get weighted errors for Reliability (weighted by squared weights)
    errors_reliability <- (1 - loadings^2)*(weights^2)
    
    
    # Cronbach's alpha is included for conventional reporting compatibility.
    # It assumes tau-equivalence (equal loadings) and is most meaningful for
    # unweighted ("average") composites. For weighted composites, rhoc (weighted
    # McDonald's omega) better reflects the scoring model. Researchers using
    # non-average weighting schemes are discouraged from reporting alpha as a
    # primary reliability index.
    alpha <- ltm::cronbach.alpha(df, na.rm = T)$alpha

    # Average Variance Extracted (AVE) — a measure of CONVERGENT validity.
    # AVE >= 0.5 indicates that, on average, the composite explains more
    # variance in its indicators than is attributable to measurement error.
    # Note: AVE alone does not establish DISCRIMINANT validity. Discriminant
    # validity requires cross-construct comparison, e.g., the Fornell-Larcker
    # criterion (AVE > squared inter-construct correlations) or HTMT < 0.85.
    ave <- sum(loadings_squared) / (sum(loadings_squared) + sum(errors_ave))
    
    # Calculate Composite Reliability using squared weights for error variance
    rhoc <- sum(loadings*weights)^2 / (sum(loadings*weights)^2 + sum(errors_reliability))
    
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
    
    
    # Compile variable reliability and convergent validity
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
      