#' DELETE: Calculate Single Correlation-Weighted Metrics (Reliability & Validity)
#'
#' @description
#'
#' Create a composite score and the reliability and validity metrics of the
#' created construct by specifying the indicators that go into the composite
#' variable.
#'
#' See documentation for \code{?correlation_metrics} for calculation of
#' loadings, reliability, and validity.
#'
#' @usage calc_correlation_metrics( data, var, name, digits = 3 )
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A required string denoting the name of the composite variable.
#'
#' @return A list of dataframes consisting of:
#' \itemize{
#'  \item{\code{data}: }{The data with the composite variables appended as new
#'  variables.}
#'  \item{\code{metrics}: }{A matrix loadings and weights of the indicators and
#'  their corresponding composite variable.}
#'  \item{\code{validity}: }{A matrix of composite reliability and validity
#'  metrics.}
#' }
#'
#' @examples
#'
#' # Load in data
#' data(grit)
#'
#' # Specify a vector of indicators
#' extraversion <- sprintf("e%01d", seq(10))
#'
#' # Calculate correlation-weighted composite scores
#' correlation_metrics(data = grit,
#'                     var = extraversion,
#'                     name = "extraversion",
#'                     digits = 3)
#'
#' @export
calc_correlation_metrics <- function(data,
                                     var,
                                     name,
                                     digits = 3) {
  
  # Call pipe function from `magrittr`
  `%>%` <- magrittr::`%>%`
  
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  # Calculate correlation matrix
  cor_matrix <- stats::cor(df,
                           use = 'pairwise.complete.obs')
  
  # Remove correlation matrix diagonal
  diag(cor_matrix) <- NA
  
  
  
  # -- COMPOSITE WEIGHTS CALCULATION -- #
  
  # Get the correlation weights
  cor_weights <- colMeans(cor_matrix,
                          na.rm = T)
  
  # Normalize correlation weights
  cor_weights <- cor_weights / mean(cor_weights)
  
  
  
  # -- COMPOSITE CALCULATION -- #
  
  # Calculate unweighted correlation composite score
  composite_score <- rowMeans(sweep(df,
                                    2,
                                    cor_weights,
                                    "*"),
                              na.rm = T)
  
  
  
  # -- METRICS CALCULATION -- #
  
  # Get loadings as the correlation between indicator and composite
  df_loadings <- cbind(composite_score,
                       df)
  
  # Get loadings
  loadings <- cor(df_loadings,
                  use = "pairwise.complete.obs")[,1][-1]
  
  # Get weighted loadings squared
  loadings_squared <- (loadings^2)*cor_weights
  
  # Get weighted errors
  errors <- (1 - loadings^2)*cor_weights
  
  
  # Calculate Cronbach's alpha
  alpha <- ltm::cronbach.alpha(df, na.rm = T)$alpha
  
  # Calculate Average Variance Extracted (AVE)
  ave <- sum(loadings_squared) / (sum(loadings_squared) + sum(errors))
  
  # Calculate Composite Reliability
  rhoc <- sum(loadings*cor_weights)^2 / (sum(loadings*cor_weights)^2 + sum(errors))
  
  # Calculate Loadings Range
  min_loading <- min(loadings)
  max_loading <- max(loadings)
  
  rounding <- paste0("%.", digits, "f")
  
  loading_range <- paste0("[",
                          sprintf(rounding, min_loading),
                          ", ",
                          sprintf(rounding, max_loading),
                          "]")
  
  # Calculate Weights Range
  min_weight  <- min(cor_weights)
  max_weight <- max(cor_weights)
  
  weight_range <- paste0("[",
                         sprintf(rounding, min_weight),
                         ", ",
                         sprintf(rounding, max_weight),
                         "]")
  
  
  # Compile variable reliability and discriminant validity
  composite_validity <- data.frame(alpha = alpha,
                                   rhoc = rhoc,
                                   ave = ave,
                                   loading_range = loading_range,
                                   weight_range = weight_range) %>% 
    # Convert rownames to composite column
    tibble::rownames_to_column(var = "composite") %>% 
    # Create column to inform which composite the metrics are for
    dplyr::mutate(composite = name)
  
  # Compile weights and loadings
  composite_metrics <- data.frame(weights  = cor_weights,
                                  loadings = loadings) %>% 
    # Convert rownames to indicator column
    tibble::rownames_to_column(var = "indicator") %>% 
    # Create column to inform which composite the indicators reflect
    dplyr::mutate(composite = name) %>% 
    # Reorder
    dplyr::select(composite,
                  indicator,
                  loadings,
                  weights)
  
  # Compile AVE and Composite Reliability
  
  list(composite_score    = composite_score,
       composite_metrics  = composite_metrics,
       composite_validity = composite_validity)
  
}
