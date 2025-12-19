#' Calculate Mutual-Information Composite Scores
#'
#' @description Calculate the composite score for the mutual information family
#'   of weighting schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param entropy A string value reflecting the mutual information entropy
#'   estimator from the \code{infotheo} package. Four estimators are available:
#'   \code{emp} to compute the entropy of the empirical probability
#'   distribution. Empirical entropy is suitable for simple calculations without
#'   corrections. \code{mm} applies an asymptotic bias-corrected estimator
#'   making it suitable for small sample sizes. \code{shrink} applies a
#'   shrinkage estimate of the Dirichlet probability distribution to provide a
#'   stable estimate useful for small sample sizes or sparse data. \code{sg}
#'   applies a Schurmann-Grassberger estimate of the Dirichlet probability
#'   distribution to serve as an alternative to the Shrinkage approach.
#' @param nmi_method A string value reflecting the method used for calculating
#'   Normalized Mutual Information (NMI) values. \code{"average"} will normalize
#'   MI values using the average entropies of variables A and B.
#'   \code{"geometric"} will normalize MI values using the geometric mean of
#'   entropies of variables A and B.
#' @param threshold An integer specifying the maximum number of unique values to
#'   consider the input as discrete. Defaults to 10.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A required string denoting the name of the composite variable.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#'
#' @returns If \code{return_metrics = FALSE}, an array of the composite score is
#'   returned. If \code{return_metrics = TRUE}, a list is returned consisting
#'   of:
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
calc_mi_composite <- function(
  
  data,
  var,
  entropy = c("emp", "mm", "shrink", "sg"),
  nmi_method = c("geometric", "average"),
  threshold = 10,
  digits = 3,
  name = NULL,
  return_metrics
  
) {
  
  # -- MATCH ARGUMENTS -- #
  
  nmi_method <- match.arg(nmi_method)
  entropy <- match.arg(entropy)
  
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  # Discretize variables where possible
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ coerce_to_discrete_if_integer_like(.x, threshold = threshold)
      )
    )
  
  # Check that all variables are discrete
  discrete_flags <- unlist(
    lapply(
      names(df),
      function(x) is_discrete_variable(df[[x]])
    )
  )
  
  # If any of the variables comes up continuous, discretize the dataframe
  if (any(!discrete_flags)) {
    df <- infotheo::discretize(df)
  }
  
  
  # -- COMPOSITE CALCULATION -- #
  
  # Initialize an empty matrix
  mi_matrix <- matrix(0, 
                      ncol(df),
                      ncol(df))
  
  # Get pairwise mutual information matrix
  for (i in 1:ncol(df)) {
    
    for (j in i:ncol(df)) {
      
      # Calculate MI
      mi_value <- infotheo::mutinformation(
        df[, i],
        df[, j]
      ) / sqrt(infotheo::entropy(df[, i], 
                                 method = entropy) * infotheo::entropy(df[, j], 
                                                                       method = entropy))
      
      # Calculate entropy of var i and j
      ent_i <- infotheo::entropy(df[, i],
                                 method = entropy)
      ent_j <- infotheo::entropy(df[, i],
                                 method = entropy)
      
      # Calculate Normalized MI (NMI)
      nmi <- if (nmi_method == "geometric") {
        
        # If NMI option is set to calculate using geometric mean entropy:
        mi_value / sqrt(ent_i) * sqrt(ent_j)
        
      } else {
        
        # If NMI option is set to calculate using average entropy:
        2 * mi_value / (ent_i + ent_j)
        
      }
      
      # Store in matrix
      mi_matrix[j, i] <- nmi
      mi_matrix[i, j] <- mi_matrix[j, i]
      
    }
    
  }
  
  # Set diagonal to NA
  diag(mi_matrix) <- NA
  
  # Get the information weights
  weights <- colMeans(mi_matrix,
                      na.rm = T)
  
  # Normalize information weights
  weights <- weights / mean(weights)
  
  # Calculate unweighted composite score
  composite_score <- weighted_row_mean(df, weights)
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    return(composite_score)
    
  }
  
  
  # -- IF CALCULATING COMPOSITE, RELIABILITY, & VALIDITY -- #
  if(return_metrics == TRUE){
    
    metrics <- calc_metrics(df = df,
                            composite_score = composite_score,
                            weights = weights,
                            digits = digits,
                            name = name)
    
    return(metrics)
    
  }
  
  
}
