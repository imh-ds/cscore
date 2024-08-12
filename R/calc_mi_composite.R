#' Calculate Mutual-Information Composite Scores
#' 
#' @description
#' 
#' Calculate the composite score for the mutual information family of weighting schemas.
#' For information on the specifics calculations, refer to the help
#' documentations of \code{?average_score} (for unweighted),
#' \code{?correlation_score} (for correlation-weighted), and
#' \code{?regression_score} (for regression-weighted).
#'
#' Refer to help documentation \code{?calc_metrics} for information on how
#' reliability and validity metrics are calculated.
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
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A required string denoting the name of the composite variable.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#'   
#' @return If \code{return_metrics = FALSE}, an array of the composite score is
#'   returned. If \code{return_metrics = TRUE}, a list is returned consisting
#'   of:
#' \itemize{
#'  \item{\code{composite_score}: }{An array with the calculated composite
#'  variable.} \item{\code{composite_metrics}: }{A matrix loadings and weights
#'  of the indicators.}
#'  \item{\code{composite_validity}: }{A matrix of composite reliability and 
#'  validity metrics.}
#' }
#' 
#' @examples
#'
#' data(grit)
#' 
#' # Specify a vector of indicators
#' extraversion <- sprintf("e%01d", seq(10))
#' 
#' # Calculate mutual information composite score
#' calc_mi_composite(data = grit,
#'                   var = extraversion,
#'                   entropy = "emp",
#'                   weight = "correlation",
#'                   return_metrics = FALSE)
#' 
#' # Calculate mutual information composite score and metrics
#' calc_mi_composite(data = grit,
#'                   var = extraversion,
#'                   entropy = "emp",
#'                   name = "extraversion",
#'                   digits = 3,
#'                   return_metrics = TRUE)
#'
#' @export
calc_mi_composite <- function(
  
  data,
  var,
  entropy = "emp",
  digits = 3,
  name = NULL,
  return_metrics  
  
) {
  
  
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  df <- infotheo::discretize(df)
  
  
  # -- COMPOSITE CALCULATION -- #
  
  # Initialize an empty matrix
  mi_matrix <- matrix(0, 
                      ncol(df),
                      ncol(df))
  
  # Get pairwise mutual information matrix
  for (i in 1:ncol(df)) {
    
    for (j in i:ncol(df)) {
      
      mi_matrix[j, i] <- infotheo::mutinformation(
        df[, i],
        df[, j]
      ) / sqrt(infotheo::entropy(df[, i], 
                                 method = entropy) * infotheo::entropy(df[, j], 
                                                                       method = entropy))
      
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
  composite_score <- rowMeans(sweep(df,
                                    2,
                                    weights,
                                    "*"),
                              na.rm = T)
  
  
  
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
