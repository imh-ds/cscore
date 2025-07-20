#' Calculate Covariance Composite Scores
#'
#' @description Calculate the composite score for the covariance family of
#'   weighting schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include
#'   \code{c("correlation", "regression", "average")}
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
#'  \item{\code{composite_score}: }{An array with the calculated composite
#'  variable.} \item{\code{composite_metrics}: }{A matrix loadings and weights
#'  of the indicators.}
#'  \item{\code{composite_validity}: }{A matrix of composite reliability and
#'  validity metrics.}
#' }
#'
#' @keywords internal
#' @noRd
calc_cov_composite <- function(
    data,
    var,
    weight,
    digits = 3,
    name = NULL,
    return_metrics
) {
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]

  
  # -- CONDITIONAL COMPOSITE CALCULATION -- #
  
  # IF UNWEIGHTED ----
  if(weight == "average"){
    
    # Set equal weights
    weights <- rep(1, ncol(df))
    
  }
  
  
  
  # IF COVARIANCE-WEIGHTED ----
  if(weight %in% c("correlation", "regression")){
    
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
    
    
    
    # IF CORRELATION-WEIGHTED ----
    if(weight == "correlation"){
      
      # Designate weight
      weights <- cor_weights
      
    }
    
    
    
    # IF REGRESSION-WEIGHTED ----
    if(weight == "regression"){
      
      # Write linear model formula
      lm_formula <- stats::as.formula(
        paste0(
          name,
          "~",
          paste
          (var,
            collapse = " + "
          )
        )
      )
      
      # Calculate correlation weighted composite score and save to dataframe
      df[[name]] <- rowMeans(sweep(df,
                                   2,
                                   cor_weights,
                                   "*"),
                             na.rm = T)
      
      # Calculate regression weights
      lm_model <- stats::lm(lm_formula,
                            data = df)
      
      # Get regression weights
      # NOTE: Getting standardized beta coefficients to avoid issues with different scales
      reg_weights <- stats::coef(
        lm.beta::lm.beta(lm_model))[setdiff(names(stats::coef(lm_model)),
                                            "(Intercept)")]
      
      # Normalize regression weights
      weights <- reg_weights / mean(reg_weights)
      
      # Return dataframe to original
      df <- df[, -ncol(df)]
      
    }
    
  }
  
  
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
