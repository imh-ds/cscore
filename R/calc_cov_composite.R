#' Calculate Covariance Composite Scores
#' 
#' @description
#' 
#' Calculate the composite score for the covariance family of weighting schemas.
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
#' @param weight Required weighting schema. Schemas include
#'   \code{c("correlation", "regression", "average")}
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
#' # Calculate correlation-weighted composite score
#' calc_cov_composite(data = grit,
#'                    var = extraversion,
#'                    weight = "correlation",
#'                    return_metrics = FALSE)
#' 
#' # Calculate correlation-weighted composite score and metrics
#' calc_cov_composite(data = grit,
#'                    var = extraversion,
#'                    weight = "correlation",
#'                    name = "extraversion",
#'                    digits = 3,
#'                    return_metrics = TRUE)
#'
#' @export
calc_cov_composite <- function(data,
                               var,
                               weight,
                               digits = 3,
                               name = NULL,
                               return_metrics) {
  
  
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
      lm_formula <- stats::as.formula(paste0(name,
                                             "~",
                                             paste(var,
                                                   collapse = " + ")))
      
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
      reg_weights <- stats::coef(lm.beta::lm.beta(lm_model))[setdiff(names(stats::coef(lm_model)),
                                                                     "(Intercept)")]
      
      # Normalize regression weights
      weights <- reg_weights / mean(reg_weights)
      
      # Return dataframe to original
      df <- df[, -ncol(df)]
      
    }
    
  }
  
  
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
