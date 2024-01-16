#' Calculate Standard Deviation Composite Scores
#'
#' @description
#' 
#' Calculate the composite score for the standard deviation family of weighting
#' schemas. For information on the specifics calculations, refer to the help
#' documentations of \code{?sd_upweight_score} (for upweighted standard
#' deviation) and \code{?sd_downweight_score} (for downweighted standard
#' deviation).
#'
#' Refer to help documentation \code{?calc_metrics} for information on how
#' reliability and validity metrics are calculated.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include
#'   \code{c("upweight", "downweight")}
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
#' calc_sd_composite(data = grit,
#'                   var = extraversion,
#'                   weight = "sd_upweight",
#'                   return_metrics = FALSE)
#' 
#' # Calculate correlation-weighted composite score and metrics
#' calc_sd_composite(data = grit,
#'                   var = extraversion,
#'                   weight = "sd_upweight",
#'                   name = "extraversion",
#'                   digits = 3,
#'                   return_metrics = TRUE)
#'
#' @export
calc_sd_composite <- function(data,
                              var,
                              weight,
                              digits = 3,
                              name = NULL,
                              return_metrics) {
  
  
  # -- DATA PREPARATION -- #
  
  # Select the numeric variables
  df <- data[, var]
  

  
  # -- CONDITIONAL COMPOSITE CALCULATION -- #
  
  # IF SD IS UPWEIGHTED ----
  if(weight == "sd_upweight") {
    
    sd_weights <- apply(df,
                        2, 
                        function(x) stats::sd(x, na.rm = TRUE))
    
  }

  # IF SD IS DOWNWEIGHTED ----
  if(weight == "sd_downweight") {
    
    sd_weights <- 1 / apply(df, 
                            2, 
                            function(x) stats::sd(x, na.rm = TRUE))
    
  }
  
  # Normalize weights
  weights <- sd_weights / mean(sd_weights)
  
  # Calculate SD composite score
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
