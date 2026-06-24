#' Calculate Standard Deviation Composite Scores
#'
#' @description Calculate the composite score for the standard deviation family
#'   of weighting schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include \code{c("upweight",
#'   "downweight")}
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
#'  \item \code{composite_score}: An array with the calculated composite
#'  variable.
#'  \item \code{composite_metrics}: A matrix loadings and weights
#'  of the indicators.
#'  \item \code{composite_validity}: A matrix of composite reliability and
#'  validity metrics.
#' }
#'
#' @keywords internal
calc_sd_composite <- function(
    data,
    var,
    weight,
    digits = 3,
    name = NULL,
    return_metrics
) {
  
  # -- DATA PREPARATION -- #

  # Select the numeric variables
  df <- data[, var]

  # -- INPUT VALIDATION -- #

  # All indicators must be numeric
  non_numeric <- vapply(df, Negate(is.numeric), logical(1))
  if (any(non_numeric)) {
    stop(
      "Non-numeric indicator(s) detected: ",
      paste(names(df)[non_numeric], collapse = ", "),
      ". All indicators must be numeric for SD-family composite scoring.",
      call. = FALSE
    )
  }


  # -- CONDITIONAL COMPOSITE CALCULATION -- #

  # IF SD IS UPWEIGHTED ----
  if(weight == "sd_upweight") {

    sd_weights <- vapply(df, function(x) stats::sd(x, na.rm = TRUE), numeric(1))

    # Zero-variance items receive weight 0 and are effectively excluded
    zero_var <- sd_weights == 0 | is.na(sd_weights)
    if (any(zero_var)) {
      warning(
        "Zero-variance indicator(s) in sd_upweight: ",
        paste(names(df)[zero_var], collapse = ", "),
        ". These items receive weight 0 and are excluded from the composite.",
        call. = FALSE
      )
      sd_weights[zero_var] <- 0
    }

  }

  # IF SD IS DOWNWEIGHTED ----
  if(weight == "sd_downweight") {

    raw_sds <- vapply(df, function(x) stats::sd(x, na.rm = TRUE), numeric(1))

    # Zero-variance items have SD = 0, so 1/SD is undefined
    zero_var <- raw_sds == 0 | is.na(raw_sds)
    if (any(zero_var)) {
      stop(
        "Zero-variance indicator(s) detected in sd_downweight: ",
        paste(names(df)[zero_var], collapse = ", "),
        ". Downweighting by SD requires non-zero variance in every indicator (1/0 is undefined).",
        call. = FALSE
      )
    }

    sd_weights <- 1 / raw_sds

  }

  # Normalize weights
  weights <- safe_normalize(sd_weights)
  
  # Calculate SD composite score
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
