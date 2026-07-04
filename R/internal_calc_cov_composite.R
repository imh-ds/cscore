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
#'  \item \code{composite_score}: An array with the calculated composite
#'  variable.
#'  \item \code{composite_metrics}: A matrix loadings and weights
#'  of the indicators.
#'  \item \code{composite_validity}: A matrix of composite reliability and
#'  validity metrics.
#' }
#'
#' @keywords internal
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

  # -- INPUT VALIDATION -- #

  # All indicators must be numeric
  non_numeric <- vapply(df, Negate(is.numeric), logical(1))
  if (any(non_numeric)) {
    stop(
      "Non-numeric indicator(s) detected: ",
      paste(names(df)[non_numeric], collapse = ", "),
      ". All indicators must be numeric for covariance-family composite scoring.",
      call. = FALSE
    )
  }

  # Zero-variance items produce undefined correlations; warn and note behavior
  if (weight %in% c("correlation", "regression")) {
    item_sds <- vapply(df, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
    zero_var <- item_sds == 0 | is.na(item_sds)
    if (any(zero_var)) {
      warning(
        "Zero-variance indicator(s) detected: ",
        paste(names(df)[zero_var], collapse = ", "),
        ". Correlations with these items are undefined (NA) and they will receive equal weight.",
        call. = FALSE
      )
    }
  }

  
  # -- CONDITIONAL COMPOSITE CALCULATION -- #
  
  # IF UNWEIGHTED ----
  if(weight == "average"){
    
    # Set equal weights
    weights <- rep(1, ncol(df))
    
  }
  
  
  
  # IF COVARIANCE-WEIGHTED ----
  if(weight %in% c("correlation", "regression")){
    
    # Calculate correlation matrix
    cor_matrix <- suppressWarnings(
      stats::cor(df, use = "pairwise.complete.obs")
    )
    
    # Remove correlation matrix diagonal
    diag(cor_matrix) <- NA
    
    
    
    # -- COMPOSITE WEIGHTS CALCULATION -- #
    
    # Get the correlation weights
    cor_weights <- colMeans(cor_matrix,
                            na.rm = T)

    # Zero-variance indicators (or otherwise undefined columns) produce NaN/NA;
    # replace with 1 so they receive equal baseline weight before normalization.
    cor_weights[is.na(cor_weights)] <- 1

    # Normalize correlation weights; safe_normalize uses mean(abs(w)) as the
    # denominator so a near-zero mean from mixed-sign weights cannot explode.
    cor_weights <- safe_normalize(cor_weights)

    # Warn if any weights are negative — likely caused by reverse-keyed items
    if (any(cor_weights < 0, na.rm = TRUE)) {
      warning(
        "Negative correlation weights detected for indicator(s): ",
        paste(names(cor_weights)[which(cor_weights < 0)], collapse = ", "),
        ". This typically indicates reverse-keyed items. Please recode them before running composite_score().",
        call. = FALSE
      )
    }


    # IF CORRELATION-WEIGHTED ----
    if(weight == "correlation"){

      # Designate weight
      weights <- cor_weights

    }
    
    
    
    # IF REGRESSION-WEIGHTED ----
    if(weight == "regression"){

      # Thomson (1951) regression factor score weights: w = R^{-1} * lambda_PC1.
      # Unlike regressing the correlation-weighted composite back on its own
      # items (circular, since the composite is a linear function of those items),
      # this derives weights purely from the inter-item correlation structure.
      # Items that overlap heavily with others are shrunk by R^{-1}, making
      # regression weighting meaningfully distinct from correlation weighting by
      # penalizing redundant items.
      
      # Handle zero-variance items: run PCA only on non-zero-variance items if at least 2 exist
      item_sds <- vapply(df, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
      zero_var <- item_sds == 0 | is.na(item_sds)
      
      if (any(zero_var)) {
        non_zero_vars <- names(df)[!zero_var]
        reg_weights <- numeric(ncol(df))
        names(reg_weights) <- colnames(df)
        
        if (length(non_zero_vars) >= 2) {
          pca_model <- tryCatch({
            suppressWarnings(
              psych::principal(
                df[, non_zero_vars, drop = FALSE],
                nfactors = 1,
                rotate = "none"
              )
            )
          }, error = function(e) NULL)
          
          if (!is.null(pca_model)) {
            reg_weights[non_zero_vars] <- as.vector(pca_model$weights[, 1])
          } else {
            reg_weights[non_zero_vars] <- 1
          }
        } else {
          reg_weights[non_zero_vars] <- 1
        }
        reg_weights[zero_var] <- 1
      } else {
        pca_model <- suppressWarnings(
          psych::principal(
            df,
            nfactors = 1,
            rotate = "none"
          )
        )

        reg_weights <- setNames(
          as.vector(pca_model$weights[, 1]),
          colnames(df)
        )
      }

      # Normalize weights
      weights <- safe_normalize(reg_weights)

      # Warn if any weights are negative — likely caused by reverse-keyed items
      if (any(weights < 0, na.rm = TRUE)) {
        warning(
          "Negative regression weights detected for indicator(s): ",
          paste(names(weights)[which(weights < 0)], collapse = ", "),
          ". This typically indicates reverse-keyed items. Please recode them before running composite_score().",
          call. = FALSE
        )
      }

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
