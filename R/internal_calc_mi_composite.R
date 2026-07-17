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

  if (!requireNamespace("infotheo", quietly = TRUE)) {
    stop(
      "Package 'infotheo' is required for mutual-information composite scoring. ",
      "Install it with: install.packages(\"infotheo\")",
      call. = FALSE
    )
  }
  
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars (raw)
  df_raw <- data[, var]
  
  # -- INPUT VALIDATION -- #

  # All indicators must be numeric
  non_numeric <- vapply(df_raw, Negate(is.numeric), logical(1))
  if (any(non_numeric)) {
    stop(
      "Non-numeric indicator(s) detected: ",
      paste(names(df_raw)[non_numeric], collapse = ", "),
      ". All indicators must be numeric for mutual-information composite scoring.",
      call. = FALSE
    )
  }

  # Zero-variance items produce 0 entropy/NMI; warn and note behavior
  item_sds <- vapply(df_raw, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  zero_var <- item_sds == 0 | is.na(item_sds)
  if (any(zero_var)) {
    warning(
      "Zero-variance indicator(s) detected: ",
      paste(names(df_raw)[zero_var], collapse = ", "),
      ". These items have 0 mutual information and will receive weight 0.",
      call. = FALSE
    )
  }

  df <- df_raw
  
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
      function(x) is_discrete_variable(df[[x]], threshold = threshold)
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
      
      # Calculate raw MI (unnormalized). Use the SAME entropy estimator as the
      # normalizing entropies below so the numerator and denominator are
      # coherent. Previously the numerator always used the default empirical
      # ("emp") estimator while the denominators used `entropy`, so for
      # `entropy != "emp"` the NMI was internally inconsistent (e.g., two
      # identical variables did not yield NMI = 1).
      mi_raw <- infotheo::mutinformation(
        df[, i],
        df[, j],
        method = entropy
      )

      # Calculate entropy of var i and j
      ent_i <- infotheo::entropy(df[, i],
                                 method = entropy)
      ent_j <- infotheo::entropy(df[, j],
                                 method = entropy)
      
      # Calculate Normalized MI (NMI)
      nmi <- if (nmi_method == "geometric") {
        
        # If NMI option is set to calculate using geometric mean entropy:
        if (ent_i == 0 || ent_j == 0) {
          0
        } else {
          mi_raw / sqrt(ent_i * ent_j)
        }
        
      } else {
        
        # If NMI option is set to calculate using average entropy:
        if (ent_i + ent_j == 0) {
          0
        } else {
          2 * mi_raw / (ent_i + ent_j)
        }
        
      }
      
      # Store in matrix
      mi_matrix[j, i] <- nmi
      mi_matrix[i, j] <- mi_matrix[j, i]
      
    }
    
  }
  
  # Set diagonal to NA
  diag(mi_matrix) <- NA
  
  # Get the information weights
  weights <- colMeans(mi_matrix, na.rm = TRUE)

  # Normalize information weights
  weights <- safe_normalize(weights)
  
  # Calculate unweighted composite score on raw data
  composite_score <- weighted_row_mean(df_raw, weights)
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    return(composite_score)
    
  }
  
  
  # -- IF CALCULATING COMPOSITE, RELIABILITY, & VALIDITY -- #
  if(return_metrics == TRUE){
    
    metrics <- calc_metrics(df = df_raw,
                            composite_score = composite_score,
                            weights = weights,
                            digits = digits,
                            name = name)
    
    return(metrics)
  }
  
}
