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
  
  # Standardized loadings from a single common-factor model.
  #
  # The AVE and composite-reliability (rhoc/omega) formulas below are derived
  # for *standardized factor loadings* on the common construct. Two proxies
  # that were used previously are both biased for that quantity:
  #   * item-total correlations (item vs. the composite that CONTAINS it) are
  #     part-whole INFLATED, especially for short scales;
  #   * corrected item-total / item-rest correlations (item vs. the composite of
  #     all OTHER items) are systematically ATTENUATED relative to the factor
  #     loading (e.g., for a 2-item scale the item-rest correlation equals the
  #     inter-item r, whereas the loading is sqrt(r)), which DEFLATES AVE, rhoc,
  #     and every downstream Fornell-Larcker verdict.
  # A one-factor solution estimates the loadings directly and is unbiased for
  # both problems. rhoc computed from these loadings reproduces McDonald's
  # omega for unit weights. See changelog/2026-07-17-factor-analytic-loadings.md.
  loadings <- fa_loadings(df, composite_score)
    
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
    alpha <- if (requireNamespace("ltm", quietly = TRUE)) {
      ltm::cronbach.alpha(df, na.rm = TRUE)$alpha
    } else {
      NA_real_
    }

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


#' Standardized Single-Factor Loadings for Metric Reporting (Internal)
#'
#' Estimates standardized loadings on one common factor from the indicators'
#' pairwise correlation matrix, for use in the AVE and composite-reliability
#' formulas. A minimum-residual (\code{fm = "minres"}) one-factor extraction is
#' used; if it fails, the function falls back to the first principal-component
#' loadings, and finally to each indicator's correlation with the observed
#' composite score. Loadings are clamped to \eqn{[-1, 1]} so that squared
#' loadings and implied error variances stay in \eqn{[0, 1]} even under Heywood
#' cases, keeping AVE and \code{rhoc} bounded.
#'
#' @param df A numeric data frame of indicators (may contain \code{NA}).
#' @param composite_score The observed composite score, used only for the
#'   final fallback when a factor solution is unavailable.
#'
#' @return A named numeric vector of standardized loadings, one per column of
#'   \code{df}.
#'
#' @keywords internal
fa_loadings <- function(df, composite_score) {

  nm <- colnames(df)
  m  <- ncol(df)

  # Pairwise correlation matrix (mirrors the missing-data handling used for the
  # weights). suppressWarnings guards zero-variance columns, which yield NA.
  cor_matrix <- suppressWarnings(
    stats::cor(df, use = "pairwise.complete.obs")
  )

  loadings <- stats::setNames(rep(NA_real_, m), nm)

  # Indicators with a fully defined row/column in the correlation matrix are
  # eligible for the factor solution; degenerate (e.g., zero-variance) items are
  # excluded and handled by the fallback below.
  usable <- if (m >= 2) {
    apply(cor_matrix, 1, function(r) all(is.finite(r)))
  } else {
    rep(FALSE, m)
  }

  if (sum(usable) >= 2) {

    R <- cor_matrix[usable, usable, drop = FALSE]

    fitted <- tryCatch(
      {
        fa_fit <- suppressWarnings(
          psych::fa(
            R,
            nfactors = 1,
            fm       = "minres",
            rotate   = "none",
            n.obs    = nrow(df),
            warnings = FALSE
          )
        )
        as.vector(fa_fit$loadings[, 1])
      },
      error = function(e) NULL
    )

    # Fallback 1: first principal-component loadings if minres fails.
    if (is.null(fitted) || any(!is.finite(fitted))) {
      fitted <- tryCatch(
        {
          pc_fit <- suppressWarnings(
            psych::principal(R, nfactors = 1, rotate = "none")
          )
          as.vector(pc_fit$loadings[, 1])
        },
        error = function(e) NULL
      )
    }

    if (!is.null(fitted) && all(is.finite(fitted))) {
      loadings[usable] <- fitted
    }
  }

  # Fallback 2: any indicator still without a loading (degenerate item, or no
  # factor solution) is scored by its correlation with the observed composite.
  missing_load <- !is.finite(loadings)
  if (any(missing_load)) {
    for (j in which(missing_load)) {
      loadings[j] <- suppressWarnings(
        stats::cor(df[, j], composite_score, use = "pairwise.complete.obs")
      )
    }
  }

  # Remaining undefined loadings (fully degenerate item) contribute nothing.
  loadings[!is.finite(loadings)] <- 0

  # Clamp to [-1, 1] to keep 1 - loadings^2 a valid error variance under
  # Heywood cases, so AVE and rhoc remain in [0, 1].
  loadings <- pmin(pmax(loadings, -1), 1)

  stats::setNames(loadings, nm)
}
