#' Calculate Composite Score Metrics
#'
#' @description Calculate the indicator and composite-level reliability and
#'   validity metrics.
#'
#' @details Indicator loadings are calculated as the bivariate correlation
#'   between the indicator and the correlation-weighted composite score. This is
#'   mathematically represented as:
#'
#' \deqn{\lambda[i] = cor(I[i], \bar{cs}_c)}{\lambda[i] = cor(I[i],
#' composite_c)}
#'
#'   where \eqn{I[i]} is the indicator and \eqn{cor()} is the function to create
#'   a correlation matrix. Two reliability metrics are given: Cronbach's Alpha
#'   (\eqn{\alpha}) and Composite Reliability (\eqn{\rho_c}, i.e.,
#'   \eqn{\omega}). Cronbach's Alpha measures construct reliability based on the
#'   covariance among the indicators.
#'
#' \deqn{\alpha = \frac{k \bar{c}}{\bar{v} + (k - 1) \bar{c}}}{alpha = k *
#' mean(c) / (mean(v) + (k - 1) * mean(c))}
#'
#'   where \eqn{\bar{v}} is the average variance and \eqn{\bar{c}} is the
#'   average inter-item covariance. Cronbach's Alpha is a popular approach to
#'   assessing construct reliability and is the most widely known reliability
#'   measure in the social and behavioral sciences. However, it has several
#'   psychometric problems. Firstly, Alpha assumes \emph{tau-equivalence}, i.e.,
#'   that all indicators contribute equally to the construction of the composite
#'   and have uniform indicator loadings. This can be forced by the researcher
#'   when constructing the composite using sum-averages, but often does not
#'   reflect the reality of psychometric measures. Secondly, Alpha is biased
#'   based on the number of indicators. In other words, Alpha can be
#'   misleadingly good even when the measure is poor if there are lots of
#'   indicators, and likewise can be misleadingly bad even when the measure is
#'   good if there are only a few indicators. This is due to the number of
#'   indicators (i.e., \eqn{k}) being in the numerator. For these reasons,
#'   Cronbach's Alpha is calculated and provided in the output for when
#'   researchers may need the value for whatever reason. However, the
#'   recommendation is to not use Alpha.
#'
#'   An alternative reliability measurement, McDonald's Omega (\eqn{\omega}) has
#'   been gaining popularity due to its assumption of multidimensionality within
#'   the data. However, because Omega is computing its own factor structure to
#'   determine multidimensionality, it can often result in calculating
#'   reliability based on a different factor model than the researcher's \emph{a
#'   priori} structure. This results in a reliability calculation of a different
#'   model than one that is used in analysis. One way to address this is to
#'   manually fit a researcher-defined factor model (e.g.,
#'   \code{psych::omegaFromSem} offers a way to fit a \code{lavaan} object).
#'   However, popular softwares do not always offer this feature (e.g., JASP,
#'   jamovi), resulting in some barriers to adoption if the researcher is not
#'   familiar with \code{lavaan} and R. From a practical perspective, the
#'   difference in reliability from this issue is often miniscule and a
#'   non-issue. Nonetheless, systematic misrepresentation can yield problems in
#'   the future. For this reason, this package calculates Composite Reliability
#'   (\eqn{\rho_c}) and fits it the defined user-defined measurement structure.
#'   Further, because of the weighted nature of the composite calculation in
#'   this package, the indicator loadings are appropriately weighted in
#'   \eqn{\rho_c} as follows:
#'
#' \deqn{\rho_c = \frac{(\sum \lambda[i]*w[i])^2}{(\sum \lambda[i]*w[i])^2 +
#' \sum e_w[i]}}{rho_c = (sum(\lambda[i]*w[i])^2) / ((sum(\lambda[i]*w[i])^2) +
#' sum(e_w[i]))}
#'
#'   Although the reliability is named Composite Reliability to keep consistent
#'   with naming conventions of the Partial Least Squares Structural Equation
#'   Modeling (PLS-SEM) literature, the underlying formula is identical to
#'   McDonald's Omega. Researchers can report this metric as either \eqn{\rho_c}
#'   or \eqn{\omega}.
#'
#'   Lastly, the Average Variance Extracted (AVE) is calculated and given in the
#'   output. AVE is often used to reflect how much the indicators capture the
#'   composite variable's variance (vs the variance being due to measurement
#'   error). AVE is frequently used to determine discriminant validity based on
#'   the Fornell-Larcker criterion, i.e., that the square root of the AVE is
#'   greater than the highest bivariate correlation with any other composite
#'   variable:
#'
#'   \deqn{\sqrt{AVE_i} > \max_{j}(r_{ij})}
#'
#' The Average Variance Extracted (AVE) is calculated as: \deqn{AVE = \frac{\sum
#' \lambda^2_{w[i]}}{\sum \lambda^2_{w[i]} + \sum e_w[i]}}{ave = sum(lambda^2_w[i])
#' / (sum(lambda^2_{w[i]}) + sum(e_{w[i]}))}
#'
#'   In which the weighted loadings squared (\eqn{\lambda^2_w}) is calculated
#'   as:
#' \deqn{\lambda^2_{w[i]} = (\lambda[i]^2) * w[i]}{\lambda^2_{w[i]} = (\lambda[i]^2)
#' * w[i]}
#'
#' And weighted errors (\eqn{e_w}) are calculated as: \deqn{e_{w[i]} = (1 -
#' \lambda[i]^2) * w[i]}{e_{w[i]} = (1 - lambda[i]^2) * w[i]}
#'
#'
#' @param df A dataframe object. This should be a structured dataset where each
#'   column represents a variable and each row represents an observation.
#' @param composite_score An array representing the composite score.
#' @param weights A vector of weights for the indicator variables.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param name A string representing the name of the composite score.
#'
#' @return A list of dataframes consisting of:
#' \itemize{
#'  \item \code{composite_score}: An array with the calculated composite
#'  variable.
#'  \item \code{composite_metrics}: A matrix loadings and weights
#'  of the indicators.
#'  \item \code{composite_validity}: A matrix of composite reliability and
#'  validity metrics.
#' }
#'
#' @export
calc_metrics <- function(
    df,
    composite_score,
    weights,
    digits,
    name
){
  
  # Get loadings as the correlation between indicator and composite
  df_loadings <- cbind(composite_score,
                       df)
  
  # Get loadings
  loadings <- stats::cor(df_loadings,
                         use = "pairwise.complete.obs")[,1][-1]
  
  # Get weighted loadings squared
  loadings_squared <- (loadings^2)*weights
  
  # Get weighted errors
  errors <- (1 - loadings^2)*weights
  
  
  # Calculate Cronbach's alpha
  alpha <- ltm::cronbach.alpha(df, na.rm = T)$alpha
  
  # Calculate Average Variance Extracted (AVE)
  ave <- sum(loadings_squared) / (sum(loadings_squared) + sum(errors))
  
  # Calculate Composite Reliability
  rhoc <- sum(loadings*weights)^2 / (sum(loadings*weights)^2 + sum(errors))
  
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
  min_weight  <- min(weights)
  max_weight  <- max(weights)
  
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
  composite_metrics <- data.frame(weights  = weights,
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
