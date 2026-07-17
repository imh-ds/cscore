#' Calculate Information-Weighted Composite Scores
#'
#' @description Create lower- and higher-order composite scores from named sets
#'   of numeric indicators using weights derived from average normalized mutual
#'   information among the indicators.
#'
#' @details
#'
#' Information-weighted composite scoring is implemented by
#' \code{information_score()}, \code{calc_mi_composite()},
#' \code{coerce_to_discrete_if_integer_like()}, \code{is_discrete_variable()},
#' \code{safe_normalize()}, \code{weighted_row_mean()}, and
#' \code{calc_metrics()}. The operative algorithm is as follows.
#'
#' \strong{1. Composite ordering and single-indicator behavior.}
#'
#' The supplied \code{composite_list} is split into lower-order composites
#' (defined directly by observed indicators) and higher-order composites
#' (defined by previously computed composites). Lower-order composites are
#' scored first and appended to \code{data}; higher-order composites, if any,
#' are then scored from those newly appended columns.
#'
#' If a target composite contains a single indicator, that indicator is returned
#' unchanged. When \code{return_metrics = TRUE}, its weight, loading, alpha,
#' composite reliability, and AVE are all reported as 1 by definition.
#'
#' \strong{2. Raw indicator matrix and discretization path.}
#'
#' For a composite with \eqn{m} indicators, let
#' \eqn{\mathbf{X}^{\mathrm{raw}} = [x_{cj}]} denote the raw \eqn{N \times m}
#' numeric indicator matrix. The mutual-information weighting step is not
#' performed directly on \eqn{\mathbf{X}^{\mathrm{raw}}}; instead, the function
#' constructs a working matrix for entropy and mutual-information estimation.
#'
#' First, each indicator is passed through
#' \code{coerce_to_discrete_if_integer_like()}. If a numeric indicator has at
#' most \code{threshold} unique non-missing values, and at least one of those
#' values is non-integer-like, the vector is rounded to the nearest integer.
#' This is a targeted coercion step meant to preserve Likert-type data stored as
#' numeric decimals.
#'
#' After that coercion step, each variable is tested with
#' \code{is_discrete_variable(..., threshold = threshold)}. Factors, ordered
#' factors, integers, and integer-like numerics are treated as discrete. Other
#' numeric vectors are also treated as discrete when they have no more than
#' \code{threshold} unique observed values. If any indicator still fails this
#' discreteness test, the full indicator data frame is discretized using
#' \code{infotheo::discretize()} before mutual information is computed.
#'
#' Consequently, the weighting stage may operate on either:
#'
#' \enumerate{
#'   \item the original numeric values after possible rounding of low-cardinality
#'   integer-like variables, or
#'   \item a fully discretized version of the indicator matrix produced by
#'   \code{infotheo::discretize()}.
#' }
#'
#' The composite scores themselves are still computed on the original raw
#' indicators \eqn{\mathbf{X}^{\mathrm{raw}}}, not on the discretized data.
#'
#' \strong{3. Pairwise normalized mutual information.}
#'
#' Let \eqn{\mathbf{X}^{\mathrm{disc}}} denote the working indicator matrix used
#' for information-theoretic calculations after the steps above. For each pair
#' of indicators \eqn{i} and \eqn{j}, the function computes raw mutual
#' information using \code{infotheo::mutinformation()}:
#'
#' \deqn{M_{ij} = I(X_i; X_j)}
#'
#' It also computes marginal entropies with
#' \code{infotheo::entropy(..., method = entropy)}:
#'
#' \deqn{H_i = H(X_i), \qquad H_j = H(X_j)}
#'
#' The normalized mutual information (NMI) is then defined by the selected
#' \code{nmi_method}.
#'
#' For \code{nmi_method = "geometric"}:
#'
#' \deqn{\mathrm{NMI}_{ij} =
#' \begin{cases}
#' 0, & \text{if } H_i = 0 \text{ or } H_j = 0 \\
#' \frac{M_{ij}}{\sqrt{H_i H_j}}, & \text{otherwise}
#' \end{cases}}
#'
#' For \code{nmi_method = "average"}:
#'
#' \deqn{\mathrm{NMI}_{ij} =
#' \begin{cases}
#' 0, & \text{if } H_i + H_j = 0 \\
#' \frac{2M_{ij}}{H_i + H_j}, & \text{otherwise}
#' \end{cases}}
#'
#' These values populate a symmetric matrix \eqn{\mathbf{N} = [\mathrm{NMI}_{ij}]}
#' whose diagonal is later set to missing so that self-associations do not enter
#' the weighting step.
#'
#' Zero-variance indicators imply zero entropy and therefore produce zero NMI
#' whenever they are paired with any other indicator. The function warns when
#' such indicators are present.
#'
#' \strong{4. Information weights and normalization.}
#'
#' After the diagonal of \eqn{\mathbf{N}} is set to \code{NA}, the raw weight
#' for indicator \eqn{j} is the mean of the off-diagonal NMI values in its
#' column:
#'
#' \deqn{w_j = \frac{1}{m_j}\sum_{i \neq j}\mathrm{NMI}_{ij}}
#'
#' where \eqn{m_j} is the number of non-missing off-diagonal entries for
#' indicator \eqn{j}. Under ordinary conditions, \eqn{m_j = m - 1}.
#'
#' The resulting weight vector is normalized with \code{safe_normalize()},
#' meaning that each weight is divided by the mean absolute weight:
#'
#' \deqn{w_j^{*} = \frac{w_j}{\frac{1}{m}\sum_{k=1}^{m}|w_k|}}
#'
#' This is the exact normalization used in the code. If all raw weights are
#' effectively zero, \code{safe_normalize()} falls back to equal weights of 1
#' for all indicators. As a result, a composite whose pairwise NMI structure is
#' completely degenerate does not fail; it reverts to equal weighting.
#'
#' \strong{5. Casewise composite score on the raw indicators.}
#'
#' Although weights are estimated from the discretized working matrix, the final
#' composite is computed from the original raw indicators by
#' \code{weighted_row_mean(df_raw, weights)}. If \eqn{O_c} is the set of
#' non-missing indicators for case \eqn{c}, then the implemented score is
#'
#' \deqn{C_c = \frac{\sum_{j \in O_c} x_{cj} w_j^{*}}{\sum_{j \in O_c}|w_j^{*}|}}
#'
#' If all indicators are missing for a case, the composite is \code{NA}. When
#' all weights are positive, this is the ordinary weighted mean over the
#' observed indicators.
#'
#' An important implication of the implementation is that missing values are not
#' imputed prior to mutual-information estimation in this scoring function.
#' Because the discretization and entropy routines operate on the observed data
#' as passed in, users should generally impute missing indicator values before
#' applying \code{information_score()} if they want the MI weights to reflect
#' substantive categories rather than missingness patterns.
#'
#' \strong{6. Reliability and validity metrics.}
#'
#' When \code{return_metrics = TRUE}, downstream metrics are computed from the
#' raw indicators and final normalized weights using \code{calc_metrics()}. The
#' reported indicator loadings \eqn{\lambda_j} are standardized loadings on a
#' single common factor, estimated by a minimum-residual one-factor solution of
#' the indicators' pairwise-complete correlation matrix
#' (\code{psych::fa(..., nfactors = 1, fm = "minres")}). Unlike an item-rest
#' (corrected item-total) correlation, a factor loading is not attenuated toward
#' the construct, so AVE and \eqn{\rho_c} are not deflated; unlike an item-total
#' correlation against the full composite, it avoids part-whole inflation.
#'
#' Using these corrected loadings, the downstream validity metrics are computed
#' as
#'
#' \deqn{\mathrm{AVE} =
#' \frac{\sum_{j=1}^{m}\lambda_j^2 w_j^{*}}
#' {\sum_{j=1}^{m}\lambda_j^2 w_j^{*} +
#'  \sum_{j=1}^{m}(1-\lambda_j^2) w_j^{*}}}
#'
#' \deqn{\rho_c =
#' \frac{\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2}
#' {\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2 +
#'  \sum_{j=1}^{m}(1-\lambda_j^2)(w_j^{*})^2}}
#'
#' Cronbach's alpha is obtained from \code{ltm::cronbach.alpha()} when
#' \pkg{ltm} is installed and otherwise returned as \code{NA}. As with the
#' other weighted scoring functions, the weighted composite reliability
#' \eqn{\rho_c} is the metric most directly aligned with the implemented
#' weighting model.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
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
#' @param htmt_cutoff Numeric threshold used for HTMT PASS/FAIL classification
#'   in the discriminant-validity summary returned when
#'   \code{return_metrics = TRUE}. Default is \code{0.90}.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' @param file An optional file path. If specified, the results will be written
#'   as a formatted excel workbook. This argument is only relevant if
#'   \code{return_metrics = TRUE}.
#' @param name A required string denoting the name of the composite variable.
#'
#' @return If \code{return_metrics = FALSE}, a dataframe identical to the input
#'   dataframe, with additional columns appended at the end, is returned. These
#'   new columns represent the calculated composite scores. If
#'   \code{return_metrics = TRUE}, a list containing the following dataframes is
#'   returned:
#'  \itemize{
#'  \item \strong{Data}: A dataframe with the composite variables appended as new
#'  variables.
#'  \item \strong{Metrics}: A matrix of indicator loadings and weights metrics.
#'  \item \strong{Validity}: A matrix of composite reliability and validity
#'  metrics.
#'  \item \strong{Discriminant Summary}: A matrix of reporting-set
#'  discriminant-validity statistics and PASS/FAIL results.
#'  \item \strong{Fornell-Larcker}: A reporting-set matrix whose diagonal is
#'  \eqn{\sqrt{AVE}} and whose off-diagonals are absolute inter-construct
#'  correlations.
#'  \item \strong{HTMT}: A reporting-set matrix of HTMT ratios.
#' }
#'
#' @examples
#'
#' data(grit)
#'
#' # Specify the named list with composite names and their respective indicators
#' composite_list <- composite_list(
#'
#'   # Lower-order composites
#'   extraversion          = sprintf("e%01d", seq(10)),
#'   neuroticism           = sprintf("n%01d", seq(10)),
#'   agreeableness         = sprintf("a%01d", seq(10)),
#'   conscientiousness     = sprintf("c%01d", seq(10)),
#'   openness              = sprintf("o%01d", seq(10)),
#'   consistency_interest  = sprintf("gs%01d", c(2,3,5,7,8,11)),
#'   perseverance_effort   = sprintf("gs%01d", c(1,4,6,9,10,12)),
#'
#'   # Higher-order composites
#'   grit                  = c("consistency_interest", "perseverance_effort")
#'
#'  )
#'
#' # Mutual-information scoring requires the suggested 'infotheo' package and
#' # can be slow, so the calls are wrapped in \donttest{}.
#' \donttest{
#' # Calculate mutual-information-weighted composite scores
#' information_score(data = grit,
#'                   composite_list = composite_list)
#'
#' # Calculate mutual-information composite scores, reliability, & validity
#' # (optionally write a formatted Excel workbook to a temporary file)
#' out <- file.path(tempdir(), "composite.xlsx")
#' information_score(data = grit,
#'                   composite_list = composite_list,
#'                   digits = 3,
#'                   return_metrics = TRUE,
#'                   file = out)
#'
#' unlink(out)
#' }
#'
#' @export
information_score <- function(
    data,
    composite_list,
    entropy = c("emp", "mm", "shrink", "sg"),
    nmi_method = c("geometric", "average"),
    threshold = 10,
    htmt_cutoff = 0.90,
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
){
  
  # -- MATCH ARGUMENTS -- #
  
  entropy <- match.arg(entropy)
  nmi_method <- match.arg(nmi_method)
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each variable of lower order composite list
    data[names(lower_order_varlist)] <- lapply(
      lower_order_varlist,
      function(var) {
        
        if (length(var) == 1) {
          
          calc_single_indicator(
            data = data,
            var = var,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics
          )
          
        } else {
          
          calc_mi_composite(
            data,
            var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics
          )
          
        }
        
      }
    )
    
    # Update the data object
    data <- as.data.frame(data)
    
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each variable of higher order composite list
      data[names(higher_order_varlist)] <- lapply(
        higher_order_varlist,
        function(var) {
          
          calc_mi_composite(
            data,
            var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics)
          
        }
      )
      
    }
    
    
    # Return
    return(data)
    
  }
  
  
  
  # -- IF CALCULATING COMPOSITE SCORE & METRICS -- #
  if(return_metrics == TRUE){
    
    # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each element of the lower order varlist
    lower_results <- lapply(
      names(lower_order_varlist),
      function(name) {
        
        var <- lower_order_varlist[[name]]
        
        if (length(var) == 1) {
          
          calc_single_indicator(
            data = data,
            var = var,
            name = name,
            digits = digits,
            return_metrics = return_metrics
          )
          
        } else {
          
          calc_mi_composite(
            data = data,
            var = var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
            name = name,
            digits = digits,
            return_metrics = return_metrics
          )
          
        }
        
      }
    )
    
    
    # Extract the results
    data[names(lower_order_varlist)] <- purrr::map(
      lower_results, 
      "composite_score"
    )
    
    metrics <- purrr::list_rbind(
      purrr::map(
        lower_results, "composite_metrics"
      )
    )
    
    validity <- purrr::list_rbind(
      purrr::map(
        lower_results, "composite_validity"
      )
    )
    
    # Update the data object
    data <- as.data.frame(data)
    
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each element of the higher order varlist
      higher_results <- lapply(
        names(higher_order_varlist),
        function(name) {
          
          var <- higher_order_varlist[[name]]
          
          calc_mi_composite(
            data = data,
            var = var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
            name = name,
            digits = digits,
            return_metrics = return_metrics
          )
          
        }
      )
      
      # Extract the results
      data[names(higher_order_varlist)] <- purrr::map(
        higher_results, 
        "composite_score"
      )
      
      metrics <- rbind(
        metrics,
        purrr::list_rbind(
          purrr::map(
            higher_results, "composite_metrics"
          )
        )
      )
      
      validity <- rbind(
        validity,
        purrr::list_rbind(
          purrr::map(
            higher_results, "composite_validity"
          )
        )
      )
      
      # Update the data object
      data <- as.data.frame(data)
      
    }
    
    
    # Combine into returnable list
    
    composite_sheets <- finalize_metric_output(
      data = data,
      metrics = metrics,
      validity = validity,
      composite_list = composite_list,
      digits = digits,
      file = file,
      name = name,
      htmt_cutoff = htmt_cutoff
    )
    
    # -- RETURN -- #
    return(composite_sheets)
    
  }
  
}
