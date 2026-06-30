#' Calculate Correlation-Weighted Composite Scores
#'
#' @description Create lower- and higher-order composite scores from named sets
#'   of numeric indicators using either correlation-based weights or
#'   one-factor regression weights derived from the indicators' correlation
#'   structure.
#'
#' @details
#'
#' Composite scoring is implemented by \code{calc_cov_composite()},
#' \code{safe_normalize()}, \code{weighted_row_mean()}, and
#' \code{calc_metrics()}. The operative algorithm is:
#'
#' \enumerate{
#'   \item The supplied \code{composite_list} is split into lower-order
#'   composites (defined directly by observed indicators) and higher-order
#'   composites (defined by previously computed composites). Lower-order
#'   composites are scored first, appended to \code{data}, and higher-order
#'   composites are then scored from those newly available columns.
#'   \item For a target composite with \eqn{m} indicators, construct the
#'   \eqn{N \times m} indicator matrix \eqn{\mathbf{X} = [x_{cj}]}, where
#'   \eqn{c = 1, \ldots, N} indexes cases and \eqn{j = 1, \ldots, m} indexes
#'   indicators.
#'   \item If \eqn{m = 1}, the single indicator is returned unchanged. When
#'   \code{return_metrics = TRUE}, its reported weight, loading, alpha,
#'   composite reliability, and AVE are all set to 1 by definition.
#'   \item If \eqn{m > 1}, raw weights are computed by the selected weighting
#'   method and then normalized with \code{safe_normalize()}:
#'
#'   \deqn{w_j^{*} = \frac{w_j}{\frac{1}{m}\sum_{k=1}^{m}|w_k|}}
#'
#'   This is the exact normalization used in the code. It scales weights so
#'   that their mean absolute value equals 1, which prevents instability when
#'   weights contain mixed signs.
#'   \item Composite scores are then computed casewise using only the observed
#'   indicators for that case. If \eqn{O_c} is the set of non-missing
#'   indicators for case \eqn{c}, then \code{weighted_row_mean()} returns
#'
#'   \deqn{C_c = \frac{\sum_{j \in O_c} x_{cj} w_j^{*}}{\sum_{j \in O_c}|w_j^{*}|}}
#'
#'   If all indicators are missing for a case, \eqn{C_c} is \code{NA}. When
#'   all weights are positive, this reduces to the usual weighted mean over the
#'   observed indicators.
#' }
#'
#' \strong{Correlation weighting.} The function computes the pairwise-complete
#' Pearson correlation matrix \eqn{\mathbf{R} = [r_{ij}]} with
#' \code{stats::cor(..., use = "pairwise.complete.obs")}. The diagonal is then
#' set to missing so that self-correlations do not contribute. For indicator
#' \eqn{j}, the raw weight is the mean of the available off-diagonal
#' correlations in column \eqn{j}:
#'
#' \deqn{w_j = \frac{1}{m_j}\sum_{i \neq j} r_{ij}}
#'
#' where \eqn{m_j} is the number of non-missing off-diagonal correlations for
#' indicator \eqn{j}. Under complete data and non-degenerate items,
#' \eqn{m_j = m - 1}; if some correlations are undefined, the average is taken
#' over the remaining defined entries because \code{colMeans(..., na.rm = TRUE)}
#' is used. If all off-diagonal correlations for an indicator are undefined,
#' the raw weight is replaced with 1 before normalization, giving that
#' indicator an equal baseline weight.
#'
#' \strong{Regression weighting.} The function uses the one-component solution
#' from \code{psych::principal(..., nfactors = 1, rotate = "none")} and takes
#' the first column of \code{pca_model$weights}. These are the regression
#' factor score coefficients for the one-factor principal-components solution.
#' In the standard full-rank case, they correspond to Thomson regression
#' scoring:
#'
#' \deqn{\mathbf{w} = \mathbf{R}^{-1}\boldsymbol{\lambda}}
#'
#' where \eqn{\mathbf{R}} is the inter-indicator correlation matrix and
#' \eqn{\boldsymbol{\lambda}} is the loading vector for the first principal
#' component. Relative to simple correlation weighting, the
#' \eqn{\mathbf{R}^{-1}} term shrinks indicators that are highly redundant with
#' the others, so the resulting weights reflect unique rather than merely
#' average shared variance.
#'
#' The implementation includes an explicit zero-variance fallback. If one or
#' more indicators have zero variance, PCA-based regression weights are
#' computed only on the subset of indicators with non-zero variance, provided
#' that at least two such indicators remain. Indicators excluded because of
#' zero variance receive raw weight 1. If the PCA fit fails, the non-zero-
#' variance indicators also fall back to raw weight 1. The full vector is then
#' normalized in the same way as above.
#'
#' In both weighting modes, negative weights trigger a warning because they
#' often indicate reverse-keyed indicators that have not been recoded. The
#' function does not reverse-score those items automatically; it uses the
#' weights implied by the observed data.
#'
#' When \code{return_metrics = TRUE}, the downstream metric calculations are
#' based on the final composite scores and normalized weights. The reported
#' indicator loadings are corrected item-total correlations rather than
#' correlations with the full composite that contains the focal item. For
#' indicator \eqn{j}, the implemented loading is
#'
#' \deqn{\lambda_j = \mathrm{cor}\!\left(x_j,\,
#' \frac{\sum_{k \neq j} x_k w_k^{*}}{\sum_{k \neq j}|w_k^{*}|}\right)}
#'
#' computed with pairwise-complete observations. This avoids part-whole
#' inflation.
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
#' \pkg{ltm} is installed and otherwise returned as \code{NA}. Alpha is
#' included for conventional reporting, but the weighted composite reliability
#' \eqn{\rho_c} is the metric that most directly reflects the weighted scoring
#' model implemented here.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param weight Required weighting schema. Schemas include
#'   \code{c("correlation", "regression")}. Default is \code{"correlation"}.
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
#' @returns If \code{return_metrics = FALSE}, a dataframe identical to the input
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
#' # Calculate correlation-weighted composite scores
#' correlation_score(data = grit,
#'                   composite_list = composite_list)
#'
#' # Calculate correlation-weighted composite scores, reliability, & validity
#' correlation_score(data = grit,
#'                   composite_list = composite_list,
#'                   digits = 3,
#'                   return_metrics = TRUE,
#'                   file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#' @export
correlation_score <- function(
    data = .,
    composite_list,
    weight = c("correlation", "regression"),
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
){
  
  # -- MATCH ARGUMENTS -- #
  
  weight <- match.arg(weight)
  
  
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
          
          calc_cov_composite(
            data = data,
            var = var,
            weight = weight,
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
          
          calc_cov_composite(
            data = data,
            var = var,
            weight = weight,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics
          )
          
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
          
          calc_cov_composite(
          data = data,
          var = var,
          weight = weight,
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
          
          calc_cov_composite(
            data = data,
            var = var,
            weight = weight,
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
    
    composite_sheets <- list(
      data = data,
      metrics = metrics,
      validity = validity
    )
    
    
    
    # -- IF FILE PATH IS SPECIFIED FOR EXPORTING RESULTS -- #
    
    if(!is.null(file)){
      
      export_metrics(
        metrics = composite_sheets,
        digits = digits,
        name = name,
        file = file
      )
      
    }
    
    
    # -- RETURN -- #
    return(composite_sheets)
    
  }
  

  
}
