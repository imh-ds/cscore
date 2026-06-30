#' Calculate Standard Deviation Composite Scores
#'
#' @description Create lower- and higher-order composite scores from named sets
#'   of numeric indicators using weights derived from each indicator's sample
#'   standard deviation.
#'
#' @details
#'
#' Standard-deviation composite scoring is implemented by \code{sd_score()},
#' \code{calc_sd_composite()}, \code{safe_normalize()},
#' \code{weighted_row_mean()}, and \code{calc_metrics()}. The operative
#' algorithm is as follows.
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
#' \strong{2. Indicator-level standard deviations.}
#'
#' For a composite with indicator matrix \eqn{\mathbf{X} = [x_{cj}]} of
#' dimension \eqn{N \times m}, the helper function first computes one empirical
#' standard deviation per indicator using \code{stats::sd(x, na.rm = TRUE)}:
#'
#' \deqn{s_j = \sqrt{\frac{1}{n_j - 1}\sum_{c \in O_j}(x_{cj} - \bar{x}_j)^2}}
#'
#' where \eqn{O_j} is the set of non-missing observations for indicator
#' \eqn{j}, \eqn{n_j = |O_j|}, and \eqn{\bar{x}_j} is the mean of the observed
#' values for that indicator. Because \code{na.rm = TRUE} is used, the standard
#' deviations are computed from the observed values available for each column,
#' not from a listwise-complete subset of rows.
#'
#' \strong{3. Upweighting by empirical variability.}
#'
#' If \code{weight = "sd_upweight"}, the raw indicator weights are simply the
#' column standard deviations:
#'
#' \deqn{w_j = s_j}
#'
#' Indicators with larger empirical dispersion therefore receive larger raw
#' weights. If an indicator has zero variance, or if its standard deviation is
#' \code{NA} because there are too few observed values, the implementation
#' warns and replaces its raw weight by 0, effectively excluding it from the
#' weighted composite.
#'
#' \strong{4. Downweighting by empirical variability.}
#'
#' If \code{weight = "sd_downweight"}, the raw indicator weights are the
#' reciprocals of the column standard deviations:
#'
#' \deqn{w_j = \frac{1}{s_j}}
#'
#' Indicators with larger empirical dispersion therefore receive smaller raw
#' weights. This scheme requires every indicator to have strictly positive and
#' well-defined standard deviation. Accordingly, if any indicator has zero
#' variance or undefined SD, the implementation stops with an error rather than
#' attempting to divide by zero.
#'
#' \strong{5. Weight normalization and composite score.}
#'
#' After either raw weight vector is constructed, it is normalized with
#' \code{safe_normalize()}, which divides by the mean absolute weight:
#'
#' \deqn{w_j^{*} = \frac{w_j}{\frac{1}{m}\sum_{k=1}^{m}|w_k|}}
#'
#' This is the exact normalization used by the code. It makes the mean absolute
#' weight equal to 1 and provides a stable fallback to equal weights if the raw
#' vector is effectively zero.
#'
#' The composite score is then computed with \code{weighted_row_mean()} using
#' only the indicators observed for each case. If \eqn{O_c} is the set of
#' non-missing indicators for case \eqn{c}, then the implemented score is
#'
#' \deqn{C_c = \frac{\sum_{j \in O_c} x_{cj} w_j^{*}}{\sum_{j \in O_c}|w_j^{*}|}}
#'
#' If all indicators are missing for a case, \eqn{C_c} is \code{NA}. When all
#' weights are positive, as they are in ordinary SD-based use, this reduces to
#' the conventional weighted mean over the observed indicators.
#'
#' \strong{6. Interpretation.}
#'
#' The SD family assigns higher or lower influence to items solely on the basis
#' of marginal sample variability. It does not use the inter-item association
#' structure, latent-variable loadings, or respondent-specific coherence. As a
#' result, SD-based weighting is most appropriately interpreted as a dispersion-
#' based reweighting rule rather than as a measurement model. The upweighting
#' variant emphasizes indicators that differentiate respondents more strongly in
#' the observed sample, whereas the downweighting variant dampens indicators
#' whose observed dispersion is comparatively large.
#'
#' \strong{7. Reliability and validity metrics.}
#'
#' When \code{return_metrics = TRUE}, downstream metrics are computed by
#' \code{calc_metrics()} using the final normalized indicator-weight vector.
#' The reported indicator loadings are corrected item-total correlations. For
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
#' \pkg{ltm} is installed and otherwise returned as \code{NA}. As with the
#' other weighted families, the weighted composite reliability \eqn{\rho_c} is
#' the metric most directly aligned with the implemented weighting model.
#' 
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param weight Required weighting schema. Schemas include
#'   \code{c("sd_upweight", "sd_downweight")}. Default is \code{"sd_upweight"}.
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
#' sd_score(data = grit,
#'          composite_list = composite_list)
#'
#' # Calculate correlation-weighted composite scores, reliability, & validity
#' sd_score(data = grit,
#'          composite_list = composite_list,
#'          digits = 3,
#'          return_metrics = TRUE,
#'          file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#' @export
sd_score <- function(
    data = .,
    composite_list,
    weight = c("sd_upweight", "sd_downweight"),
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
) {
  
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
            name = name,
            digits = digits,
            return_metrics = return_metrics
          )
          
        } else {
          
          calc_sd_composite(
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
          
          calc_sd_composite(
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
          
          calc_sd_composite(
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
          
          calc_sd_composite(
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
