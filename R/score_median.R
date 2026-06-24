#' Calculate Median Composite Scores & Metrics
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#'
#' @details Median-based composite scores are designed for \strong{outlier
#' robustness}: indicators that deviate sharply from the respondent's typical
#' response pattern receive less weight, so a single extreme or inconsistent
#' item has limited influence on the composite. Three schemes are supported.
#'
#' \emph{Unweighted median.} The median composite score
#' \eqn{\bar{C}_c^{\mathrm{med}}} for case \eqn{c} is:
#'
#' \deqn{\bar{C}_c^{\mathrm{med}} = \mathrm{median}(I_{c1}, I_{c2}, \dots,
#' I_{cm})}
#'
#' where \eqn{I_{cj}} is the value of indicator \eqn{j} for case \eqn{c} and
#' \eqn{m} is the number of indicators. Missing items are excluded row-wise.
#'
#' \emph{Decay-weighted and Gaussian-weighted median.} For each respondent,
#' let \eqn{M_c} denote the within-row median (missing items excluded) and
#' \eqn{R_c} the within-row range of observed items:
#'
#' \deqn{M_c = \mathrm{median}(I_{c1}, \dots, I_{cm}), \quad
#' R_c = \max_j(I_{cj}) - \min_j(I_{cj})}
#'
#' Distances are scaled by \eqn{R_c} to make \code{decay_rate} and
#' \code{sigma} scale-invariant across different response formats (e.g., 1--5
#' Likert vs. 0--100 sliders):
#'
#' \deqn{\tilde{D}_{cj} = \frac{|I_{cj} - M_c|}{R_c}}
#'
#' For \code{"median_decay"}, the weight for indicator \eqn{j} is:
#'
#' \deqn{w_{cj} = \exp(-\gamma \cdot \tilde{D}_{cj})}
#'
#' where \eqn{\gamma} is the user-specified \code{decay_rate}. A value of 0.5
#' means an item at the full range from the median retains weight
#' \eqn{e^{-0.5} \approx 0.61}.
#'
#' For \code{"median_gauss"}, the Gaussian weight is:
#'
#' \deqn{w_{cj} = \exp\!\left(-\frac{\tilde{D}_{cj}^2}{2\sigma^2}\right)}
#'
#' where \eqn{\sigma} controls the spread relative to the item range. A value
#' of 0.5 means an item at half the range from the median retains weight
#' \eqn{e^{-0.5} \approx 0.61}.
#'
#' Weights are row-normalized with missing-item exclusion: items with missing
#' values have their weights zeroed out and the remaining weights are rescaled
#' to sum to 1 before computing:
#'
#' \deqn{\bar{C}_c = \sum_{j=1}^{m} I_{cj} \cdot \tilde{w}_{cj}}
#'
#' where \eqn{\tilde{w}_{cj} = w_{cj} / \sum_{k:\, I_{ck} \neq \mathrm{NA}} w_{ck}}.
#'
#' All median-based methods treat each case independently and handle missing
#' data via row-wise exclusion. Imputation is not required but may be applied
#' via \code{impute = TRUE} in \code{composite_score()} if desired.
#'
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param weight Required weighting schema. Schemas include \code{c("median",
#'   "median_decay", "median_gauss")}. Default is \code{"median"}.
#' @param decay_rate Numeric. Decay rate \eqn{\gamma} for \code{"median_decay"}.
#'   Distances are first scaled by the respondent's item range, so this
#'   parameter is scale-invariant: \eqn{\gamma = 0.5} means an item at the
#'   full observed range from the median retains weight \eqn{e^{-0.5} \approx
#'   0.61}. Larger values penalise outlying items more steeply. Default is
#'   \code{0.5}.
#' @param sigma Numeric. Bandwidth \eqn{\sigma} for \code{"median_gauss"}.
#'   Distances are scaled by the respondent's item range, so \eqn{\sigma} is
#'   expressed as a proportion of that range: \eqn{\sigma = 0.5} means an item
#'   at half the range from the median retains weight \eqn{e^{-0.5} \approx
#'   0.61}. Smaller values make the Gaussian narrower (stronger penalisation).
#'   Default is \code{0.5}.
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
#' # Calculate median composite scores
#' median_score(data = grit,
#'              composite_list = composite_list)
#'
#' # Calculate median composite scores, reliability, & validity
#' median_score(data = grit,
#'              composite_list = composite_list,
#'              digits = 3,
#'              return_metrics = TRUE,
#'              file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#' @export
median_score <- function(
    data = .,
    composite_list,
    weight = c("median", "median_decay", "median_gauss"),
    decay_rate = 0.5,
    sigma = 0.5,
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
    
    # -- RUN MEDIAN COMPOSITE SCORING FOR LOWER ORDER -- #
    
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
          
          calc_median_composite(
            data = data,
            var = var,
            weight = weight,
            decay_rate = decay_rate,
            sigma = sigma,
            digits = digits,
            name = NULL,
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
          
          calc_median_composite(
            data = data,
            var = var,
            weight = weight,
            decay_rate = decay_rate,
            sigma = sigma,
            digits = digits,
            name = NULL,
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
    
    # -- RUN MEDIAN COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each element of the lower order varlist
    lower_results <- lapply(
      names(lower_order_varlist),
      function(name) {
        
        var <- lower_order_varlist[[name]]
        
        if (length(var) == 1) {
          
          calc_single_indicator(
            data = data,
            var = var,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics
          )
          
        } else {
          
          calc_median_composite(
            data = data,
            var = var,
            weight = weight,
            decay_rate = decay_rate,
            sigma = sigma,
            digits = digits,
            name = name,
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
          
          calc_median_composite(
            data = data,
            var = var,
            weight = weight,
            decay_rate = decay_rate,
            sigma = sigma,
            digits = digits,
            name = name,
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
