#' Calculate Median Composite Scores & Metrics
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#'
#' @details Composite scores can be calculated using three median-based
#' weighting schemes: \strong{unweighted median}, \strong{decay-weighted}, and
#' \strong{Gaussian-weighted}.
#'
#' \emph{Unweighted median.} The median composite score
#' \eqn{\bar{C}_c^{\mathrm{med}}} for case \eqn{c} is:
#'
#' \deqn{\bar{C}_c^{\mathrm{med}} = \mathrm{median}(I_{c1}, I_{c2}, \dots,
#' I_{cm})}
#'
#' where \eqn{I_{cj}} is the value of indicator \eqn{j} for case \eqn{c}, and
#' \eqn{m} is the number of indicators.
#'
#' \emph{Decay-weighted median.} For each respondent, let \eqn{M_c} denote the
#' within-row median:
#'
#' \deqn{M_c = \mathrm{median}(I_{c1}, I_{c2}, \dots, I_{cm})}
#'
#' The absolute distance to the median is:
#'
#' \deqn{D_{cj} = |I_{cj} - M_c|}
#'
#' The decay weight for indicator \eqn{j} is then:
#'
#' \deqn{w_{cj} = \exp(-\gamma \cdot D_{cj})}
#'
#' where \eqn{\gamma} is the user-specified \code{decay_rate}. Weights are
#' row-normalized:
#'
#' \deqn{w_{cj}^{*} = \frac{w_{cj}}{\frac{1}{m} \sum_{k=1}^{m} w_{ck}}}
#'
#' The decay-weighted median composite score is:
#'
#' \deqn{\bar{C}_c^{\mathrm{decay}} = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_{cj}^{*}}
#'
#' \emph{Gaussian-weighted median.} Using the same distance \eqn{D_{cj}}, the
#' Gaussian weight is:
#'
#' \deqn{w_{cj} = \exp\left(-\frac{D_{cj}^2}{2\sigma^2}\right)}
#'
#' where \eqn{\sigma} is the user-specified \code{sigma} parameter controlling
#' the spread. Weights are row-normalized as before, and the final score is:
#'
#' \deqn{\bar{C}_c^{\mathrm{gauss}} = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_{cj}^{*}}
#'
#' All median-based methods treat each case independently. Weighting is
#' performed per respondent, allowing differential emphasis on indicators based
#' on their proximity to the respondent-specific median.
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
#' @param decay_rate A numeric value reflecting the decay rate (i.e.,
#'   sensitivity) of the distance-to-median weighting schema. The default value
#'   is set to 0.5.
#' @param sigma A numeric value reflecting the sigma value for the Gaussian
#'   function in the distance-to-median weighting schema. The default value is
#'   set to 0.5.
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
