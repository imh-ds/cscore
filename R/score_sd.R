#' Calculate Standard Deviation Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#'
#' @details Standard deviation (SD)–weighted composite scores assign indicator
#' weights based on their empirical variability. Two variants are supported:
#' \strong{upweighting} and \strong{downweighting} by standard deviation.
#'
#' Let \eqn{I_{cj}} denote the value of indicator \eqn{j} for case \eqn{c}. For
#' each indicator \eqn{j}, its sample standard deviation \eqn{\sigma_j} is
#' computed as:
#'
#' \deqn{\sigma_j = \sqrt{\frac{1}{n - 1} \sum_{i=1}^{n} (I_{ij} -
#' \bar{I}_{\cdot j})^2}}
#'
#' where \eqn{\bar{I}_{\cdot j}} is the sample mean of indicator \eqn{j} across
#' all \eqn{n} observations.
#'
#' \strong{1. SD-Upweighted Composite.}
#'
#' Indicators with greater dispersion are weighted more heavily. The weights are
#' normalized:
#'
#' \deqn{w_j = \frac{\sigma_j}{\frac{1}{m} \sum_{k=1}^{m} \sigma_k}}
#'
#' The final composite score for case \eqn{c} is:
#'
#' \deqn{\bar{C}_c^{(u)} = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_j}
#'
#' where \eqn{m} is the total number of indicators.
#'
#' \strong{2. SD-Downweighted Composite.}
#'
#' Indicators with greater dispersion are weighted less heavily by using the
#' inverse of the standard deviation:
#'
#' \deqn{w_j = \frac{\sigma_j^{-1}}{\frac{1}{m} \sum_{k=1}^{m} \sigma_k^{-1}}}
#'
#' The resulting downweighted composite score is:
#'
#' \deqn{\bar{C}_c^{(d)} = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_j}
#'
#' Both upweighted and downweighted schemes produce unit-normalized weights
#' (\eqn{\bar{w} = 1}). These can be used to emphasize or attenuate variability
#' in the indicators depending on the analytic goal.
#'
#' If \code{return_metrics = TRUE}, the function returns reliability and
#' validity metrics in addition to the composite score. See
#' \code{\link{calc_metrics}} for details.
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
            name = NULL,
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
            name = NULL,
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
