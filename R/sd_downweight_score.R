#' Calculate Standard Deviation Downweighted Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#' @details Standard Deviation upweighted composite scores are calculated by
#'   using the indicator's standard deviation as the weights. The indicators are
#'   multiplied by the inverse of their standard deviations normalized across
#'   all indicators. Standard deviations are therefore upweighted, i.e.,
#'   indicators with larger standard deviations are weighted higher than
#'   indicators with smaller standard deviations.
#'
#'   The inverse of the standard deviation (\eqn{\sigma}) is calculated:
#'
#' \deqn{\sigma_i = (\sqrt{\frac{1}{n-1} \sum_{i=1}^{n} (x_i - \bar{x})^2})^{-1}}{sd =
#' (sqrt((1 / (n - 1)) * sum((x[i] - mean(x))^2)))^{-1}}
#'
#'   The standard deviations are then normalized by dividing each \eqn{\sigma}
#'   by the mean of the \eqn{\sigma}:
#'
#' \deqn{\sigma[j] = \frac{\sigma[j]}{\frac{1}{m} \sum_{k=1}^{m} \sigma[k]}}{sigma[j] = sigma[j] / (1/m
#' * sum(sigma[k] for k=1 to m))}
#'
#' where \eqn{m} is the number of indicators, and the sum is taken over all
#' \eqn{k}. The SD-weighted composite score is then calculated as follows:
#'
#' \deqn{\bar{cs}_d = \frac{1}{n} \sum_{i=1}^{n} I[i] * w_i}{composite_score_d = 1/n
#' * sum(df[i] * w_i for i=1 to n)}
#'
#' where \eqn{\bar{cs}_d} is the SD downweighted composite score, \eqn{I} is the
#' raw indicator input, and \eqn{w_i} is the same value as the normalized
#' \eqn{\sigma_i}. If \code{return_metrics} is set to \code{TRUE}, the function
#' also returns composite reliability and validity metrics, as well as
#' indicator-level loadings and weights. See the documentation
#' \code{?calc_metrics} for the calculation and reporting of reliability and
#' validity measures.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
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
#' sd_downweight_score(data = grit,
#'                     composite_list = composite_list)
#'
#' # Calculate correlation-weighted composite scores, reliability, & validity
#' sd_downweight_score(data = grit,
#'                     composite_list = composite_list,
#'                     digits = 3,
#'                     return_metrics = TRUE,
#'                     file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#' @export
sd_downweight_score <- function(
    data = .,
    composite_list,
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
) {
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each variable of lower order composite list
    data[names(lower_order_varlist)] <- lapply(lower_order_varlist,
                                               function(var) {
                                                 
                                                 calc_sd_composite(data,
                                                                   var,
                                                                   weight = "sd_downweight",
                                                                   name = NULL,
                                                                   digits = 3,
                                                                   return_metrics = FALSE)
                                                 
                                               })
    
    # Update the data object
    data <- as.data.frame(data)
    
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each variable of higher order composite list
      data[names(higher_order_varlist)] <- lapply(higher_order_varlist,
                                                  function(var) {
                                                    
                                                    calc_sd_composite(data,
                                                                      var,
                                                                      weight = "sd_downweight",
                                                                      name = NULL,
                                                                      digits = 3,
                                                                      return_metrics = FALSE)
                                                    
                                                  })
      
    }
    
    
    # Return
    return(data)
    
  }
  
  
  
  # -- IF CALCULATING COMPOSITE SCORE & METRICS -- #
  if(return_metrics == TRUE){
    
    # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each element of the lower order varlist
    lower_results <- lapply(names(lower_order_varlist),
                            function(name) {
                              
                              var <- lower_order_varlist[[name]]
                              
                              calc_sd_composite(data = data,
                                                var = var,
                                                weight = "sd_downweight",
                                                name = name,
                                                digits = digits,
                                                return_metrics = TRUE)
                              
                            })
    
    
    # Extract the results
    lower_scores    <- sapply(lower_results, `[[`, "composite_score")
    lower_metrics   <- lapply(lower_results, `[[`, "composite_metrics")
    lower_validity  <- lapply(lower_results, `[[`, "composite_validity")
    
    # Bind the results together
    lower_metrics   <- do.call(rbind, lower_metrics)
    lower_validity  <- do.call(rbind, lower_validity)
    data[names(lower_order_varlist)] <- lower_scores
    
    # Update the data object
    data <- as.data.frame(data)
    
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each element of the higher order varlist
      higher_results <- lapply(names(higher_order_varlist),
                               function(name) {
                                 
                                 var <- higher_order_varlist[[name]]
                                 
                                 calc_sd_composite(data = data,
                                                   var = var,
                                                   weight = "sd_downweight",
                                                   name = name,
                                                   digits = digits,
                                                   return_metrics = TRUE)
                                 
                               })
      
      # Extract the results
      higher_scores    <- sapply(higher_results, `[[`, "composite_score")
      higher_metrics   <- lapply(higher_results, `[[`, "composite_metrics")
      higher_validity  <- lapply(higher_results, `[[`, "composite_validity")
      
      # Bind the results together
      higher_metrics   <- do.call(rbind, higher_metrics)
      higher_validity  <- do.call(rbind, higher_validity)
      data[names(higher_order_varlist)] <- higher_scores
      
    }
    
    
    # Combine into returnable list
    if(length(higher_order_varlist) > 0){
      
      metrics <- rbind(lower_metrics,
                       higher_metrics)
      
      validity <- rbind(lower_validity,
                        higher_validity)
      
      composite_sheets <- list(data = data,
                               metrics = metrics,
                               validity = validity)
      
    } else {
      
      metrics <- rbind(lower_metrics)
      
      validity <- rbind(lower_validity)
      
      composite_sheets <- list(data = data,
                               metrics = metrics,
                               validity = validity)
      
    }
    
    
    
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
