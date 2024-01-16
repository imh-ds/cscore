#' DELETE: Calculate Regression-Weighted Metrics (Weights & Loadings)
#'
#' @description
#'
#' Create composite scores as well as the reliability and validity metrics of
#' the created constructs by specifying the indicators that go into each
#' respective composite variable.
#'
#' Indicator loadings are calculated as the bivariate correlation between the
#' indicator and the regression-weighted composite score. This is mathematically
#' represented as:
#'
#' \deqn{\lambda[i] = cor(I[i], \bar{cs}_r)}{\lambda[i] =
#' cor(indicator[i], composite_r)}
#'
#' where \eqn{I[i]} is the i-th indicator item, \eqn{\bar{cs}_r} is the
#' regression-weighted composite score, and \eqn{cor()} is the correlation
#' function.
#'
#' See documentation \code{?regression_score()} for the calculation and
#' normalization of regression weights.
#'
#' Two reliability metrics are also given: Cronbach's Alpha (\eqn{\alpha}) and
#' Composite Reliability (\eqn{\rho_c}). The calculation of reliability and
#' validity are consistent across different weighting schemas. See the
#' documentation \code{?correlation_metrics()} for the calculation and reporting
#' of reliability and validity measures.
#'
#' If \code{file} is specified with a file path, this function will
#' automatically write a formatted excel workbook of the returned output. The
#' format is consistent with what is typically reported in APA 7th edition, and
#' can be copy and pasted directly into a Word document or similar document
#' software.
#'
#' @usage regression_metrics( data = ., composite_list, digits = 3, file = NULL)
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the composite list represents a composite variable, and the corresponding
#'   vector  contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3.
#' @param file An optional file path. If specified, the results will be written
#'   as a formatted excel workbook.
#'
#' @return A list of dataframes consisting of:
#' \itemize{
#'  \item{\code{data}: }{The data with the composite variables appended as new
#'  variables.}
#'  \item{\code{metrics}: }{A matrix loadings and weights of the indicators and
#'  their corresponding composite variable.}
#'  \item{\code{validity}: }{A matrix of composite reliability and validity
#'  metrics.}
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
#'   perseverence_effort   = sprintf("gs%01d", c(1,4,6,9,10,12)),
#'
#'   # Higher-order composites
#'   grit                  = c("consistency_interest", "perseverence_effort")
#'
#'  )
#'
#' # Calculate correlation-weighted composite scores
#' regression_metrics(data = grit,
#'                    composite_list = composite_list,
#'                    digits = 3,
#'                    file = "composite_metrics.xlsx")
#'
#' @export
regression_metrics <- function(data = .,
                               composite_list,
                               digits = 3,
                               file = NULL){
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  
  
  # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
  
  # Apply the function to each element of the lower order varlist
  lower_results <- lapply(names(lower_order_varlist),
                          function(name) {
                            
                            var <- lower_order_varlist[[name]]
                            
                            calc_regression_metrics(data,
                                                    var,
                                                    name,
                                                    digits)
                            
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
                               
                               calc_regression_metrics(data,
                                                       var,
                                                       name,
                                                       digits)
                               
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
    
    export_metrics(composite_sheets,
                   digits,
                   file)
    
  }
  
  
  # -- RETURN -- #
  return(composite_sheets)
  
  
  
}
