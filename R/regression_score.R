#' Calculate Regression-Weighted Composite Scores
#'
#' @description
#'
#' Create composite scores of scales by specifying the indicators that go into
#' each respective composite variable. Regression-weighted composite scores are
#' calculated by fitting a linear model where the correlation-weighted composite
#' scores are regressed on the indicators. The indicators are multiplied by
#' their respective regression weights to calculate the predicted values in the
#' linear model, which represent the regression-weighted composite scores.
#'
#' The regression weights (\eqn{w}) are calculated by fitting a linear model
#' with the correlation-weighted composite score (\eqn{\bar{cs}_c}) as the
#' outcome and the indicators (\eqn{I_{1...n}}) as the predictors (see help
#' document for \code{?correlation_score} for information on the calculation of
#' the correlation-weighted composite score). The standardized coefficients
#' (\eqn{\beta_{1...n}}) from this model are used as the weights:
#'
#' \deqn{\bar{cs}_c = \beta_0 + \beta_1I_1 + \beta_2I_2 + ... + \epsilon}{cs_c =
#' beta_0 + beta_1I_1 + beta_2I_2 + ... + error}
#'
#' The regression weights are simply the normalized beta coefficient:
#'
#' \deqn{w_i = \frac{\beta_i}{\frac{1}{m}
#' \sum_{k=1}^{m} \beta_k}}{beta_i = beta_i / (1/m * sum(beta_k) for k=1 to m)}
#'
#' where \eqn{m} is the number of indicators, and the sum is taken over all
#' \eqn{k}. Finally, the regression-weighted composite score is calculated as
#' follows:
#'
#' \deqn{\bar{cs}_r = \frac{1}{n} \sum_{i=1}^{n} I[i] *
#' w_i}{composite_score = 1/n * sum(I[i] * w_i) for i=1 to n)}
#'
#' where \eqn{\bar{cs}_r} is the regression weighted composite score. If
#' \code{return_metrics} is set to \code{TRUE}, the function also returns
#' composite reliability and validity metrics, as well as indicator-level
#' loadings and weights. See the documentation \code{?calc_metrics} for
#' the calculation and reporting of reliability and validity measures.
#'
#' If \code{file} is specified with a file path, this function will
#' automatically write a formatted excel workbook of the returned output. The
#' format is consistent with what is typically reported in APA 7th edition, and
#' can be copy and pasted directly into a Word document or similar document
#' software.
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
#'
#' @return If \code{return_metrics = FALSE}, a dataframe identical to the input
#'   dataframe, with additional columns appended at the end, is returned. These
#'   new columns represent the calculated composite scores. If
#'   \code{return_metrics = TRUE}, a list containing the following dataframes is
#'   returned:
#'  \itemize{
#'  \item{\strong{Data}: }{A dataframe with the composite variables appended as new
#'  variables.}
#'  \item{\strong{Metrics}: }{A matrix of indicator loadings and weights metrics.}
#'  \item{\strong{Validity}: }{A matrix of composite reliability and validity
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
#' # Calculate regression-weighted composite scores
#' regression_score(data = grit,
#'                  composite_list = composite_list)
#'                                      
#' # Calculate regression-weighted composite scores, reliability, & validity
#' regression_score(data = grit,
#'                  composite_list = composite_list,
#'                  digits = 3,
#'                  return_metrics = TRUE,
#'                  file = "composite.xlsx")
#' 
#' unlink("composite.xlsx")
#' 
#' @export
regression_score <- function(data = .,
                             composite_list,
                             digits = 3,
                             return_metrics = FALSE,
                             file = NULL){
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    # -- RUN CORRELATION WEIGHTED COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each variable of lower order composite list
    lower_results <- lapply(names(lower_order_varlist),
                            function(name) {
                              
                              var <- lower_order_varlist[[name]]
                              
                              calc_cov_composite(data,
                                                 var,
                                                 weight = "regression",
                                                 name = name,
                                                 digits = 3,
                                                 return_metrics = FALSE)
                              
                            })
    
    # Update the data object
    lower_scores <- sapply(lower_results, rbind)
    
    data[names(lower_order_varlist)] <- lower_scores
    
    data <- as.data.frame(data)
    
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each variable of higher order composite list
      higher_results <- lapply(names(higher_order_varlist),
                               function(name) {
                                 
                                 var <- higher_order_varlist[[name]]
                                 
                                 calc_cov_composite(data,
                                                    var,
                                                    weight = "regression",
                                                    name = name,
                                                    digits = 3,
                                                    return_metrics = FALSE)
                                 
                               })
      
      # Update the data object
      higher_scores <- sapply(higher_results, rbind)
      
      data[names(higher_order_varlist)] <- higher_scores
      
      data <- as.data.frame(data)
      
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
                              
                              calc_cov_composite(data = data,
                                                 var = var,
                                                 weight = "regression",
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
                                 
                                 calc_cov_composite(data = data,
                                                    var = var,
                                                    weight = "regression",
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
      
      export_metrics(composite_sheets,
                     digits,
                     file)
      
    }
    
    
    # -- RETURN -- #
    return(composite_sheets)
    
  }
  
  
  
}
