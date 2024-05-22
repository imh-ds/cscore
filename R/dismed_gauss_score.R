#' Calculate Distance-to-Median Composite Scores & Metrics with Gaussian Function
#' 
#' @description
#' 
#' Create composite scores of scales by specifying the indicators that go into
#' its respective composite variable. Composite scores are weighted based on the
#' indicators' distances to the median of all indicators. Indicators with
#' greater distance from the median (i.e., greater the absolute difference
#' between the median and the indicator) are downweighted based on a gaussian
#' function. Median composite scores are calculated as the median of the
#' indicators:
#' 
#' \deqn{M = median(I[1], I[2], ..., I[n])}{M = median(I[1], I[2], ..., I[n])}
#'
#' If we denote the \eqn{j^{th}} indicator (\eqn{I}) for the \eqn{i^{th}} respondent
#' as \eqn{I[ij]}, the distance-to-median calculation can be given as:
#'
#' \deqn{D[ij] = |I[ij] - M|}{D[ij] = |I[ij] - M|}
#' 
#' Distance-to-median gaussian function applies sigma as the constant to each
#' distance to calculate the weights. In a Gaussian function, weights decrease
#' in a bell curve (i.e., "Gaussian") shape as the distance from the median
#' increases. The rate of decrease is controlled by the \code{sigma} parameter.
#' A larger \code{sigma} value results in a wider bell curve, in which the
#' values further from the median retain their weighting on the composite
#' calculation. The Gaussian function therefore provides a smoother decrease of
#' weights with distance-to-median compared to the decay function. Values close
#' to the median (i.e., small distances) will have weights approaching 1,
#' whereas values further from the median (i.e., large distances) will have
#' weights approaching 0.
#'
#' The rate at which the weights decrease as the distance increases is
#' determined by the \code{sigma} value. A larger \code{sigma} results in a
#' faster decrease (i.e., weights will approach 0 more quickly), whereas a
#' smaller \code{sigma} results in a slower decrease (i.e., weights will stay
#' closer to 1 for larger distances). \code{sigma} therefore controls the
#' weighting schema's sensitivity to the distance from the median. A higher
#' \code{sigma} increases the influence of indicators closer to the median. A
#' lower \code{sigma} mutes the effect of distance to the median, thereby
#' placing more equal weight across all values regardless of their distance from
#' the median. Denoting sigma as \eqn{\sigma}, the weight calculation can be
#' given as:
#' 
#' \deqn{w[ij] = exp(-\frac{D[ij]^2}{2\sigma^2})}{w[ij] = exp(-(D[ij]^2) / (2 * sigma^2))}
#' 
#' Unlike covariance or standard deviation based weighting schemas, the median
#' weighting schemas are unique to each respondent since it is possible for
#' respondents to have different medians. The distance-to-median weights are
#' normalized by dividing each weight by the mean of the weights: 
#'
#' \deqn{w[j] = \frac{w[j]}{\frac{1}{m} \sum_{k=1}^{m} w[k]}}{w[j] = w[j] / (1/m
#' * sum(w[k] for k=1 to m))}
#' 
#' where \eqn{m} is the number of indicators, and the sum is taken over all
#' \eqn{k}. The distance-to-median composite score is then calculated as
#' follows:
#'
#' \deqn{\bar{cs}_{mg} = \frac{1}{n} \sum_{i=1}^{n} I[i] * w_i}{composite_score_mg = 1/n
#' * sum(df[i] * w_i for i=1 to n)}
#'
#' where \eqn{\bar{cs}_{mg}} is the gaussian distance-to-median weighted
#' composite score.
#' 
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
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
#' 
#' @return If \code{return_metrics = FALSE}, a dataframe identical to the input
#'  dataframe, with additional columns appended at the end, is returned. These
#'  new columns represent the calculated composite scores. If
#'  \code{return_metrics = TRUE}, a list containing the following dataframes is
#'  returned:
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
#'   perseverance_effort   = sprintf("gs%01d", c(1,4,6,9,10,12)),
#'
#'   # Higher-order composites
#'   grit                  = c("consistency_interest", "perseverance_effort")
#'
#'  )
#'                                    
#' # Calculate distance-to-median gaussian composite scores
#' dismed_gauss_score(data = grit,
#'                    composite_list = composite_list,
#'                    sigma = 0.5)
#'                             
#' # Calculate distance-to-median gaussian composite scores, reliability, & validity
#' dismed_gauss_score(data = grit,
#'                    composite_list = composite_list,
#'                    sigma = 0.5,
#'                    digits = 3,
#'                    return_metrics = TRUE,
#'                    file = "composite.xlsx")
#' 
#' unlink("composite.xlsx")
#' 
#' @export
dismed_gauss_score <- function(
    data = .,
    composite_list,
    sigma = 0.5,
    digits = 3,
    return_metrics = FALSE,
    file = NULL
) {
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    # -- RUN MEDIAN COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each variable of lower order composite list
    data[names(lower_order_varlist)] <- lapply(lower_order_varlist,
                                               function(var) {
                                                 
                                                 calc_median_composite(data,
                                                                       var,
                                                                       weight = "median_gauss",
                                                                       name = NULL,
                                                                       sigma = sigma,
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
                                                    
                                                    calc_median_composite(data,
                                                                          var,
                                                                          weight = "median_gauss",
                                                                          name = NULL,
                                                                          sigma = sigma,
                                                                          digits = 3,
                                                                          return_metrics = FALSE)
                                                    
                                                  })
      
    }
    
    
    # Return
    return(data)
    
  }
  
  
  
  # -- IF CALCULATING COMPOSITE SCORE & METRICS -- #
  if(return_metrics == TRUE){
    
    # -- RUN MEDIAN COMPOSITE SCORING FOR LOWER ORDER -- #
    
    # Apply the function to each element of the lower order varlist
    lower_results <- lapply(names(lower_order_varlist),
                            function(name) {
                              
                              var <- lower_order_varlist[[name]]
                              
                              calc_median_composite(data = data,
                                                    var = var,
                                                    weight = "median_gauss",
                                                    name = name,
                                                    sigma = sigma,
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
                                 
                                 calc_median_composite(data = data,
                                                       var = var,
                                                       weight = "median_gauss",
                                                       name = name,
                                                       sigma = sigma,
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
