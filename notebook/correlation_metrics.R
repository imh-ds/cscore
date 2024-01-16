#' DELETE: Calculate Correlation-Weighted Metrics (Reliability & Validity)
#'
#' @description
#'
#' Create composite scores as well as the reliability and validity metrics of
#' the created constructs by specifying the indicators that go into each
#' respective composite variable.
#'
#' Indicator loadings are calculated as the bivariate correlation between the
#' indicator and the correlation-weighted composite score. This is
#' mathematically represented as:
#'
#' \deqn{\lambda[i] = cor(I[i], \bar{cs}_c)}{\lambda[i] =
#' cor(I[i], composite_c)}
#'
#' where \eqn{I[i]} is the i-th indicator item, \eqn{\bar{cs}_c} is the
#' correlation-weighted composite score, and \eqn{cor()} is the correlation
#' function.
#'
#' See documentation \code{?correlation_score()} for the calculation and
#' normalization of correlation weights.
#'
#' Two reliability metrics are also given: Cronbach's Alpha (\eqn{\alpha}) and
#' Composite Reliability (\eqn{\rho_c}). Cronbach's Alpha measures construct
#' reliability based on the covariance among the indicators.
#'
#' \deqn{\alpha = \frac{k \bar{c}}{\bar{v} + (k - 1) \bar{c}}}{alpha = k *
#' mean(c) / (mean(v) + (k - 1) * mean(c))}
#'
#' where \eqn{\bar{v}} is the average variance and \eqn{\bar{c}} is the average
#' inter-item covariance. Cronbach's Alpha is a popular approach to assessing
#' construct reliability and is the most widely known reliability measure in the
#' social and behavioral sciences. However, it has several psychometric
#' problems. Firstly, Alpha assumes \emph{tau-equivalence}, i.e., that all
#' indicators contribute equally to the construction of the composite and have
#' uniform indicator loadings. This can be forced by the researcher when
#' constructing the composite using sum-averages, but often does not reflect the
#' reality of psychometric measures. Secondly, Alpha is biased based on the
#' number of indicators. In other words, Alpha can be misleadingly good even
#' when the measure is poor if there are lots of indicators, and likewise can be
#' misleadingly bad even when the measure is good if there are only a few
#' indicators. This is due to the number of indicators (i.e., \eqn{k}) being in
#' the numerator. For these reasons, Cronbach's Alpha is calculated and provided
#' in the output for when researchers may need the value for whatever reason.
#' However, the recommendation is to not use Alpha.
#'
#' An alternative reliability measurement, McDonald's Omega (\eqn{\omega}) has
#' been gaining popularity due to its assumption of multidimensionality within
#' the data. However, because Omega is computing its own factor structure to
#' determine multidimensionality, it can often result in calculating reliability
#' based on a different factor model than the researcher's \emph{a priori}
#' structure. This results in a reliability calculation of a different model
#' than one that is used in analysis. One way to address this is to manually fit
#' a researcher-defined factor model (e.g., \code{psych::omegaFromSem} offers a
#' way to fit a \code{lavaan} object). However, popular softwares do not always
#' offer this feature (e.g., JASP, jamovi), resulting in some barriers to
#' adoption. From a practical perspective, the difference in reliability from
#' this issue is often miniscule and a non-issue. Nonetheless, systematic
#' misrepresentation can yield problems in the future. For this reason, this
#' package calculates Composite Reliability (\eqn{\rho_c}) and fits it the
#' defined user-defined measurement structure. Further, because of the weighted
#' nature of the composite calculation in this package, the indicator loadings
#' are appropriately weighted in \eqn{\rho_c} as follows:
#'
#' \deqn{\rho_c = \frac{(\sum \lambda[i]*w[i])^2}{(\sum \lambda[i]*w[i])^2 +
#' \sum e_w[i]}}{rho_c = (sum(\lambda[i]*w[i])^2) / ((sum(\lambda[i]*w[i])^2) +
#' sum(e_w[i]))}
#'
#' Although the reliability is named Composite Reliability to keep consistent
#' with naming conventions of the Partial Least Squares Structural Equation
#' Modeling (PLS-SEM) literature, the underlying formula is identical to
#' McDonald's Omega. Researchers can report this metric as either \eqn{\rho_c}
#' or \eqn{\omega}.
#'
#' Lastly, the Average Variance Extracted (AVE) is calculated and given in the
#' output. AVE is often used to reflect how much the indicators capture the
#' composite variable's variance (vs the variance being due to measurement
#' error). AVE is frequently used to determine discriminant validity based on
#' the Fornell-Larcker criterion, i.e., that the square root of the AVE is
#' greater than the highest bivariate correlation with any other composite
#' variable:
#'
#' \deqn{\sqrt{AVE_i} > \max_{j}(r_{ij})}
#'
#' The Average Variance Extracted (AVE) is calculated as: \deqn{AVE = \frac{\sum
#' \lambda^2_w[i]}{\sum \lambda^2_w[i] + \sum e_w[i]}}{ave = sum(lambda^2_w[i])
#' / (sum(lambda^2_w[i]) + sum(e_w[i]))}
#'
#' In which the weighted loadings squared (\eqn{\lambda^2_w}) is calculated as:
#' \deqn{\lambda^2_w[i] = (\lambda[i]^2) * w[i]}{\lambda^2_w[i] = (\lambda[i]^2)
#' * w[i]}
#'
#' And weighted errors (\eqn{e_w}) are calculated as: \deqn{e_w[i] = (1 -
#' \lambda[i]^2) * w[i]}{e_w[i] = (1 - lambda[i]^2) * w[i]}
#'
#'
#' If \code{file} is specified with a file path, this function will
#' automatically write a formatted excel workbook of the returned output. The
#' format is consistent with what is typically reported in APA 7th edition, and
#' can be copy and pasted directly into a Word document or similar document
#' software.
#'
#'
#' @usage correlation_metrics( data = ., composite_list, digits = 3, file = NULL
#'   )
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
#' # Load in data
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
#' correlation_metrics(data = grit,
#'                     composite_list = composite_list,
#'                     digits = 3,
#'                     file = "composite_metrics.xlsx")
#'
#' @export
correlation_metrics <- function(data = .,
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
                            
                            calc_correlation_metrics(data,
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
                               
                               calc_correlation_metrics(data,
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
