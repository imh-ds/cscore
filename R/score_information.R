#' Calculate Information-Weighted Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#' @details Composite scores are computed as the \strong{information-weighted}
#'   mean of the indicators.
#'
#'   \emph{Information-weighted scores.} For a given composite, pairwise mutual
#'   information (MI) is computed between all indicators to form an information
#'   matrix \eqn{I_{ij}}. Self-information values on the diagonal are excluded.
#'   Each mutual information value is then normalized to obtain the Normalized
#'   Mutual Information (NMI).
#'
#'   The NMI between variables \eqn{i} and \eqn{j} can be computed using either
#'   arithmetic mean:
#'
#'   \deqn{\mathrm{NMI}(i, j) = \frac{2 \cdot I(i, j)}{H(i) + H(j)}}
#'
#'   or geometric mean:
#'
#'   \deqn{\mathrm{NMI}(i, j) = \frac{I(i, j)}{\sqrt{H(i) \cdot H(j)}}}
#'
#'   where \eqn{I(i, j)} is the mutual information and \eqn{H(i)} and \eqn{H(j)}
#'   are the entropies of variables \eqn{i} and \eqn{j}, respectively. The
#'   method is selected via \code{nmi_method}. The information weight for
#'   indicator \eqn{j} is the average of its NMI values:
#'
#'   \deqn{w_j = \frac{1}{n} \sum_{i \neq j} \mathrm{NMI}(i, j)}
#'
#'   where \eqn{n} is the number of indicators and the sum excludes self-pairs.
#'   The weights are then normalized:
#'
#'   \deqn{w_j^{*} = \frac{w_j}{\frac{1}{m} \sum_{k=1}^{m} w_k}}
#'
#'   where \eqn{m} is the number of indicators in the composite. The
#'   information-weighted composite score \eqn{\bar{C}_{c}^{\mathrm{MI}}} for
#'   case \eqn{c} is calculated as:
#'
#'   \deqn{\bar{C}_{c}^{\mathrm{MI}} = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_j^{*}}
#'
#'   where \eqn{I_{cj}} is the value of indicator \eqn{j} for case \eqn{c}.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param entropy A string value reflecting the mutual information entropy
#'   estimator from the \code{infotheo} package. Four estimators are available:
#'   \code{emp} to compute the entropy of the empirical probability
#'   distribution. Empirical entropy is suitable for simple calculations without
#'   corrections. \code{mm} applies an asymptotic bias-corrected estimator
#'   making it suitable for small sample sizes. \code{shrink} applies a
#'   shrinkage estimate of the Dirichlet probability distribution to provide a
#'   stable estimate useful for small sample sizes or sparse data. \code{sg}
#'   applies a Schurmann-Grassberger estimate of the Dirichlet probability
#'   distribution to serve as an alternative to the Shrinkage approach.
#' @param nmi_method A string value reflecting the method used for calculating
#'   Normalized Mutual Information (NMI) values. \code{"average"} will normalize
#'   MI values using the average entropies of variables A and B.
#'   \code{"geometric"} will normalize MI values using the geometric mean of
#'   entropies of variables A and B.
#' @param threshold An integer specifying the maximum number of unique values to
#'   consider the input as discrete. Defaults to 10.
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
#' information_score(data = grit,
#'                   composite_list = composite_list)
#'
#' # Calculate correlation-weighted composite scores, reliability, & validity
#' information_score(data = grit,
#'                   composite_list = composite_list,
#'                   digits = 3,
#'                   return_metrics = TRUE,
#'                   file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#' @export
information_score <- function(
    data = .,
    composite_list,
    entropy = c("emp", "mm", "shrink", "sg"),
    nmi_method = c("geometric", "average"),
    threshold = 10,
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
){
  
  # -- MATCH ARGUMENTS -- #
  
  entropy <- match.arg(entropy)
  nmi_method <- match.arg(nmi_method)
  
  
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
          
          calc_mi_composite(
            data,
            var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
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
          
          calc_mi_composite(
            data,
            var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics)
          
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
          
          calc_mi_composite(
            data = data,
            var = var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
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
          
          calc_mi_composite(
            data = data,
            var = var,
            entropy = entropy,
            nmi_method = nmi_method,
            threshold = threshold,
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
