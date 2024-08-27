#' Calculate Information-Weighted Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#' @details Composite scores are calculated as the information-weighted mean of
#'   the indicators.
#'
#'   Each indicator's information weight (\eqn{w}) is calculated as its average
#'   mutual information with all other indicators. An information matrix is
#'   first computed using all indicators of the composite variable to obtain all
#'   possible combinations of pairwise mutual information, such that the
#'   information matrix is calculated where the entry in the i-th row and j-th
#'   column is the mutual information between the i-th and j-th indicators. The
#'   diagonal of the information matrix (i.e., mutual information of i-th
#'   indicator to itself) are set to \code{NA} to remove perfect information.
#'   Further, all mutual information values are normalized to obtain the
#'   Normalized Mutual Information (NMI) value. The NMI can be calculated in two
#' ways in the function using the average entropy (via \code{nmi_method =
#' "average"}) and geometric mean of entropies (via \code{nmi_method =
#' "geometric"}). Using the average entropies of \eqn{i} and \eqn{j}, the NMI is
#'   calculated as:
#'
#'   \deqn{\text{NMI}(i; j) = \frac{2 \cdot I(i; j)}{H(i) + H(j)}}
#'
#'   whereas using the geometric means of \eqn{i} and \eqn{j}, the NMI is
#'   calculated as:
#'
#'   \deqn{\text{NMI}(i; j) = \frac{I(i; j)}{\sqrt{H(i) \cdot H(j)}}}
#'
#'   where \eqn{I(i; j)} represents the mutual information between variables
#'   \eqn{i} and \eqn{j}, and \eqn{H(i)} and \eqn{H(j)} represent the entropies
#'   of variables \eqn{i} and \eqn{j}, respectively. Indicator weight
#'   calculation is then mathematically represented as:
#'
#' \deqn{w[j] = \frac{1}{n} \sum_{i=1}^{n} mi\_matrix[i, j]}{w[j] = 1/n *
#' sum(inf_matrix[i, j] for i=1 to n)}
#'
#'   where \eqn{n} is the number of rows in \eqn{mi\_matrix}, and the sum is
#'   taken over all \eqn{i} such that \eqn{mi\_matrix[i, j]} is not \code{NA}.
#'   The information weights are then normalized by dividing each weight by the
#'   mean of the weights:
#'
#' \deqn{w[j] = \frac{w[j]}{\frac{1}{m} \sum_{k=1}^{m} w[k]}}{w[j] = w[j] / (1/m
#' * sum(w[k] for k=1 to m))}
#'
#' where \eqn{m} is the number of columns in \eqn{inf\_matrix}, and the sum is
#' taken over all \eqn{k}. The information-weighted composite score is then
#' calculated as follows:
#'
#' \deqn{\bar{cs}_{mi} = \frac{1}{n} \sum_{i=1}^{n} I[i] * w_i}{composite_score_{mi} = 1/n
#' * sum(df[i] * w_i for i=1 to n)}
#'
#' where \eqn{\bar{cs}_{mi}} is the correlation weighted composite score and
#' \eqn{I} is the raw indicator input. If \code{return_metrics} is set to
#' \code{TRUE}, the function also returns composite reliability and
#' validity metrics, as well as indicator-level loadings and weights. See the
#' documentation \code{?calc_metrics} for the calculation and reporting of
#' reliability and validity measures.
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
#'  variables.}
#'  \item \strong{Metrics}: A matrix of indicator loadings and weights metrics.}
#'  \item \strong{Validity}: A matrix of composite reliability and validity
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
    entropy = "emp",
    nmi_method = "geometric",
    digits = 3,
    return_metrics = FALSE,
    file = NULL,
    name = NULL
){
  
  
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
                                                 
                                                 calc_mi_composite(data,
                                                                   var,
                                                                   entropy = entropy,
                                                                   nmi_method = nmi_method,
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
                                                    
                                                    calc_mi_composite(data,
                                                                      var,
                                                                      entropy = entropy,
                                                                      nmi_method = nmi_method,
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
                              
                              calc_mi_composite(data = data,
                                                var = var,
                                                entropy = entropy,
                                                nmi_method = nmi_method,
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
                                 
                                 calc_mi_composite(data = data,
                                                   var = var,
                                                   entropy = entropy,
                                                   nmi_method = nmi_method,
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
