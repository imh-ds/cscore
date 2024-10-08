#' Calculate Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable. See help documents
#'   `?average_score`, `?regression_score`, and `?correlation_score` for
#'   information on how the composite scores for each weighting scheme are
#'   calculated.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param weight The weighting schema to use in calculating composite scores.
#'   For a weighting schema within the Covariance Family, set \code{weight} to
#'   \code{"average"} for unweighted, \code{"correlation"} for
#'   correlation-weighted, and \code{"regression"} for regression-weighted. For
#'   a weighting schema within the Standard Deviation (SD) Family, set
#'   \code{weight} to \code{"sd_upweight"} to upweight SDs and
#'   \code{"sd_downweight"} to downweight SDs. For a weighting schema within the
#'   Median Family, set \code{weight} to \code{"median"} to calculate the
#'   composite score as the median score, \code{"median_decay"} for
#'   distance-to-median weighting with the decay function, and
#'   \code{"median_gauss"} for distance-to-median weighting with the gaussian
#'   function.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param decay_rate A numeric value reflecting the decay rate (i.e.,
#'   sensitivity) of the distance-to-median weighting schema. The default value
#'   is set to 0.5. This argument is only relevant if \code{weight =
#'   "median_decay"}.
#' @param sigma A numeric value reflecting the sigma value for the Gaussian
#'   function in the distance-to-median weighting schema. The default value is
#'   set to 0.5. This argument is only relevant if \code{weight =
#'   "median_gauss"}.
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
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' @param file An optional file path. If specified, the results will be written
#'   as a formatted excel workbook. This argument is only relevant if
#'   \code{return_metrics = TRUE}.
#' @param name An optional string denoting the name of the study/analysis.
#'
#' @importFrom magrittr %>%
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
#' # FOR COVARIANCE FAMILY
#'
#' # Calculate unweighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "average")
#'
#' # Calculate correlation-weighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "correlation")
#'
#' # Calculate regression-weighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "regression")
#'
#'
#' # FOR STANDARD DEVIATION FAMILY
#'
#' # Calculate SD-upweighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "sd_upweight")
#'
#' # Calculate SD-downweighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "sd_downweight")
#'
#'
#' # FOR MEDIAN FAMILY
#'
#' # Calculate median composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "median")
#'
#' # Calculate distance-to-median decay function composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "median_decay")
#'
#' # Calculate distance-to-median gaussian function composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "median_gauss")
#'
#'
#'
#' # FOR INFORMATION FAMILY
#'
#' # Calculate median composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "mutual_info")
#'
#'
#' @export
composite_score <- function(
    data = .,
    composite_list,
    weight = "correlation",
    digits = 3,
    decay_rate = 0.5,
    sigma = 0.5,
    entropy = "emp",
    nmi_method = "geometric",
    return_metrics = FALSE,
    file = NULL,
    name = NULL
){
  

  # COVARIANCE FAMILY WEIGHTING ---------------------------------------------
  
  # -- AVERAGE UNWEIGHTED -- #
  if(weight == "average"){
    
    cs <- average_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- CORRELATION-WEIGHTED -- #  
  
  if(weight == "correlation"){
    
    cs <- correlation_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- REGRESSION-WEIGHTED -- #  
  
  if(weight == "regression"){
    
    cs <- regression_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  

  # STANDARD DEVIATION FAMILY WEIGHTING -------------------------------------
  
  # -- STANDARD DEVIATION UPWEIGHT -- #
  if(weight == "sd_upweight"){
    
    cs <- sd_upweight_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- STANDARD DEVIATION DOWNWEIGHT -- #
  if(weight == "sd_downweight"){
    
    cs <- sd_downweight_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  

  # MEDIAN FAMILY WEIGHTING -------------------------------------------------
  
  # -- MEDIAN SCORE -- #
  if(weight == "median"){
    
    cs <- median_score(
      data = data,
      composite_list = composite_list,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- DISTANCE-TO-MEDIAN DECAY FUNCTION -- #
  if(weight == "median_decay"){
    
    cs <- dismed_decay_score(
      data = data,
      composite_list = composite_list,
      decay_rate = decay_rate,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- DISTANCE-TO-MEDIAN GAUSSIAN FUNCTION -- #
  if(weight == "median_gauss"){
    
    cs <- dismed_gauss_score(
      data = data,
      composite_list = composite_list,
      sigma = sigma,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  

  # INFORMATION FAMILY WEIGHTING --------------------------------------------
  
  # -- MUTUAL INFORMATION SCORE -- #
  if(weight == "mutual_info"){
    
    cs <- information_score(
      data = data,
      composite_list = composite_list,
      entropy = entropy,
      nmi_method = nmi_method,
      digits = 3,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  # Return
  return(cs)
  
}


#' @rdname composite_score
#' @export
cscore <- composite_score

#' @rdname composite_score
#' @export
cs <- composite_score