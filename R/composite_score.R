#' Calculate Composite Scores
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable. See help documents
#'   \code{average_score}, \code{correlation_score}, \code{discriminant_score},
#'   \code{information_score}, \code{median_score}, or \code{sd_score} for
#'   information on how the composite scores for each weighting scheme are
#'   calculated.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param composite_model An optional \code{composite_model} object. Combines
#'   one or more sets of `link` paths specifying directed associations between
#'   variables. The model is assumed to be a directed acyclic graph.
#' @param weight The weighting schema used to calculate composite scores. Choose
#'   from the following families:
#'
#'   \strong{Covariance Family:}
#' \itemize{
#'   \item \code{"average"}: Unweighted average (straight mean).
#'   \item \code{"correlation"}: Correlation-weighted composite scores.
#'   \item \code{"regression"}: Regression-weighted composite scores.
#' }
#'   \strong{Standard Deviation (SD) Family:}
#' \itemize{
#'   \item \code{"sd_upweight"}: Upweights indicators with higher standard deviation.
#'   \item \code{"sd_downweight"}: Downweights indicators with higher standard deviation.
#' }
#'   \strong{Median Family:}
#' \itemize{
#'   \item \code{"median"}: Uses the median of the indicators as the composite score.
#'   \item \code{"median_decay"}: Applies a decay-weighted distance from the median.
#'   \item \code{"median_gauss"}: Applies a Gaussian-weighted distance from the median.
#' }
#'   \strong{Information Family:}
#' \itemize{
#'   \item \code{"mutual_info"}: Shared mutual information-weighted composite scores.
#' }
#'   \strong{Discriminant Family:}
#' \itemize{
#'   \item \code{"irt"}: Item response theory discriminant parameter-weighted composite scores.
#'   \item \code{c("pca", "glm")}: Principal component analysis and generalized linear model weighted composite scores.
#' }
#' @param digits Integer. The decimal places for the metrics to be rounded to
#'   when returning metrics. Default is 3.
#' @param decay_rate Numeric. Reflects the decay rate (i.e., sensitivity) of the
#'   distance-to-median weighting schema. The default value is set to 0.5.
#' @param sigma Numeric. Reflects the sigma value for the Gaussian function in
#'   the distance-to-median weighting schema. The default value is set to 0.5.
#' @param entropy String. Reflects the mutual information entropy estimator from
#'   the \code{infotheo} package. Four estimators are available: \code{emp} to
#'   compute the entropy of the empirical probability distribution. Empirical
#'   entropy is suitable for simple calculations without corrections. \code{mm}
#'   applies an asymptotic bias-corrected estimator making it suitable for small
#'   sample sizes. \code{shrink} applies a shrinkage estimate of the Dirichlet
#'   probability distribution to provide a stable estimate useful for small
#'   sample sizes or sparse data. \code{sg} applies a Schurmann-Grassberger
#'   estimate of the Dirichlet probability distribution to serve as an
#'   alternative to the Shrinkage approach.
#' @param nmi_method String. Reflects the method used for calculating Normalized
#'   Mutual Information (NMI) values. \code{"average"} will normalize MI values
#'   using the average entropies of variables A and B. \code{"geometric"} will
#'   normalize MI values using the geometric mean of entropies of variables A
#'   and B.
#' @param threshold Integer. Specifies the maximum number of unique values to
#'   consider the input as discrete. Defaults to 10.
#' @param pred_type Prediction method schema for predictive weighting. Schemas
#'   include \code{c("glm", "rf")}. \code{"glm"} runs \code{glmnet::cv.glmnet()}
#'   for regularization linear regression. \code{"rf"} runs
#'   \code{ranger::ranger()} for Random Forest modeling. Default is
#'   \code{"glm"}.
#' @param item_type String. Specifies the item response model(s) to be used,
#'   passed to the \code{itemtype} argument in \code{mirt::mirt()}. See
#'   \code{?mirt::mirt} for the full list of supported item types and their
#'   definitions. Defaults to \code{NULL}.
#' @param pmm_k Integer. Number of donor candidates used for predictive mean
#'   matching (PMM) during imputation. If set to \code{0}, PMM is disabled and
#'   raw predictions are used.
#' @param maxiter Integer. Maximum number of iterations for chained equations in
#'   \code{missRanger::missRanger()}. Iteration continues until convergence or
#'   the specified limit is reached.
#' @param digits Integer. The decimal places for the metrics to be rounded to.
#'   Default is 3. This argument is only relevant if \code{return_metrics =
#'   TRUE}.
#' @param alpha Numeric. Elastic net mixing parameter, with \code{0 <= alpha <=
#'   1}. Controls the relative weighting of L1 (lasso) and L2 (ridge) penalties
#'   in the model. The penalty term is defined as \code{(1 - alpha)/2 * sum(beta^2)
#'   + alpha * sum(abs(beta))}, where \code{beta} is the vector of coefficients.
#'   When \code{alpha = 1}, the penalty is pure lasso; when \code{alpha = 0}, it
#'   is ridge regression.
#' @param nfolds Integer. The number of folds used for random forest k-fold
#'   cross-validation. Controls how the data are partitioned during resampling.
#'   Must be at least 2. Default is 10.
#' @param ntrees Integer. Number of trees to grow in the random forest model.
#'   Default in \code{ranger} is 500.
#' @param importance Character string. Specifies the type of variable importance
#'   measure to compute. Must be one of \code{"permutation"} or
#'   \code{"impurity"}.
#'   \itemize{
#'     \item \code{"permutation"}: Computes mean decrease in predictive accuracy by permuting each variable and measuring the resulting drop in model performance (typically using out-of-bag data). This method is more computationally expensive but generally provides more reliable and differentiated estimates of variable importance. It is robust to scale and less biased in the presence of correlated or irrelevant predictors.
#'     \item \code{"impurity"}: Computes mean decrease in node impurity (e.g., Gini for classification, variance for regression) aggregated over all trees. This method is fast and computed during model training, but can be biased toward variables with many categories or continuous values, and tends to distribute importance more evenly across predictors—even when some are irrelevant or redundant.
#'   }
#'   Recommend using \code{"impurity"} for speed and \code{"permutation"} for
#'   interpretability, reliability, or feature selection. Default is
#'   \code{"permutation"}.
#' @param family Character string. Specifies the model family. Must be one of
#'   \code{"gaussian"}, \code{"binomial"}, \code{"multinomial"}, or
#'   \code{"poisson"}. This argument determines the loss function used in
#'   penalized model fitting via \code{cv.glmnet}. The supported options are:
#'   \itemize{
#'     \item \code{"gaussian"}: for linear regression with continuous outcomes.
#'     \item \code{"binomial"}: for binary classification using logistic regression. The response should be a factor with two levels or a numeric vector containing 0 and 1.
#'     \item \code{"multinomial"}: for multi-class classification with three or more unordered outcome categories. The response should be a factor.
#'     \item \code{"poisson"}: for modeling count data under a Poisson distribution with a log link. The response should be a non-negative count-valued numeric vector.
#'   }
#'   Default is \code{"gaussian"}.
#' @param return_metrics Logic. Determines whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' @param file An optional file path. If specified, the results will be written
#'   as a formatted excel workbook. This argument is only relevant if
#'   \code{return_metrics = TRUE}.
#' @param name An optional string denoting the name of the study/analysis.
#' @param verbose Logical. If \code{1}, print progress messages and intermediate
#'   output. Defaults to \code{0}.
#' @param seed Integer. Seed for reproducibility. Defaults to \code{NULL}, where
#'   no seed is set.
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
#' # Calculate unweighted composite scores
#' composite_score(data = grit,
#'                 composite_list = composite_list,
#'                 weight = "average")
#'
#'
#' @export
composite_score <- function(
    data = .,
    composite_list,
    composite_model = NULL,
    weight = c("correlation", "regression", "average",
               "sd_upweight", "sd_downweight",
               "median", "median_decay", "median_gauss",
               "mutual_info",
               "irt", "glm", "pca"),
    decay_rate = 0.5,
    sigma = 0.5,
    entropy = c("emp", "mm", "shrink", "sg"),
    nmi_method = c("geometric", "average"),
    threshold = 10,
    pred_type = c("glm", "rf"),
    item_type = NULL,
    pmm_k = 5,
    maxiter = 10,
    verbose = 0,
    alpha = 0.5,
    nfolds = 10,
    ntrees = 100,
    importance = c("permutation", "impurity"),
    family = c("gaussian", "binomial", "multinomial", "poisson"),
    return_metrics = FALSE,
    digits = 3,
    file = NULL,
    name = NULL,
    seed = NULL
){
  

  # MATCH ARGUMENTS ---------------------------------------------------------

  pred_type <- match.arg(pred_type)
  weight <- match.arg(weight)
  entropy <- match.arg(entropy)
  nmi_method <- match.arg(nmi_method)
  importance <- match.arg(importance)
  family <- match.arg(family)
    

  # COVARIANCE FAMILY WEIGHTING ---------------------------------------------
  
  # -- AVERAGE UNWEIGHTED -- #
  if(weight == "average"){
    
    cs <- average_score(
      data = data,
      composite_list = composite_list,
      digits = digits,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  
  
  # -- CORRELATION-WEIGHTED or REGRESSION-WEIGHTED -- #  
  
  if(weight %in% c("correlation", "regression")){
    
    cs <- correlation_score(
      data = data,
      composite_list = composite_list,
      weight = weight,
      digits = digits,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  

  # STANDARD DEVIATION FAMILY WEIGHTING -------------------------------------
  
  if(weight %in% c("sd_upweight", "sd_downweight")){
    
    cs <- sd_score(
      data = data,
      composite_list = composite_list,
      weight = weight,
      digits = digits,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  

  # MEDIAN FAMILY WEIGHTING -------------------------------------------------
  
  # -- MEDIAN SCORE -- #
  if(weight %in% c("median", "median_decay", "median_gauss")){
    
    cs <- median_score(
      data = data,
      composite_list = composite_list,
      weight = weight,
      decay_rate = decay_rate,
      sigma = sigma,
      digits = digits,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  

  # INFORMATION FAMILY WEIGHTING --------------------------------------------
  
  if(weight == "mutual_info"){
    
    cs <- information_score(
      data = data,
      composite_list = composite_list,
      entropy = entropy,
      nmi_method = nmi_method,
      threshold = threshold,
      digits = digits,
      return_metrics = return_metrics,
      file = file,
      name = name
    )
    
  }
  

  # DISCRIMINANT FAMILY WEIGHTING -------------------------------------------

  # -- ITEM RESPONSE THEORY SCORE -- #
  if(weight %in% c("irt", "glm", "pca")){
    
    cs <- discriminant_score(
      data = data,
      composite_list = composite_list,
      composite_model = composite_model,
      weight = weight,
      pred_type = pred_type,
      item_type = item_type,
      pmm_k = pmm_k,
      ntrees = ntrees,
      nfolds = nfolds,
      maxiter = maxiter,
      seed = seed,
      digits = digits,
      alpha = alpha,
      return_metrics = return_metrics,
      file = file,
      name = name,
      verbose = verbose
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