#' Calculate Discriminant-Weighted Composite Scores
#'
#'
#' @description Create lower- and higher-order composite scores from named sets
#'   of numeric indicators using discrimination-based weights derived either
#'   from a unidimensional latent-variable model or from the first principal
#'   component, with an optional predictive reweighting stage based on external
#'   downstream outcomes.
#'
#' @details
#'
#' Discriminant composite scoring is implemented by \code{discriminant_score()},
#' \code{calc_discriminant_composite()}, \code{safe_normalize()},
#' \code{weighted_row_mean()}, \code{calc_metrics()}, and, when a structural
#' model is supplied, \code{expand_lower_paths()}. The code defines the
#' procedure operationally as follows.
#'
#' \strong{1. Preprocessing and ordering.}
#'
#' The supplied \code{composite_list} is split into lower-order composites
#' (defined directly by observed indicators) and higher-order composites
#' (defined by previously computed composites). Lower-order composites are
#' always scored first. If higher-order composites are present, they are then
#' scored from the newly appended lower-order composite columns.
#'
#' Before any discriminant weights are estimated, the function inspects all
#' lower-order indicators for missing values. If any are missing, only those
#' lower-order indicator columns are imputed using
#' \code{missRanger::missRanger()} with the user-supplied \code{pmm_k},
#' \code{ntrees}, \code{maxiter}, \code{seed}, and \code{verbose} settings.
#' The imputed values replace only the original missing lower-order indicator
#' cells before the composite-scoring stage begins.
#'
#' The function also checks whether indicators within each lower-order composite
#' appear to share the same response scale. A composite is treated as
#' approximately continuous if any indicator has more than 15 unique values; in
#' that case the within-composite scale-consistency check is skipped. Otherwise,
#' the check requires all indicators in that composite to have the same number
#' of unique observed values. If any composite fails this test, the function
#' either warns or errors according to \code{on_scale_mismatch}.
#'
#' If a \code{composite_model} is supplied, its path table is expanded so that
#' any higher-order predictor in the \code{from} column is duplicated by its
#' immediate lower-order components. The expanded model is then used to identify
#' which composites should receive predictive reweighting from downstream
#' outcomes and in what topological order they should be scored.
#'
#' \strong{2. Casewise composite formula.}
#'
#' For any composite with indicator matrix
#' \eqn{\mathbf{X} = [x_{cj}]} of dimension \eqn{N \times m}, the final
#' composite score is computed by \code{weighted_row_mean()} using only the
#' indicators observed for each case. If \eqn{O_c} is the set of non-missing
#' indicators for case \eqn{c}, then
#'
#' \deqn{C_c = \frac{\sum_{j \in O_c} x_{cj} w_j^{*}}{\sum_{j \in O_c}|w_j^{*}|}}
#'
#' where \eqn{w_j^{*}} denotes the final normalized weight for indicator
#' \eqn{j}. If all indicators are missing for a case, \eqn{C_c} is \code{NA}.
#' This is the exact casewise scoring rule used by the code; when all weights
#' are positive, it reduces to the usual weighted mean over the observed
#' indicators.
#'
#' The normalization used throughout is \code{safe_normalize()}, which divides a
#' raw weight vector by its mean absolute value:
#'
#' \deqn{w_j^{*} = \frac{w_j}{\frac{1}{m}\sum_{k=1}^{m}|w_k|}}
#'
#' This choice is important because it is also the mechanism that stabilizes the
#' algorithm when weights contain a mixture of positive and negative values.
#'
#' \strong{3. Latent discrimination weights.}
#'
#' For each composite, \code{calc_discriminant_composite()} first extracts the
#' indicator data frame \code{df <- data[, var]}. All indicators must be
#' numeric. Indicators with zero variance are identified by
#' \code{sd(x, na.rm = TRUE) == 0} (or \code{NA} after removing missingness) and
#' are treated specially: they trigger a warning and are assigned raw
#' discrimination weight 0.
#'
#' If \code{weight = "irt"}, the function fits a unidimensional
#' \code{mirt::mirt(..., model = 1)} model to the non-zero-variance indicators,
#' using the user-supplied \code{item_type}. The discrimination coefficients
#' \eqn{a_j} are extracted from \code{mirt::coef(..., IRTpars = TRUE,
#' simplify = TRUE)[["items"]][, "a"]}. If at least two non-zero-variance
#' indicators are available, those coefficients become the raw discrimination
#' weights; if fewer than two remain, any non-zero-variance indicator falls back
#' to raw weight 1 while zero-variance indicators remain at 0. The resulting
#' vector is then normalized with \code{safe_normalize()}.
#'
#' If \code{weight = "pca"} or \code{weight = "glm"}, the latent discrimination
#' stage is identical: the function runs
#' \code{psych::principal(..., nfactors = 1, rotate = "none")} on the
#' non-zero-variance indicators and uses the first principal-component loadings
#' directly as raw discrimination weights. Thus, in the current implementation,
#' \code{"glm"} and \code{"pca"} do \emph{not} differ in their latent weighting
#' stage. The \code{"glm"} label persists only because predictive reweighting,
#' described next, may use GLM-based outcome models. If the PCA fit fails, the
#' non-zero-variance indicators fall back to raw weight 1 and zero-variance
#' indicators remain at 0. If the entire discrimination vector is effectively
#' zero, the code falls back to equal weights
#' \eqn{(1/m, \ldots, 1/m)} before normalization.
#'
#' Negative latent discrimination weights trigger a warning because they often
#' indicate reverse-keyed items that have not been recoded. The function does
#' not reverse-score such items automatically.
#'
#' \strong{4. Optional predictive reweighting from outcomes.}
#'
#' Predictive weighting occurs only when \code{outcomes} are supplied to
#' \code{calc_discriminant_composite()}, which in practice happens when
#' \code{discriminant_score()} is called with a non-\code{NULL}
#' \code{composite_model} and the current composite appears as a predictor in
#' the expanded path table. The downstream outcomes are then extracted from
#' \code{data[, outcomes]}.
#'
#' Two predictive engines are supported.
#'
#' If \code{pred_type = "glm"}, the function fits one elastic-net model per
#' outcome using \code{glmnet::cv.glmnet()} with design matrix
#' \eqn{\mathbf{X}} equal to the composite's indicators and response equal to
#' the selected outcome. For each outcome \eqn{r = 1, \ldots, R}, the model uses
#' \code{alpha = alpha}, \code{family = family}, \code{lower.limits = 0}, and a
#' reproducible fold assignment
#' \eqn{\mathrm{foldid}_c \in \{1, \ldots, K\}} generated by
#' \code{sample(rep(seq_len(nfolds), length.out = nrow(df)))} after setting the
#' user-supplied seed once before the outcome loop. The coefficient vector at
#' \code{s = "lambda.min"} is extracted, its intercept is discarded, and the
#' remaining coefficients are normalized with \code{safe_normalize()} unless
#' they are all essentially zero, in which case that outcome contributes a zero
#' vector. The per-outcome coefficient vectors are then averaged:
#'
#' \deqn{\bar{\boldsymbol{\gamma}} = \frac{1}{R}\sum_{r=1}^{R}\boldsymbol{\gamma}^{(r)}}
#'
#' and normalized again with \code{safe_normalize()} unless the average vector
#' is itself essentially zero.
#'
#' Because \code{lower.limits = 0} is passed to \code{cv.glmnet()}, the GLM
#' predictive stage constrains coefficients to be nonnegative in the fitted
#' model.
#'
#' If \code{pred_type = "rf"}, the function performs a manual
#' \eqn{K}-fold resampling procedure for each outcome. For outcome \eqn{r}, the
#' indicator matrix and that outcome are combined, fold labels are sampled using
#' the same seeded strategy described above, and for each fold a
#' \code{ranger::ranger()} model is fitted on the training partition only, using
#' \code{importance = importance}, \code{num.trees = ntrees}, and
#' \code{seed = seed}. The resulting variable-importance values are averaged
#' across folds for each indicator, re-ordered to match the indicator columns,
#' and then averaged across outcomes. As in the GLM case, the final predictive
#' vector is normalized with \code{safe_normalize()} unless it is effectively
#' zero, in which case a zero vector is used.
#'
#' \strong{Circularity caution.} Predictive reweighting uses each composite's
#' downstream outcome(s) to construct that composite's weights, so the resulting
#' predictor is not independent of the outcome. Estimating and testing the same
#' predictor-outcome association(s) on the same data is therefore circular and
#' yields optimistically biased results (inflated effect sizes, anticonservative
#' inference). Whenever outcome-informed weighting is applied, the function
#' issues a \code{warning()} naming the affected composites. To draw valid
#' inferences about those paths, estimate the weights and test the paths on
#' \emph{different} data, e.g., a train/test split or a separate confirmatory
#' sample.
#'
#' \strong{5. Final weight aggregation.}
#'
#' Let \eqn{\mathbf{d}} denote the normalized latent discrimination vector and
#' \eqn{\mathbf{p}} the normalized predictive vector, if present. The final
#' weights are:
#'
#' \deqn{\mathbf{w}^{*} =
#' \frac{\mathbf{d} + \mathbf{p}}
#' {\frac{1}{m}\sum_{j=1}^{m}|d_j + p_j|}}
#'
#' when predictive weighting is available, and
#'
#' \deqn{\mathbf{w}^{*} =
#' \frac{\mathbf{d}}
#' {\frac{1}{m}\sum_{j=1}^{m}|d_j|}}
#'
#' otherwise. These formulas reflect the exact implementation, since the code
#' combines the latent and predictive vectors additively and then applies
#' \code{safe_normalize()} to the sum.
#'
#' \strong{6. Higher-order composites and structural-model behavior.}
#'
#' When higher-order composites are scored in the main \code{discriminant_score()}
#' workflow, the implementation always calls
#' \code{calc_discriminant_composite(..., weight = "pca")} regardless of the
#' user-supplied \code{weight}. This is done because higher-order indicators are
#' themselves already continuous composite scores rather than original item
#' responses. Consequently, IRT weighting is only used for lower-order
#' composites built directly from item-level indicators.
#'
#' If a \code{composite_model} is supplied, composites that are terminal
#' outcomes in the model are first scored without outcome-based predictive
#' reweighting. Composites that serve as predictors in the expanded path table
#' are then rescored in topological order using the corresponding model-implied
#' downstream outcomes. In this sense, the structural model affects only the
#' optional predictive augmentation of the weights, not the basic latent
#' discrimination step.
#'
#' \strong{7. Reliability and validity metrics.}
#'
#' When \code{return_metrics = TRUE}, the downstream metrics are computed by
#' \code{calc_metrics()} using the final normalized weights and the composite
#' score produced above. The reported item loadings \eqn{\lambda_j} are
#' standardized loadings on a single common factor, estimated by a
#' minimum-residual one-factor solution of the indicators' pairwise-complete
#' correlation matrix (\code{psych::fa(..., nfactors = 1, fm = "minres")}).
#' Unlike an item-rest (corrected item-total) correlation, a factor loading is
#' not attenuated toward the construct, so AVE and \eqn{\rho_c} are not
#' deflated; unlike an item-total correlation against the full composite, it
#' avoids part-whole inflation. The same downstream metric formulas documented
#' for \code{correlation_score()} then apply:
#'
#' \deqn{\mathrm{AVE} =
#' \frac{\sum_{j=1}^{m}\lambda_j^2 w_j^{*}}
#' {\sum_{j=1}^{m}\lambda_j^2 w_j^{*} +
#'  \sum_{j=1}^{m}(1-\lambda_j^2) w_j^{*}}}
#'
#' \deqn{\rho_c =
#' \frac{\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2}
#' {\left(\sum_{j=1}^{m}\lambda_j w_j^{*}\right)^2 +
#'  \sum_{j=1}^{m}(1-\lambda_j^2)(w_j^{*})^2}}
#'
#' Cronbach's alpha is obtained from \code{ltm::cronbach.alpha()} when
#' \pkg{ltm} is installed and otherwise returned as \code{NA}. As with the
#' other weighted scoring functions in the package, the weighted composite
#' reliability \eqn{\rho_c} is the metric that most directly reflects the
#' implemented scoring model.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param composite_list A required \code{composite_list} object. Each name in
#'   the list represents a composite variable, and the corresponding vector
#'   contains the column names that are associated with the indicators
#'   comprising said composite variable.
#' @param composite_model An optional \code{composite_model} object. Combines
#'   one or more sets of `link` paths specifying directed associations between
#'   variables. The model is assumed to be a directed acyclic graph. Supplying a
#'   model triggers outcome-informed (predictive) reweighting of the predictor
#'   composites, which introduces circularity: see the \strong{Circularity
#'   caution} in Details. Weights tuned to predict an outcome must not be used to
#'   test that same predictor-outcome path on the same data.
#' @param weight Required weighting schema. Schemas include \code{c("irt",
#'   "pca", "glm")}. \code{c("pca", "glm")} run the same weighting schema.
#'   Default is \code{"irt"}.
#' @param pred_type Prediction method schema for predictive weighting. Schemas
#'   include \code{c("glm", "rf")}. \code{"glm"} runs \code{glmnet::cv.glmnet()}
#'   for regularization linear regression. \code{"rf"} runs
#'   \code{ranger::ranger()} for Random Forest modeling. Default is
#'   \code{"glm"}.
#' @param item_type Character string or vector specifying the item response
#'   model(s) to be used, passed to the \code{itemtype} argument in
#'   \code{mirt::mirt()}. See \code{?mirt::mirt} for the full list of supported
#'   item types and their definitions. Defaults to \code{NULL}.
#' @param pmm_k Integer. Number of donor candidates used for predictive mean
#'   matching (PMM) during imputation. If set to \code{0}, PMM is disabled and
#'   raw predictions are used.
#' @param maxiter Integer. Maximum number of iterations for chained equations in
#'   \code{missRanger::missRanger()}. Iteration continues until convergence or
#'   the specified limit is reached.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param alpha Elastic net mixing parameter, with \code{0 <= alpha <= 1}.
#'   Controls the relative weighting of L1 (lasso) and L2 (ridge) penalties in
#'   the model. The penalty term is defined as \code{(1 - alpha)/2 * sum(beta^2)
#'   + alpha * sum(abs(beta))}, where \code{beta} is the vector of coefficients.
#'   When \code{alpha = 1}, the penalty is pure lasso; when \code{alpha = 0}, it
#'   is ridge regression.
#' @param nfolds Integer. The number of folds used for k-fold cross-validation
#'   in both the GLM elastic-net path (\code{glmnet::cv.glmnet}) and the
#'   Random Forest resampling path. Controls how the data are partitioned during
#'   resampling. Must be at least 2. Default is 10.
#' @param ntrees Integer. Number of trees to grow in the random forest model.
#'   Default in \code{ranger} is 500.
#' @param importance Character string specifying the type of variable importance
#'   measure to compute. Must be one of \code{"permutation"} or
#'   \code{"impurity"}.
#'   \itemize{
#'     \item \code{"permutation"}: Computes mean decrease in predictive accuracy by permuting each variable and measuring the resulting drop in model performance (typically using out-of-bag data). This method is more computationally expensive but generally provides more reliable and differentiated estimates of variable importance. It is robust to scale and less biased in the presence of correlated or irrelevant predictors.
#'     \item \code{"impurity"}: Computes mean decrease in node impurity (e.g., Gini for classification, variance for regression) aggregated over all trees. This method is fast and computed during model training, but can be biased toward variables with many categories or continuous values, and tends to distribute importance more evenly across predictors—even when some are irrelevant or redundant.
#'   }
#'   Recommend using \code{"impurity"} for speed and \code{"permutation"} for
#'   interpretability, reliability, or feature selection. Default is
#'   \code{"permutation"}.
#' @param family Character string specifying the model family. Must be one of
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
#' @param verbose Logical; if \code{1}, print progress messages and intermediate
#'   output. Defaults to \code{0}.
#' @param htmt_cutoff Numeric threshold used for HTMT PASS/FAIL classification
#'   in the discriminant-validity summary returned when
#'   \code{return_metrics = TRUE}. Default is \code{0.90}.
#' @param seed An integer seed for reproducibility. Defaults to \code{NULL},
#'   where no seed is set.
#' @param on_scale_mismatch Character string controlling behavior when indicators
#'   within a composite appear to be on different response scales.
#'   \itemize{
#'     \item \code{"warn"} (default): issue a \code{warning()} and continue.
#'       Suitable for interactive use and scripted pipelines.
#'     \item \code{"error"}: call \code{stop()} to halt execution. Use this when
#'       you want the pipeline to fail loudly so the problem cannot be overlooked.
#'   }
#' @param return_metrics Logic to determine whether to return reliability and
#'   validity metrics. Set to \code{TRUE} for a list of dataframes with
#'   reliability and validity metrics.
#' @param file An optional file path. If specified, the results will be written
#'   as a formatted excel workbook. This argument is only relevant if
#'   \code{return_metrics = TRUE}.
#' @param name A required string denoting the name of the composite variable.
#'
#' @returns If \code{return_metrics = FALSE}, a dataframe identical to the input
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
#'  \item \strong{Discriminant Summary}: A matrix of reporting-set
#'  discriminant-validity statistics and PASS/FAIL results.
#'  \item \strong{Fornell-Larcker}: A reporting-set matrix whose diagonal is
#'  \eqn{\sqrt{AVE}} and whose off-diagonals are absolute inter-construct
#'  correlations.
#'  \item \strong{HTMT}: A reporting-set matrix of HTMT ratios.
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
#' # Discriminant scoring imputes internally and fits latent-variable models
#' # (suggested packages 'mirt'/'glmnet'/'missRanger'); it can be slow, so the
#' # calls are wrapped in \donttest{}.
#' \donttest{
#' # Calculate discriminant-weighted composite scores
#' discriminant_score(data = grit,
#'                    composite_list = composite_list)
#'
#' # Calculate discriminant-weighted composite scores, reliability, & validity
#' # (optionally write a formatted Excel workbook to a temporary file)
#' out <- file.path(tempdir(), "composite.xlsx")
#' discriminant_score(data = grit,
#'                    composite_list = composite_list,
#'                    digits = 3,
#'                    return_metrics = TRUE,
#'                    file = out)
#'
#' unlink(out)
#' }
#'
#'
#' @export
discriminant_score <- function(
    data,
    composite_list,
    composite_model = NULL,
    weight = c("irt", "pca", "glm"),
    pred_type = c("glm", "rf"),
    item_type = NULL,
    pmm_k = 5,
    maxiter = 10,
    verbose = 0,
    htmt_cutoff = 0.90,
    digits = 3,
    alpha = 0.5,
    nfolds = 10,
    ntrees = 100,
    importance = c("permutation", "impurity"),
    family = c("gaussian", "binomial", "multinomial", "poisson"),
    on_scale_mismatch = c("warn", "error"),
    return_metrics = FALSE,
    seed = NULL,
    file = NULL,
    name = NULL){
  
  # -- MATCH ARGUMENTS -- #
  
  pred_type <- match.arg(pred_type)
  weight <- match.arg(weight)
  importance <- match.arg(importance)
  family <- match.arg(family)
  on_scale_mismatch <- match.arg(on_scale_mismatch)
  if (alpha < 0) {alpha <- 0}
  if (alpha > 1) {alpha <- 1}
  
  
  # -- FUNCTION CHECK -- #
  
  if (!is.null(composite_model) && !(weight %in% c("irt", "pca", "glm"))) {
    
    # Give message weight must be "irt", "pca", or "glm" when using composite models
    message(
      "`weight` must be one of `c('irt', 'pca', 'glm')` when `composite_model` is specified. Defaulting to 'irt'."
    )
    
    # Default to IRT
    weight <- "irt"
    
  }
  
  # Expand the composite model paths to include lower-order predictors (if any)
  if (!is.null(composite_model)) {
    composite_model[["paths"]] <- expand_lower_paths(
      composite_model[["paths"]],
      composite_list
    )
  }
  
  # Get the indicator variables
  indicators <- unname(unlist(composite_list[["lower"]]))
  
  # Identify which indicators have missing values
  missing_vars <- names(data[indicators])[colSums(is.na(data[indicators])) > 0]
  
  # If there is any missingness across the indicators, impute using Random Forest
  if(length(missing_vars) > 0){

    if (!requireNamespace("missRanger", quietly = TRUE)) {
      stop(
        "Package 'missRanger' is required when lower-order indicators contain missing data. ",
        "Install it with: install.packages(\"missRanger\")",
        call. = FALSE
      )
    }
    
    # Message with variable names
    message(
      paste0(
        "Missing data detected in the following variables: ",
        paste(missing_vars, collapse = ", "),
        ". Imputing with Random Forest."
      )
    )
    
    # Impute only those columns
    imputed_data <- missRanger::missRanger(
      data = data[, indicators],
      pmm.k = pmm_k,
      num.trees = ntrees,
      maxiter = maxiter,
      seed = seed,
      verbose = verbose
    )
    
    # Replace only the columns with missingness
    data[missing_vars] <- imputed_data[missing_vars]
  }
  
  # Check which composites should be treated as continuous vs. ordinal.
  # Confirm composites match scales.
  composite_value_check <- setNames(
    lapply(
      seq_along(composite_list[["lower"]]),
      function(i) {
        
        # Indicators
        ind <- composite_list[["lower"]][[i]]
        comp_name <- names(composite_list[["lower"]])[i]
        
        # List of unique counts
        unique_counts <- lapply(ind, function(x) length(unique(data[[x]])))
        
        # Convert list to numeric vector
        counts_vec <- unlist(unique_counts)
        non_constant_counts <- counts_vec[counts_vec > 1]
        
        # Check if variable may be continuous
        continuous <- any(counts_vec > 15)
        
        # If continuous, skip the scale consistency check
        if (isTRUE(continuous)) {
          
          message(
            paste0(comp_name, " has >15 unique values and will be treated as continuous.")
          )
          
          consistency <- TRUE
          
        } else {
          
          # Check for scale consistency
          consistency <- length(unique(non_constant_counts)) <= 1
          
          # If there is no mismatch
          if (isTRUE(consistency)) {
            
            message(
              paste0("PASS: ", comp_name, " has consistent values.")
            )
            
          } else {
            
            message(
              paste0("FAIL: ", comp_name, " has inconsistent values. Check indicators.")
            )
            
          }
          
        }
        
        # Return
        check <- list(
          continuous = continuous,
          consistency = consistency
        )
        
      }
    ),
    names(composite_list[["lower"]])
  )
  
  # Extract composite continuous flags
  composite_continuous_check <- purrr::map(
    composite_value_check,
    "continuous") |> 
    purrr::compact() |> 
    purrr::reduce(c)
  
  # Extract composite consistency flags
  composite_consistency_check <- purrr::map(
    composite_value_check,
    "consistency") |> 
    purrr::compact() |> 
    purrr::reduce(c)
  
  # Flag if any false in consistency checks
  match_confirm <- all(composite_consistency_check)
  
  # If any composite fails the scale consistency check, apply the mismatch policy
  if (isFALSE(match_confirm)) {

    mismatch_msg <- paste0(
      "One or more composites have indicators that may be on different scales. ",
      "Check that all items within each composite share the same response scale ",
      "before running composite_score()."
    )

    if (on_scale_mismatch == "error") {
      stop(mismatch_msg, call. = FALSE)
    } else {
      warning(mismatch_msg, call. = FALSE)
    }

  }
  
  
  
  # -- DATA PREPARATION -- #
  
  # Get only lower order variables
  lower_order_varlist  <- composite_list[["lower"]]
  
  # Get only higher order variables
  higher_order_varlist <- composite_list[["higher"]]
  
  # Check that all the variables in the model are contained within the composite list
  if(!is.null(composite_model)){
    
    composite_variables <- c(
      setdiff(names(lower_order_varlist),
              unlist(higher_order_varlist)),
      names(higher_order_varlist)
    )
    
    model_variables <- composite_model[["order"]]
    
    variable_check <- all(model_variables %in% composite_variables)
    
    # Stop if the composite model contains any variables not specified in the
    # composite list.
    if (isFALSE(variable_check)){
      stop("Error: Composite model contains variables not in the composite list.")
    }
    
    
    # Separate variables that need to be run first before predictors in the model
    
    # Higher order outcomes
    higher_order_outcomes <- intersect(
      composite_model[["outcomes"]],
      names(higher_order_varlist)
    )
    
    higher_order_indicators <- unname(
      unlist(
        composite_list[["higher"]][higher_order_outcomes]
      )
    )
    
    # Lower order outcomes
    lower_order_outcomes <- c(
      intersect(
        composite_model[["outcomes"]],
        names(lower_order_varlist)
      ),
      higher_order_indicators
    )
    
    # -- Adjust lower order and higher order varlists for modeling
    pred_lower_order_varlist <- lower_order_varlist[setdiff(names(lower_order_varlist),
                                                            lower_order_outcomes)]
    pred_higher_order_varlist <- higher_order_varlist[setdiff(names(higher_order_varlist),
                                                              higher_order_outcomes)]
    
    # Combine
    pred_varlist <- c(
      pred_lower_order_varlist,
      pred_higher_order_varlist
    )
    
    # Update the lower and higher order varlist that don't need modeling
    lower_order_varlist <- lower_order_varlist[lower_order_outcomes]
    higher_order_varlist <- higher_order_varlist[higher_order_outcomes]
    
    # Create new predictor list
    pred_vars <- setdiff(
      composite_model$order,
      c(names(lower_order_varlist), names(higher_order_varlist))
    )
    
    exp_pred_vars <- c()
    
    for (var in pred_vars) {
      # Check if the variable has higher-order components
      if (var %in% names(pred_higher_order_varlist)) {
        # Prepend the composite variables before the original variable
        exp_pred_vars <- c(exp_pred_vars, composite_list$higher[[var]])
      }
      # Always add the original variable
      exp_pred_vars <- c(exp_pred_vars, var)
    }
    
    # Remove duplicates
    exp_pred_vars <- unique(exp_pred_vars)

    # Circularity warning. The composites in `exp_pred_vars` are about to be
    # rescored with outcome-informed (predictive) weights, i.e., their indicator
    # weights are tuned to predict their model-implied downstream outcome(s).
    # Because the outcome is used to build the predictor, the two are no longer
    # independent, and estimating/testing those same predictor -> outcome paths
    # on this same data set is circular and optimistically biased. Warn once.
    if (length(exp_pred_vars) > 0) {
      warning(
        "Outcome-informed weighting will be applied to: ",
        paste(exp_pred_vars, collapse = ", "),
        ". These composites' weights are tuned to predict their downstream ",
        "outcome(s), so testing the same predictor-outcome path(s) on this same ",
        "data is circular and will be optimistically biased (inflated effect ",
        "sizes, anticonservative inference). Validate those paths on held-out or ",
        "independent data (e.g., a train/test split or a separate confirmatory ",
        "sample).",
        call. = FALSE
      )
    }

  }



  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    # -- RUN CORRELATION WEIGHTED DISCRIMINANT SCORING FOR LOWER ORDER -- #
    
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
          
          calc_discriminant_composite(
            data = data,
            var = var,
            weight = weight,
            pred_type = pred_type,
            item_type = item_type,
            name = NULL,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
            return_metrics = return_metrics
          )
          
        }
        
      }
    )
    
    
    # -- IF HIGHER ORDER VARIABLES EXIST -- #
    
    if(length(higher_order_varlist) > 0){
      
      # Apply the function to each variable of higher order composite list
      data[names(higher_order_varlist)] <- lapply(
        higher_order_varlist,
        function(var) {
          
          calc_discriminant_composite(
            data = data,
            var = var,
            weight = "pca", # Revert to PCA due to the use of continuous inputs
            pred_type = pred_type,
            item_type = item_type,
            name = NULL,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
            return_metrics = return_metrics
          )
          
        }
      )
      
    }
    
    
    # -- IF COMPOSITE MODELING IS SPECIFIED -- #
    if(!is.null(composite_model)) {
      
      # -- RUN WEIGHTED DISCRIMINANT SCORING -- #
      
      # Loop (instead of lapply) due to need for data to be updated. Use
      # `comp_name` (not `name`) so the function's `name` argument is not
      # clobbered by the loop variable.
      for (comp_name in exp_pred_vars) {

        # Get indicators
        var <- pred_varlist[[comp_name]]

        # Higher-order predictors are built from continuous composite scores, so
        # they must be PCA-weighted regardless of the user's `weight`: IRT/mirt
        # expects item-level ordinal data and misbehaves on continuous inputs.
        comp_weight <- if (comp_name %in% names(composite_list[["higher"]])) {
          "pca"
        } else {
          weight
        }

        # Get outcomes
        outcomes <- composite_model[["paths"]] |>
          dplyr::filter(
            from == comp_name
          ) |>
          dplyr::pull(to)

        data[[comp_name]] <- if (length(var) == 1) {

          calc_single_indicator(
            data = data,
            var = var,
            name = NULL,
            digits = digits,
            return_metrics = return_metrics
          )

        } else {

          calc_discriminant_composite(
            data = data,
            var = var,
            weight = comp_weight,
            outcomes = outcomes,
            pred_type = pred_type,
            item_type = item_type,
            name = NULL,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
            return_metrics = return_metrics
          )

        }

      }
      
    }
    
    
    # Update the data object
    data <- as.data.frame(data)
    
    # Return
    return(data)
    
  }
  
  
  
  # -- IF CALCULATING COMPOSITE SCORE & METRICS -- #
  if(return_metrics == TRUE){
    
    # -- RUN WEIGHTED COMPOSITE SCORING -- #
    
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
          
          calc_discriminant_composite(
            data = data,
            var = var,
            weight = weight,
            pred_type = pred_type,
            item_type = item_type,
            name = name,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
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
          
          calc_discriminant_composite(
            data = data,
            var = var,
            weight = "pca", # Revert to PCA due to the use of continuous inputs
            pred_type = pred_type,
            item_type = item_type,
            name = name,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
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
    
    
    # -- IF COMPOSITE MODELING IS SPECIFIED -- #
    if(!is.null(composite_model)) {
      
      # Loop (instead of lapply) due to need for data to be updated. Use
      # `comp_name` (not `name`) so the function's `name` argument survives the
      # loop and is passed correctly to finalize_metric_output()/export below.
      for (comp_name in exp_pred_vars) {

        # Get indicators
        var <- pred_varlist[[comp_name]]

        # Higher-order predictors are built from continuous composite scores, so
        # they must be PCA-weighted regardless of the user's `weight`: IRT/mirt
        # expects item-level ordinal data and misbehaves on continuous inputs.
        comp_weight <- if (comp_name %in% names(composite_list[["higher"]])) {
          "pca"
        } else {
          weight
        }

        # Get outcomes
        outcomes <- composite_model[["paths"]] |>
          dplyr::filter(
            from == comp_name
          ) |>
          dplyr::pull(to)

        pred_results <- if (length(var) == 1) {

          calc_single_indicator(
            data = data,
            var = var,
            name = comp_name,
            digits = digits,
            return_metrics = return_metrics
          )

        } else {

          calc_discriminant_composite(
            data = data,
            var = var,
            weight = comp_weight,
            outcomes = outcomes,
            pred_type = pred_type,
            item_type = item_type,
            name = comp_name,
            digits = digits,
            alpha = alpha,
            nfolds = nfolds,
            family = family,
            ntrees = ntrees,
            importance = importance,
            verbose = verbose,
            seed = seed,
            return_metrics = return_metrics
          )

        }

        # Extract the score
        data[[comp_name]] <- pred_results[["composite_score"]]

        # Update the data object
        data <- as.data.frame(data)

        # Extract the results
        metrics <- rbind(
          metrics,
          pred_results[["composite_metrics"]]
        )

        validity <- rbind(
          validity,
          pred_results[["composite_validity"]]
        )

      }
      
    }
    
    
    
    # Combine into returnable list
    
    composite_sheets <- finalize_metric_output(
      data = data,
      metrics = metrics,
      validity = validity,
      composite_list = composite_list,
      digits = digits,
      file = file,
      name = name,
      htmt_cutoff = htmt_cutoff
    )
    
    # -- RETURN -- #
    return(composite_sheets)
    
  }
  
}
