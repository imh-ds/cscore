#' Calculate Discriminant-Weighted Composite Scores
#'
#'
#' @description Create composite scores of scales by specifying the indicators
#'   that go into each respective composite variable.
#'
#' @details Discriminant composite scores combine psychometric discrimination
#' and predictive utility into a single weighting schema. Two primary
#' discriminant sources are supported: \strong{latent composite loadings} and
#' \strong{predictive weights}. The composite score for case \eqn{c} is computed
#' as:
#'
#' \deqn{\bar{C}_c = \frac{1}{m} \sum_{j=1}^{m} I_{cj} \cdot w_j^{*}}
#'
#' where \eqn{I_{cj}} is the value of indicator \eqn{j} for case \eqn{c}, and
#' \eqn{w_j^{*}} is the final normalized weight. The weighting includes two
#' stages:
#'
#' \strong{1. Discriminant weighting from latent structure.}
#'
#' If \code{weight = "irt"}, a unidimensional Item Response Theory (IRT) model
#' is estimated:
#'
#' \deqn{P(Y_{cj} = 1) = \mathrm{logit}^{-1}(a_j \cdot \theta_c + b_j)}
#'
#' where \eqn{a_j} is the discrimination parameter for indicator \eqn{j}, and
#' \eqn{\theta_c} is the latent trait estimate for case \eqn{c}. The
#' discrimination parameters \eqn{a_j} are extracted and normalized:
#'
#' \deqn{w_j = \frac{a_j}{\sum_{k=1}^{m} a_k}}
#'
#' If \code{weight = "glm"} or \code{"pca"}, a single-factor principal component
#' analysis (PCA) is conducted. The first factor score is extracted as a latent
#' proxy \eqn{Z_c}. A penalized linear model is then fit:
#'
#' \deqn{Z_c = \sum_{j=1}^{m} \beta_j \cdot I_{cj} + \varepsilon_c}
#'
#' where \eqn{\beta_j} are estimated using elastic net regularization (via
#' \pkg{glmnet}). These are normalized analogously to IRT:
#'
#' \deqn{w_j = \frac{\beta_j}{\sum_{k=1}^{m} \beta_k}}
#'
#' \strong{2. Predictive weighting from external outcomes (optional).}
#'
#' If one or more outcome variables are provided via \code{outcomes}, an
#' additional predictive weighting stage is applied. Two methods are available:
#'
#' \emph{(a) Generalized Linear Model (GLM).} Each outcome \eqn{Y} is regressed
#' on the indicators \eqn{I_{cj}} using elastic net regularization. For each
#' fitted model, the normalized coefficients \eqn{\gamma_j^{(r)}} are extracted.
#' The final predictive weights are averaged over all outcomes:
#'
#' \deqn{p_j = \frac{1}{R} \sum_{r=1}^{R} \gamma_j^{(r)}}
#'
#' where \eqn{R} is the number of outcomes. The predictive weights are then
#' normalized:
#'
#' \deqn{p_j^{*} = \frac{p_j}{\sum_{k=1}^{m} p_k}}
#'
#' \emph{(b) Random Forest (RF).} Each outcome \eqn{Y} is predicted via a random
#' forest, and variable importance scores (e.g., permutation or impurity) are
#' extracted for each indicator. These are averaged across outcomes and
#' normalized in the same manner as the GLM approach (see step a above).
#'
#' \strong{3. Final weight aggregation.}
#'
#' If predictive weights \eqn{p_j^{*}} are available, they are combined with the
#' latent discrimination weights \eqn{w_j} to form final composite weights:
#'
#' \deqn{w_j^{*} = \frac{w_j + p_j^{*}}{\frac{1}{m} \sum_{k=1}^{m} (w_k +
#' p_k^{*})}}
#'
#' If no predictive targets are provided, \eqn{w_j^{*}} defaults to normalized
#' discrimination weights.
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
#' @param nfolds Integer. The number of folds used for random forest k-fold
#'   cross-validation. Controls how the data are partitioned during resampling.
#'   Must be at least 2. Default is 10.
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
#' @param seed An integer seed for reproducibility. Defaults to \code{NULL},
#'   where no seed is set.
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
#' # Calculate discriminant-weighted composite scores
#' discriminant_score(data = grit,
#'                    composite_list = composite_list)
#'
#' # Calculate discriminant-weighted composite scores, reliability, & validity
#' discriminant_score(data = grit,
#'                    composite_list = composite_list,
#'                    digits = 3,
#'                    return_metrics = TRUE,
#'                    file = "composite.xlsx")
#'
#' unlink("composite.xlsx")
#'
#'
#' @export
discriminant_score <- function(
    data = .,
    composite_list,
    composite_model = NULL,
    weight = c("irt", "pca", "glm"),
    pred_type = c("glm", "rf"),
    item_type = NULL,
    pmm_k = 5,
    maxiter = 10,
    verbose = 0,
    digits = 3,
    alpha = 0.5,
    nfolds = 10,
    ntrees = 100,
    importance = c("permutation", "impurity"),
    family = c("gaussian", "binomial", "multinomial", "poisson"),
    return_metrics = FALSE,
    seed = NULL,
    file = NULL,
    name = NULL){
  
  # -- MATCH ARGUMENTS -- #
  
  pred_type <- match.arg(pred_type)
  weight <- match.arg(weight)
  importance <- match.arg(importance)
  family <- match.arg(family)
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
          consistency <- length(unique(counts_vec)) == 1
          
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
  
  # If any composite does fails, prompt user
  if (isFALSE(match_confirm)) {
    
    choice <- menu(
      choices = c("Yes", "No"),
      title = "WARNING: One or more composites have indicators that may be on different scales. Do you want to proceed?"
    )
    
    if (choice != 1) {
      stop("Execution aborted.")
    }
    
    message("Proceeding with execution...")
    
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
      
      # Loop (instead of lapply) due to need for data to be updated
      for (name in exp_pred_vars) {
        
        # Get indicators
        var <- pred_varlist[[name]]
        
        # Get outcomes
        outcomes <- composite_model[["paths"]] |> 
          dplyr::filter(
            from == name
          ) |> 
          dplyr::pull(to)
        
        data[[name]] <- if (length(var) == 1) {
          
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
      
      # Loop (instead of lapply) due to need for data to be updated
      for (name in exp_pred_vars) {
        
        # Get indicators
        var <- pred_varlist[[name]]
        
        # Get outcomes
        outcomes <- composite_model[["paths"]] |> 
          dplyr::filter(
            from == name
          ) |> 
          dplyr::pull(to)
        
        pred_results <- if (length(var) == 1) {
          
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
            outcomes = outcomes,
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
        
        # Extract the score
        data[[name]] <- pred_results[["composite_score"]]
        
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
