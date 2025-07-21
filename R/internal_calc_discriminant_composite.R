#' Calculate Discriminant Composite Scores
#'
#' @description Calculate the composite score for the discriminant family of
#'   weighting schemas.
#'
#' @param data A dataframe object. This should be a structured dataset where
#'   each column represents a variable and each row represents an observation.
#' @param var A required vector of indicator column names.
#' @param weight Required weighting schema. Schemas include \code{c("irt",
#'   "glm", "pca")}. Default is \code{"irt"}.
#' @param outcomes An optional vector of outcome variable names.
#' @param pred_type Prediction method schema for predictive weighting. Schemas
#'   include \code{c("glm", "rf")}. \code{"glm"} runs \code{glmnet::cv.glmnet()}
#'   for regularization linear regression. \code{"rf"} runs
#'   \code{ranger::ranger()} for Random Forest modeling. Default is
#'   \code{"glm"}.
#' @param item_type Character string or vector specifying the item response
#'   model(s) to be used, passed to the \code{itemtype} argument in
#'   \code{mirt::mirt()}. See \code{?mirt::mirt} for the full list of supported
#'   item types and their definitions. Defaults to \code{NULL}.
#' @param digits The decimal places for the metrics to be rounded to. Default is
#'   3. This argument is only relevant if \code{return_metrics = TRUE}.
#' @param name A required string denoting the name of the composite variable.
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
#'
#' @returns If \code{return_metrics = FALSE}, an array of the composite score is
#'   returned. If \code{return_metrics = TRUE}, a list is returned consisting
#'   of:
#' \itemize{
#'  \item{\code{composite_score}: }{An array with the calculated composite
#'  variable.} \item{\code{composite_metrics}: }{A matrix loadings and weights
#'  of the indicators.}
#'  \item{\code{composite_validity}: }{A matrix of composite reliability and
#'  validity metrics.}
#' }
#'
#' @importFrom stats setNames as.formula
#' @importFrom utils menu
#'
#' @keywords internal
#' @noRd
calc_discriminant_composite <- function(
    data,
    var,
    weight = c("irt", "glm", "pca"),
    outcomes = NULL,
    pred_type = c("glm", "rf"),
    item_type = NULL,
    digits = 3,
    name = NULL,
    alpha = 0.5,
    nfolds = 10,
    ntrees = 100,
    importance = c("permutation", "impurity"),
    family = c("gaussian", "binomial", "multinomial", "poisson"),
    verbose = 0,
    seed = NULL,
    return_metrics){
  
  # -- DATA PREPARATION -- #
  
  # Get dataframe with just indicator vars
  df <- data[, var]
  
  # If outcomes are specified, get dataframe with just outcomes
  if(!is.null(outcomes)){
    odf <- data |> 
      dplyr::select(
        dplyr::all_of(outcomes)
      )
  }
  
  
  # -- DISCRIMINANT WEIGHTS CALCULATION -- #
  
  # IF IRT DISCRIMINANT-WEIGHTED ----
  if(weight %in% c("irt")){
    
    # Run IRT model
    model <- mirt::mirt(
      data = df, 
      model = 1, # latent dimension
      itemtype = item_type,
      verbose = verbose
    )
    
    # Extract coefficients
    coef <- mirt::coef(
      model, 
      IRTpars = TRUE, 
      simplify = TRUE
    )
    
    # Extract discriminant parameters
    discrimination <- coef[["items"]][, "a"]
    
    # Calculate weights
    discrimination <- discrimination / sum(discrimination)
    
  }
  
  # IF PCA-GLM-WEIGHTED ----
  if(weight %in% c("pca", "glm")){
    
    # Run single factor PCA model
    model <- psych::principal(
      df,
      nfactors = 1,
      rotate = "none"
    )
    
    # Extract the latent proxy
    latent_proxy <- model[["scores"]][,1]
    
    # Fit elastic net regression
    cv_fit <- glmnet::cv.glmnet(
      x = as.matrix(df),
      y = latent_proxy,
      alpha = alpha,
      standardize = TRUE,
      nfolds = nfolds
    )
    
    # Extract optimal coefficients (excluding intercept)
    discrimination <- setNames(
      coef(cv_fit, s = "lambda.min")[-1], 
      names(df)
    )
    
    # Calculate weights
    discrimination <- if (sum(discrimination) == 0) {
      rep(1 / length(discrimination), length(discrimination))
    } else {
      discrimination / sum(discrimination)
    }
    
  }
  
  
  # -- PREDICTION WEIGHTS CALCULATION -- #
  
  if(!is.null(outcomes)) {
    
    # IF GLM PREDICTION-WEIGHTED ----
    if(pred_type %in% c("glm")){
      
      # Calculate GLM for each outcome
      glm_coefs <- lapply(
        outcomes,
        function(o){
          
          # Fit glm model
          cv_fit <- glmnet::cv.glmnet(
            x = as.matrix(df), 
            y = odf[[o]], 
            alpha = alpha,
            lower.limits = 0
          )
          
          # Extract prediction coefficients
          coefficients <- coef(cv_fit, s = "lambda.min")[-1]
          
          # Calculate weights
          if (sum(coefficients) == 0) {
            rep(0, length(coefficients))
          } else {
            coefficients / sum(coefficients)
          }
          
        }
      )
      
      # Reduce to a single average vector
      avg_coefs <- purrr::reduce(glm_coefs, `+`) / length(glm_coefs)
      
      # Calculation prediction weights
      prediction <- if (sum(avg_coefs) == 0) {
        rep(0, length(avg_coefs))
      } else {
        avg_coefs / sum(avg_coefs)
      }
      
    }
    
    # IF RF PREDICTION-WEIGHTED ----
    if(pred_type %in% c("rf")){
      
      rf_coefs <- lapply(
        outcomes,
        function(o) {
          
          tdf <- cbind(df, odf[o])
          
          folds <- sample(rep(1:nfolds, length.out = nrow(tdf)))
          
          results <- purrr::map_dfr(1:nfolds, function(i) {
            train_data <- tdf[folds != i, ]
            test_data  <- tdf[folds == i, ]
            
            fit <- ranger::ranger(
              formula = as.formula(paste(o, "~ .")),
              data = train_data,
              importance = importance,
              num.trees = ntrees,
              seed = seed
            )
            
            tibble::tibble(
              variable = names(fit$variable.importance),
              importance = fit$variable.importance,
              fold = i
            )
          })
          
          vi_df <- results |> 
            dplyr::group_by(variable) |> 
            dplyr::summarise(importance = mean(importance, na.rm = TRUE)) |> 
            dplyr::ungroup()
          
          vi_df <- vi_df[match(names(df), vi_df$variable), ]
          
          dplyr::pull(vi_df, importance)
          
        }
      )
      
      # Reduce to a single average vector
      avg_coefs <- purrr::reduce(rf_coefs, `+`) / length(rf_coefs)
      
      # Calculation prediction weights
      prediction <- if (sum(avg_coefs) == 0) {
        rep(0, length(avg_coefs))
      } else {
        avg_coefs / sum(avg_coefs)
      }
      
    }
    
    # Set names to prediction weights
    prediction <- setNames(prediction, names(df))
    
  }
  
  # Calculate weights
  weights <- if(!is.null(outcomes)) {
    (discrimination + prediction) / mean((discrimination + prediction))
  } else {
    discrimination / mean(discrimination)
  }
  
  # Calculate composite score
  composite_score <- weighted_row_mean(df, weights)
  
  
  # -- IF ONLY CALCULATING COMPOSITE -- #
  if(return_metrics == FALSE){
    
    return(composite_score)
    
  }
  
  
  # -- IF CALCULATING COMPOSITE, RELIABILITY, & VALIDITY -- #
  if(return_metrics == TRUE){
    
    metrics <- calc_metrics(
      df = df,
      composite_score = composite_score,
      weights = weights,
      digits = digits,
      name = name
    )
    
    return(metrics)
    
  }
  
}