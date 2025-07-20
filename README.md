---
editor_options: 
  markdown: 
    wrap: 72
---

# cscore

A lightweight R package for constructing composite scores with
correlation or regression-based weighting. Designed for use in
psychological and behavioral science research, particularly where
traditional unweighted scores may distort underlying factor structures.

## Use-case

Conventional composite scores often assume **tau-equivalence**, i.e.,
all indicators equally represent a latent construct. This assumption is
typically violated in practice. `cscore` allows researchers to:

-   Weigh indicators based on how well they represent their latent
    construct

-   Compute reliability and validity metrics

-   Integrate with DAG-based models to increase predictive power

These features make `cscore` particularly useful in prediction inference
workflows.

## Installation

``` r
# install.packages("devtools") # if not already installed
devtools::install_github("imh-ds/cscore")
```

## Quick Start

### Weighting Schemas

`cscore` supports a wide range of weighting strategies:

-   **Covariance-based methods**

    -   `"average"`: unweighted mean

    -   `"correlation"`: Pearson correlation weights

    -   `"regression"`: regression coefficients predicting the
        correlation weighted composite

-   **Standard deviation-based methods**

    -   `"sd_upweight"`: weights proportional to indicator SDs

    -   `"sd_downweight"`: weights inversely proportional to indicator
        SDs

-   **Median-based methods**

    -   `"median"`: simple median of indicators

    -   `"median_decay"`: exponential distance-to-median weighting

    -   `"median_gauss"`: Gaussian distance-to-median weighting

-   **Information-based methods**

    -   `"mutual_info"`: mutual information weighting

-   **Factor-based discriminant methods**

    -   `c("pca", "glm")`: PCA latent proxy + GLM-predicted weighting

    -   `"irt"`: item response theory discrimination parameters

### Creating Composite Scores

To run a correlation-weighted composite, specify the `composite_list`
specifying the composite variable name and the indicators making up the
composite variable. The `composite_score` function can be piped into the
dataframe and it will return the dataframe with the composites appended
at the end of the data.

``` r
library(cscore)

data(grit) # example dataset

# Specify the named list with composite names and their respective indicators
composite_list <- composite_list(
  # Lower-order composites
  extraversion          = sprintf("e%01d", seq(10)),
  neuroticism           = sprintf("n%01d", seq(10)),
  agreeableness         = sprintf("a%01d", seq(10)),
  conscientiousness     = sprintf("c%01d", seq(10)),
  openness              = sprintf("o%01d", seq(10)),
  consistency_interest  = sprintf("gs%01d", c(2,3,5,7,8,11)),
  perseverance_effort   = sprintf("gs%01d", c(1,4,6,9,10,12)),
  # Higher-order composites
  grit                  = c("consistency_interest", "perseverance_effort")
 )
 
# Correlation-weighted composite
grit |>
  composite_score(
    composite_list = composite_list, 
    weight = "correlation"
  )
```

### Returning Reliability and Validity Metrics

When `return_metrics = TRUE`, the following psychometric indices are
returned:

-   **Indicator loadings** (correlation with composite)

-   **Cronbach’s alpha** (α)

-   **Composite reliability** (ρₚ or ω)

-   **Average variance extracted (AVE)\`** for discriminant validity

``` r
grit |> 
  composite_score(composite_list, weight = "correlation", return_metrics = TRUE)
```

### Outcome-Informed Weighting

If there is a certain outcome variable of interest, `cscore` allows the
option of dynamically weighting the composite indicators to increase the
variance explained or predictive power of the composite predictor
variables. Specify the `composite_model` with the directed acyclic graph
(DAG) model. This option is currently only available with
`weight = c("irt", "pca", "glm")`. Specify the `pred_type` as either:

-   `"glm"`: Generalized linear model regression (L1 lasso, L2 ridge,
    Elastic-net)

-   `"rf"`: Random forest importance (GINI impurity, permutation)

``` r
# Define model structure
composite_model <- composite_model(
  link(
    from = c("extraversion",
             "neuroticism",
             "agreeableness",
             "conscientiousness",
             "openness"),
    to = c("grit")
  )
)

# Correlation-weighted composite
grit |>
  composite_score(
    composite_list = composite_list, 
    composite_model = composite_model,
    weight = "irt",
    pred_type = "glm"
  )
```

## Citation

If you use `cscore` in published work, please cite:

> Im, H. (2025). cscore: Weighted Composite Scoring for Scales. GitHub
> repository. <https://github.com/imh-ds/cscore>.
