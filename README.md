## Introduction

**NOTE: Current README file is outdated. Update to be released in future beta version.**


In the realm of psychology and other social behavioral sciences, latent
constructs are often measured using scales. These scales commonly
undergo psychometric validation through methods such as exploratory
factor analysis (EFA), confirmatory factor analysis (CFA), and principal
component analysis (PCA). It is also common to observe varying factor
loadings among indicators, implying that some are *more* reflective
(i.e., representative) of their latent construct than others.

Despite this, many researchers outside of scale development or factor
analysis-related studies (e.g., structural equation modeling) tend to
create an *unweighted* composite score by simply averaging or summing
the indicator items, particularly when the analyses they are running
require the input of just the composite scores (e.g., linear regression,
SPSS PROCESS model, generalized linear models, etc.). This prevalent
practice in psychology overlooks the natural variation in factor
loadings across indicators as usually allowed in the scale’s original
development, thereby forcing all indicators to have an equal
representation of the latent construct. Although this assumption may
hold true for some scales, it is seldom the case in practice. In other
words, for many studies, researchers unintentionally apply a factor
structure on their scales that does not reflect the original scale
development nor have been benchmarked to be valid.

To address this issue, this package provides a straightforward solution
for researchers to generate weighted composite scores. Users can specify
the indicators and choose between correlation or regression weights.
This approach ensures a more accurate representation of the latent
constructs by naturally allowing the indicators’ *weights* and
*loadings* to vary. Below are sections that detail the installation and
setup of the package as well as the use-cases for different weighting
schemas. Lastly, the `cscoring` package is compared with Partial Least
Squares Structural Equation Modeling (PLS-SEM) to detail when and where
one can be preferred over the other.

### Example Data: Big 5 Personality & Grit

In this tutorial, we use a dataset included in the package that contains
500 respondents from the
[OpenPsychoMetrics](https://openpsychometrics.org/_rawdata/) website.
The example dataset in this package randomly sampled 500 respondents
from the much larger original file to increase processing and
calculation time for this quick tutorial. This dataset contains survey
data measuring the Big 5 Personality Traits and Grit.

The **Big 5 Personality Trait** is a personality framework that models
personality on five dimensions: 1) openness, 2) conscientiousness, 3)
extraversion, 4) agreeableness, and 5) neuroticism (John & Sanjay,
1999). Each dimension is argued to represent a spectrum (i.e.,
continuum) with the opposite ends reflecting 1) closedness, 2)
unconscientiousness, 3) introversion, 4) disagreeableness, and 5)
emotional stability, respectively.

1.  **Openness** represents one’s curiosity, imagination, and
    creativity. *Open* people tend to explore new experiences and have a
    broad range of interests whereas those *closed* people tend to
    prefer things that are more routine, conventional, and predictable.
2.  **Conscientiousness** represents one’s organization, discipline, and
    goal orientation. *Conscientious* people tend to work hard and show
    persistence toward their objectives. On the other hand,
    *unconscientious* people tend to be more prone to procrastination as
    well as being more disorganized and impulsive.
3.  **Extraversion** represents one’s sociability and assertiveness.
    *Extraverted* people tend to be sociable around others, like to seek
    excitement, and find it easy to express their emotions.
    *Introverted* people tend to be more reserved, quiet, and
    introspective.
4.  **Agreeableness** represents one’s compassion, cooperation, and
    friendliness. *Agreeable* people tend to be trusting, helpful, and
    polite. *Disagreeable* people tend to be more cynical and
    competitive in how they interact with others.
5.  **Neuroticism** represents one’s anxiety and emotional instability.
    Neurotic people tend to be more susceptible to the experience of
    negative emotions and maladaptive adjustments. Emotionally stable
    people tend to be more calm, confident, and resilient to negative
    experiences.

The **Grit** framework is a theory proposed by Duckworth and colleagues
(Duckworth et al., 2007; Duckworth & Quinn, 2009) to explain why some
people can achieve their goals while others give up. Grit is proposed to
be a combination of two dimensions: 1) passion and 2) perseverance for
long-term objectives.

1.  **Passion** is the degree to which a person has a consistent and
    enduring interest in a long-term goal without being easily
    distracted or bored by their pursuits. Passion motivates people to
    stay focused on their objectives, even in the face of difficulties
    or delays.
2.  **Perseverance** is the degree to which a person works hard and
    persists in their pursuit of goals without giving up or switching to
    easier tasks. Perseverance helps people overcome obstacles and
    achieve their goals, even when obstacles arise.

## Installation & Setup

This package can be installed directly from GitHub with `devtools`:

``` r
# If `devtools` is not already installed, install from CRAN
# install.packages("devtools")
devtools::install_github("imh-ds/cscore")
```

Then load in the necessary packages:

``` r
# If `tidyverse` is not already installed, install from CRAN
# install.packages("tidyverse")

library(tidyverse)
library(cscore)
```

## Usage: Composite Scoring With Correlation Weights

The composite score is a weighted average of the indicator variables. In
a simple average or sum composite, each variable contributes equally to
the final score. However, in a weighted average, each variable
contributes proportionally to its weight. In other words, variables with
higher weights have a greater influence on the composite score.

For correlation weights, the weights are determined by the average
correlation of each indicator with all other indicators. Taking the
average correlation of each indicator measures how well each variable
represents the common trend among all indicators. Indicators highly
correlated with others will have a higher average correlation and thus a
higher weight, contributing more to the composite score calculation.
This approach provides a more nuanced representation of the latent
construct, as it takes into account the varying associations between the
indicators.

By default, the `composite_score` function will use correlation weights.
To use the function with correlation weights, you will only need to
specify a named list of vectors. Each name in the list should represent
the name of the composite score variable, and the corresponding vector
should contain the names of the indicator variables.

``` r
# Load data
data(grit)

# Specify the named list with composite names and their respective indicators
composite_list <- composite_list(
  extraversion = sprintf("e%01d", seq(10)),
  neuroticism = sprintf("n%01d", seq(10)),
  agreeableness = sprintf("a%01d", seq(10)),
  conscientiousness = sprintf("c%01d", seq(10)),
  openness = sprintf("o%01d", seq(10)),
  grit = sprintf("gs%01d", seq(12))
)

# Calculate correlation weighted composite scores
correlation_data <- composite_score(
  data = grit,
  composite_list = composite_list,
  weight = "correlation"
)
```

## Usage: Composite Scoring With Regression Weights

In the context of regression weights, the weights are derived from a
linear regression model. Specifically, the correlation-weighted
composite score is regressed on the indicators, and the resulting
standardized regression coefficients (i.e., beta coefficients) are
utilized as weights. This means that the contribution of each indicator
to the composite score is commensurate with its influence on the
criterion variable, which in this case is the correlation-weighted
composite score. Indicators exerting a greater influence on the
correlation composite score will possess larger coefficients, thereby
contributing more significantly to the regression-weighted composite
score.

Several key things to note about the regression-weighted approach
warrant attention. Firstly, all coefficients are standardized. Secondly,
the use of indicators in the linear regression model necessitates
careful consideration of multicollinearity issues. High correlation
among indicators can lead to multicollinearity, which may introduce bias
or incorrect regression weights in the composite score computation.
Lastly, as the regression weights are the beta coefficients, the
proportion of variance in the composite score explained by the
indicators will approach 100%.

To use the function with regression weights, you will need to specify a
named list of vectors like in the case with correlation weights, and
also specify `weight = "regression"`. Each name in the list should
represent the name of the composite score variable, and the
corresponding vector should contain the names of the indicator
variables. Since the composite variables are kept the same, we’ll use
the previously defined `composite_list` for the regression-weighted data.

``` r
# Calculate regression weighted composite scores
regression_data <- composite_score(
  data = grit,
  composite_list = composite_list,
  weight = "regression"
)
```

As an example of how different or similar the correlation and
regression-weighted composites are, look at the bivariate pearson’s r of
the two for `grit`.

``` r
round(cor(correlation_data$grit,
          regression_data$grit,
          use = "complete.obs"), 3)
```

    ## [1] 0.978

We see that pearson’s r = 0.978. By this metric, the correlation and
regression-weighted composite scores seem identical. However, we’ll see
more differences between the two in later comparisons.

## Usage: Composite Scoring With Equal Weights

If you need to create unweighted composite scores, then it’s as simple
as using the same function as before but specifying the `weight`
argument as `"average"` like so:

``` r
# Create unweighted composite scores using sum averages
sum_data <- composite_score(
  data = grit,
  varlist = varlist,
  weight = "average"
)
```

## Comparing with Unweighted Composites: Uniform Indicators

To compare with unweighted composite scores, we’ll use the unweighted
composite scores created in the section above. We can compare the
correlation and regression-weighted grit composite scores with the
unweighted composite score like so:

``` r
round(cor(correlation_data$grit,
          sum_data$grit,
          use = "complete.obs"), 3)
```

    ## [1] 0.997

``` r
round(cor(regression_data$grit,
          sum_data$grit,
          use = "complete.obs"), 3)
```

    ## [1] 0.98

We can see that the unweighted composite scores closely align with that
of the correlation (r = 0.997) and regression-weighted composite scores
(r = 0.98). This can also be further demonstrated by examining the
correlation matrix of the Big 5 personality traits and Grit for the
unweighted composite scores:

``` r
variables <- c("grit", "extraversion", "neuroticism", "openness", "conscientiousness", "agreeableness")

cor(sum_data %>% select(all_of(variables)),
    use = "complete.obs") %>% round(2)
```

    ##                    grit extraversion neuroticism openness conscientiousness
    ## grit               1.00         0.19       -0.40     0.13              0.61
    ## extraversion       0.19         1.00       -0.30     0.16              0.04
    ## neuroticism       -0.40        -0.30        1.00    -0.10             -0.27
    ## openness           0.13         0.16       -0.10     1.00              0.07
    ## conscientiousness  0.61         0.04       -0.27     0.07              1.00
    ## agreeableness      0.24         0.28       -0.15     0.10              0.11
    ##                   agreeableness
    ## grit                       0.24
    ## extraversion               0.28
    ## neuroticism               -0.15
    ## openness                   0.10
    ## conscientiousness          0.11
    ## agreeableness              1.00

We can see that `conscientiousness` is highly correlated with `grit`.
This is fairly consistent with the existing literature (Credé et al.,
2017; Ponnock et al., 2020; Schmidt et al., 2018, 2020). When we examine
the correlation matrices for the weighted composite scores, the results
tell a similar story:

``` r
# Correlation matrix for correlation-weighted composites
cor(correlation_data %>% select(all_of(variables)),
    use = "complete.obs") %>% round(2)
```

    ##                    grit extraversion neuroticism openness conscientiousness
    ## grit               1.00         0.19       -0.40     0.13              0.61
    ## extraversion       0.19         1.00       -0.30     0.16              0.05
    ## neuroticism       -0.40        -0.30        1.00    -0.11             -0.28
    ## openness           0.13         0.16       -0.11     1.00              0.06
    ## conscientiousness  0.61         0.05       -0.28     0.06              1.00
    ## agreeableness      0.23         0.29       -0.13     0.10              0.09
    ##                   agreeableness
    ## grit                       0.23
    ## extraversion               0.29
    ## neuroticism               -0.13
    ## openness                   0.10
    ## conscientiousness          0.09
    ## agreeableness              1.00

``` r
# Correlation matrix for regression-weighted composites
cor(regression_data %>% select(all_of(variables)),
    use = "complete.obs") %>% round(2)
```

    ##                    grit extraversion neuroticism openness conscientiousness
    ## grit               1.00         0.16       -0.35     0.16              0.50
    ## extraversion       0.16         1.00       -0.28     0.05              0.03
    ## neuroticism       -0.35        -0.28        1.00    -0.15             -0.21
    ## openness           0.16         0.05       -0.15     1.00              0.01
    ## conscientiousness  0.50         0.03       -0.21     0.01              1.00
    ## agreeableness      0.18         0.27       -0.11    -0.08              0.38
    ##                   agreeableness
    ## grit                       0.18
    ## extraversion               0.27
    ## neuroticism               -0.11
    ## openness                  -0.08
    ## conscientiousness          0.38
    ## agreeableness              1.00

The correlation matrix for the correlation-weighted scores is nearly
identical to the unweighted score, indicating that the indicators
contribute uniformly to the composite. This is expected for scales where
indicators do not significantly vary in their reflection of the latent
construct, i.e., the indicators consistently correlate on average. In
such instances, researchers should anticipate minimal differences in
their results, regardless of whether they employ unweighted or weighted
composite scores. *(NOTE: the uniformity of indicator contributions may
suggest redundancy. Researchers may consider whether their scales can be
streamlined by eliminating some repetitive items. This methodological
consideration, however, extends beyond the purview of this package’s
discussion.)*

The correlation matrix for the regression-weighted scores shows
different magnitudes of effects compared to the unweighted matrix, but
the patterns hold. Researchers should expect some muting effect between
composite variables given that they are calculated as the predicted
values from linear models. In other words, it introduces a new layer of
error since the coefficients used to derive the predicted values do not
map perfectly onto the actual values.

The differences between weighted and unweighted composite scores (i.e.,
the value of using weighted composites) become more pronounced when the
scales have indicators that vary in their representation of the latent
construct. To demonstrate this, we can artificially induce this scenario
by combining indicators from other constructs.

## Comparing with Unweighted Composites: Varying Indicators

Consider a scenario where our `conscientiousness` composite includes
three ‘bad’ items (we’ll take three items from `neuroticism` as
examples). `neuroticism` correlates negatively with both
`conscientiousness` and `grit` and naturally, prudent researchers would
reverse-code instances where individual indicators negatively correlate
with the rest. However, for the purposes of this illustration, we will
maintain them as they are. Let’s proceed to construct the flawed
`conscientiousness` composite as follows:

``` r
# Define a new varlist but mix-and-match openness with conscientiousness
bad_list <- list(extraversion = sprintf("e%01d", seq(10)),
                 openness = sprintf("o%01d", seq(10)),
                 agreeableness = sprintf("a%01d", seq(10)),
                 conscientiousness = c(sprintf("c%01d", seq(7)),
                                       sprintf("n%01d", c(8:10))),
                 grit = sprintf("gs%01d", seq(12)))


# Create correlation-weighted composite
correlation_data_mix <- composite_score(data = grit,
                                        varlist = bad_list,
                                        weight = "correlation")

# Create regression-weighted composite
regression_data_mix <- composite_score(data = grit,
                                       varlist = bad_list,
                                       weight = "regression")

# Create unweighted composite
sum_data_mix <- composite_score(data = grit,
                                varlist = bad_list,
                                weight = "average")


# Check correlations of correlation-weighted composite with unweighted
round(cor(correlation_data_mix$conscientiousness,
          sum_data_mix$conscientiousness,
          use = "complete.obs"), 3)
```

    ## [1] 0.933

``` r
# Check correlations of regression-weighted composite with unweighted
round(cor(regression_data_mix$conscientiousness,
          sum_data_mix$conscientiousness,
          use = "complete.obs"), 3)
```

    ## [1] 0.465

Upon implementing this, we observe that the correlation between the
unweighted and weighted `conscientiousness` variables decreases slightly
for correlation-weighted scores and substantially for the
regression-weighted scores. Although the correlations imply that these
are still highly similar, to the extent that they could be considered
identical variables, the difference becomes more noticeable when
examining the correlation matrices.

``` r
variables <- c("grit", "extraversion", "openness", "conscientiousness", "agreeableness")

cor(sum_data_mix %>% select(all_of(variables)),
    use = "complete.obs") %>% round(3)
```

    ##                    grit extraversion openness conscientiousness agreeableness
    ## grit              1.000        0.188    0.130             0.336         0.237
    ## extraversion      0.188        1.000    0.156            -0.151         0.283
    ## openness          0.130        0.156    1.000            -0.010         0.105
    ## conscientiousness 0.336       -0.151   -0.010             1.000        -0.050
    ## agreeableness     0.237        0.283    0.105            -0.050         1.000

``` r
cor(correlation_data_mix %>% select(all_of(variables)),
    use = "complete.obs") %>% round(3)
```

    ##                    grit extraversion openness conscientiousness agreeableness
    ## grit              1.000        0.189    0.127             0.503         0.226
    ## extraversion      0.189        1.000    0.159            -0.047         0.289
    ## openness          0.127        0.159    1.000             0.021         0.100
    ## conscientiousness 0.503       -0.047    0.021             1.000         0.018
    ## agreeableness     0.226        0.289    0.100             0.018         1.000

``` r
cor(regression_data_mix %>% select(all_of(variables)),
    use = "complete.obs") %>% round(3)
```

    ##                    grit extraversion openness conscientiousness agreeableness
    ## grit              1.000        0.161    0.161             0.179         0.184
    ## extraversion      0.161        1.000    0.054            -0.030         0.270
    ## openness          0.161        0.054    1.000             0.028        -0.075
    ## conscientiousness 0.179       -0.030    0.028             1.000         0.160
    ## agreeableness     0.184        0.270   -0.075             0.160         1.000

The correlation between `grit` and `conscientiousness` for the
unweighted sum score is r = 0.336, while it is r = 0.503 for
correlation-weighted composites and r = 0.179 for regression-weighted
composites. As evident, the unweighted composite’s performance
significantly deteriorates where the ‘bad’ indicators exert excessive
influence on the calculation of the final composite score. Similarly,
the regression-weighted composite score is more sensitive to the
inclusion of ‘bad’ indicators because it directly biases the beta
coefficients in the linear model (in this case, the ‘bad’ indicators
have a negative impact rather than simply a muted impact. In most cases,
the negative impact will be mitigated but this serves as a good example
of when regression weights can be more volatile than its correlation
counterpart). In contrast, the correlation-weighted composite
`conscientiousness` significantly outperforms its counterparts by
upweighting the ‘good’ indicators and downweighting the ‘bad’
indicators, thereby retaining a stronger correlation closer to the true
association we observed earlier.

## Additional Usage: Higher-Order Constructs

In cases where a construct has multiple dimensions (i.e., higher-order
construct), the package can simply be piped in again with the dimensions
as the ‘indicators’ for the higher-order composite.

``` r
# Specify the named list with the lower-order composite names and their respective indicators
lower_varlist <- list(consistency_interest = sprintf("gs%01d", c(2,3,5,7,8,11)),
                      perseverence_effort = sprintf("gs%01d", c(1,4,6,9,10,12)))
                      
# Specify the named list with the higher-order composite names and their respective indicators
higher_varlist <- list(grit = c("consistency_interest", "perseverence_effort"))

# Calculate weighted composite scores
higher_data <- grit %>%
  composite_score(varlist = lower_varlist,
                  weight = "correlation") %>%
  composite_score(varlist = higher_varlist,
                  weight = "correlation")
```

## Additional Usage: Measurement Metrics

Researchers can also extract measurement metrics for both correlation
and regression-weighted composite scores. Two metrics can be
extracted: 1) *weights* and 2) *loadings*. The weights are the
correlation and regression weights that were discussed in previous
sections. The loadings are just the bivariate correlation coefficients
between the individual indicators and their respective composite
variable. To get both the weights and loadings of the indicators, we can
use the `composite_metrics` function like so:

``` r
# Specify data and varlist like when calculating the composite scores
metrics <- composite_metrics(grit,
                             weight = "correlation",
                             varlist = varlist)

head(metrics)
```

    ##      composite indicator    weights  loadings
    ## 1 extraversion        e1 0.09829385 0.6966296
    ## 2 extraversion        e2 0.10237413 0.7209283
    ## 3 extraversion        e3 0.09795526 0.6845268
    ## 4 extraversion        e4 0.11210493 0.7916125
    ## 5 extraversion        e5 0.10850156 0.7675970
    ## 6 extraversion        e6 0.08735864 0.5986227

By default, it will return both weights and loadings. The weighting
scheme is also set to correlation by default, but it can be changed by
specifying it in the `weight` argument. If you only require one, then
you can specify it in the function or use the metric-specific functions
like so:

``` r
# To get only the weights
composite_metrics(grit,
                  weight = "correlation",
                  varlist = varlist,
                  metric = "weights")

# To get only the loadings
composite_metrics(grit,
                  weight = "regression",
                  varlist = varlist,
                  metric = "loadings")
```

If we want to score the composites *and* extract the metrics in one
function, then we can use the composite_score_summary function. This
function will return a list of 1) the dataframe with the composite
scores and 2) the metrics table.

``` r
composite_score_summary(grit,
                        varlist = varlist,
                        weight = "correlation")
```

## Comparing with PLS-SEM Reflective and Formative Measurements

Lastly, weighted composite scoring warrants a comparison with partial
least squares structural equation modeling (PLS-SEM). PLS-SEM constructs
composite variables in a similar fashion, creating correlation-weighted
composites when the measurement models are defined as reflective (i.e.,
Mode A), and regression-weighted composites when the measurement models
are defined as formative (i.e., Mode B). The question arises as to the
necessity of the composite scoring offered by the current package when
PLS-SEM can perform the same function. The answer lies in the operation
of PLS-SEM at the structural model level.

Specifically, PLS-SEM aims to maximize the variance explained between
composite variables at the structural model level and adjusts its
indicator weights accordingly (Hair Jr., Hult, Ringle, Sarstedt, Danks,
et al., 2021; Hair Jr., Hult, Ringle, & Sarstedt, 2021). As a result,
PLS-SEM has a higher tendency to encounter overfitting issues compared
to other analyses. In most academic research scenarios, overfitting is
not a concern as the associations between pathways are typically not
substantial enough to warrant this issue. However, in certain areas of
attitudinal-behavioral research (e.g., intent -\> behavior) and industry
research (e.g., consumer experience surveys) where predictor and outcome
variables are expected to correlate highly, overfitting can become a
significant concern.

This concern is exemplified in the current example using data on Big 5
personality traits and Grit. Recent research in this field suggests that
grit and the conscientiousness personality trait are, at best,
overlapping constructs (Schmidt et al., 2018, 2020), and at worst,
identical constructs (Credé et al., 2017; Ponnock et al., 2020). The
correlation between the unweighted conscientiousness and grit composite
scores exceeded 0.6, indicating empirical overlap, especially
considering that these associations are at the respondent level, not the
aggregate level. If grit and conscientiousness are indeed overlapping
constructs, we would expect PLS-SEM to display even stronger
associations as the analysis aims to maximize this association. When
associations in PLS-SEM become excessively strong, it can be inferred
that the analysis is overfitting the model. This overfitting, akin to
the ‘overfitting’ concern in machine learning where noise is mistakenly
identified as real patterns, can result in an overemphasis on the
association between one predictor and the outcome, thereby diverting the
effects from all other viable predictors.

``` r
# If not in library, install `seminr` from CRAN
# install.packages("seminr")

# Load in the package `seminr`
library(seminr)
```

    ## 
    ## Attaching package: 'seminr'

    ## The following objects are masked from 'package:cscoring':
    ## 
    ##     correlation_weights, regression_weights

    ## The following object is masked from 'package:purrr':
    ## 
    ##     rerun

``` r
# Set measurements for PLS-SEM correlation weights
measurements_cor <- constructs(
  composite('grit', multi_items("gs", seq(12)), mode_A),
  composite("extraversion", multi_items("e", seq(10)), mode_A),
  composite("openness", multi_items("o", seq(10)), mode_A),
  composite("neuroticism", multi_items("n", seq(10)), mode_A),
  composite("conscientiousness", multi_items("c", seq(10)), mode_A),
  composite("agreeableness", multi_items("a", seq(10)), mode_A)
)

# Set structure path
structure <- relationships(
  paths(from = c("extraversion", "openness", "neuroticism", "conscientiousness", "agreeableness"),
        to = "grit")
)

# Run PLS-SEM for reflective correlation weights
plssem_cor <- seminr::estimate_pls(data = grit,
                                   measurement_model = measurements_cor,
                                   structural_model = structure)
```

    ## Generating the seminr model

    ## All 500 observations are valid.

``` r
# Save PLS-SEM correlation weighted composite scores
plssem_cordata <- as.data.frame(plssem_cor$construct_scores)

# Examine correlation matrix for composite scores
cor(plssem_cordata) %>% round(3)
```

    ##                   extraversion openness neuroticism conscientiousness
    ## extraversion             1.000    0.210      -0.331             0.108
    ## openness                 0.210    1.000      -0.192             0.185
    ## neuroticism             -0.331   -0.192       1.000            -0.299
    ## conscientiousness        0.108    0.185      -0.299             1.000
    ## agreeableness            0.369    0.089      -0.200             0.159
    ## grit                     0.244    0.239      -0.417             0.663
    ##                   agreeableness   grit
    ## extraversion              0.369  0.244
    ## openness                  0.089  0.239
    ## neuroticism              -0.200 -0.417
    ## conscientiousness         0.159  0.663
    ## agreeableness             1.000  0.292
    ## grit                      0.292  1.000

As observed, the correlation matrix for the PLS-SEM correlation-weighted
reflective composites reveals that the association between
`conscientiousness` and `grit` is significantly higher compared to when
we utilized the unweighted composite scores. This is a direct
consequence of how PLS-SEM operates–it maximizes the variance explained
in the outcome at the structural model by adjusting the weighting schema
of the indicators of the predictor composites.

In industry settings, this is particularly beneficial as it helps to
identify of areas businesses can invest development in to improve a key
performance indicator (KPI). Likewise, in academic research, this is
especially useful when the research objective is to build a prediction
model to aid in the development and refinement of scales and theories by
pinpointing which dimensions of a construct are most likely to be
conceptual antecedents of the outcome of interest.

However, issues arise in scenarios such as the one demonstrated, where
the associations between predictor and outcome variables become overly
optimized, leading to overfitting. This problem also surfaces for
regression-weighted formative composites in PLS-SEM:

``` r
# Set measurements for PLS-SEM regression weights
measurements_reg <- constructs(
  composite("extraversion", multi_items("e", seq(10)), mode_B),
  composite("openness", multi_items("o", seq(10)), mode_B),
  composite("neuroticism", multi_items("n", seq(10)), mode_B),
  composite("conscientiousness", multi_items("c", seq(10)), mode_B),
  composite("agreeableness", multi_items("a", seq(10)), mode_B),
  composite('grit', multi_items("gs", seq(12)), mode_B)
)

# Run PLS-SEM for formative regression weights
plssem_reg <- seminr::estimate_pls(data = grit,
                                   measurement_model = measurements_reg,
                                   structural_model = structure)
```

    ## Generating the seminr model

    ## All 500 observations are valid.

``` r
# Save PLS-SEM regression weighted composite scores
plssem_regdata <- as.data.frame(plssem_reg$construct_scores)

# Examine correlation matrix for composite scores
cor(plssem_regdata) %>% round(3)
```

    ##                   extraversion openness neuroticism conscientiousness
    ## extraversion             1.000    0.174      -0.306             0.284
    ## openness                 0.174    1.000      -0.239             0.339
    ## neuroticism             -0.306   -0.239       1.000            -0.362
    ## conscientiousness        0.284    0.339      -0.362             1.000
    ## agreeableness            0.368    0.160      -0.332             0.290
    ## grit                     0.327    0.327      -0.430             0.720
    ##                   agreeableness   grit
    ## extraversion              0.368  0.327
    ## openness                  0.160  0.327
    ## neuroticism              -0.332 -0.430
    ## conscientiousness         0.290  0.720
    ## agreeableness             1.000  0.360
    ## grit                      0.360  1.000

As evident, the overfitting issue becomes significantly more pronounced
when employing regression-weighted formative composite variables. In
instances where PLS-SEM may not be a suitable option due to overfitting
concerns, one might consider covariance-based SEM (CB-SEM) as an
alternative. This can be implemented through R packages such as `lavaan`
or specialized software like STATA or SPSS AMOS. However, despite its
widespread use in social and behavioral sciences, CB-SEM has several
limitations not well understood by its everyday users.

For example, the emphasis on goodness of fit in CB-SEM often leads
researchers to make ill-guided decisions. Conceptually sound models may
be prematurely dismissed based on insufficient empirical fit. Often,
this is not a reflection of the model’s theoretical foundation, but
rather the result of measurement decisions, such as having an excessive
number of indicators, an overly complex measurement model, or the use of
poorly developed scales. Models that represent partial snapshots of a
much larger, but unmeasured, theoretical model may also yield inadequate
empirical fits due to the omission of certain covariates that could
otherwise enhance the fit and compensate other latent constructs.
Consequently, conceptually valid models are frequently misinterpreted
and dismissed by researchers and reviewers, despite their potential
contributions to the scientific literature.

On the other hand, a poorly conceptualized model may misleadingly meet
empirical field standards for ‘good fit’ by manipulating the model at
the measurement level. This could involve using a minimal number of
indicators per latent construct, thereby limiting the degrees of freedom
to artificially inflate the goodness of fit. In such cases, a
conceptually nonsensical model can misleadingly appear as an empirically
valid model. Although the discussion of how the social and behavioral
sciences misinterpret and incorrectly use powerful tools like CB-SEM
extends beyond the scope of the current package, these are just a few
examples of situations where one might prefer not to use PLS-SEM or
CB-SEM, but still require a more rigorous construction of composite
variables than simply creating unweighted means.

## References

Credé, M., Tynan, M. C., & Harms, P. D. (2017). Much ado about grit: A
meta-analytic synthesis of the grit literature. *Journal of Personality
and Social Psychology*, *113*(3), 492–511.
<https://doi.org/10.1037/pspp0000102>

Duckworth, A. L., Peterson, C., Matthews, M. D., & Kelly, D. R. (2007).
Grit: Perseverance and passion for long-term goals. *Journal of
Personality and Social Psychology*, *92*(6), 1087–1101.
<https://doi.org/10.1037/0022-3514.92.6.1087>

Duckworth, A. L., & Quinn, P. D. (2009). Development and Validation of
the Short Grit Scale (GritS). *Journal of Personality Assessment*,
*91*(2), 166–174. <https://doi.org/10.1080/00223890802634290>

Hair Jr., J. F., Hult, G. T. M., Ringle, C. M., & Sarstedt, M. (2021).
*A Primer on Partial Least Squares Structural Equation Modeling
(PLS-SEM)* (3rd ed.). SAGE Publications.

Hair Jr., J. F., Hult, G. T. M., Ringle, C. M., Sarstedt, M., Danks, N.
P., & Ray, S. (2021). *Partial least squares structural equation
modeling (PLS-SEM) using r: A workbook*. Springer Nature.
<https://doi.org/10.1007/978-3-030-80519-7>

John, O., & Sanjay, S. (1999). *The big five trait taxonomy: History,
measurement, and theoretical perspectives* (2nd ed., Vol. 2, pp.
102–138). The Guilford Press.

Ponnock, A., Muenks, K., Morell, M., Seung Yang, J., Gladstone, J. R., &
Wigfield, A. (2020). Grit and conscientiousness: Another jangle fallacy.
*Journal of Research in Personality*, *89*, 104021.
<https://doi.org/10.1016/j.jrp.2020.104021>

Schmidt, F. T. C., Lechner, C. M., & Danner, D. (2020). New wine in an
old bottle? A facet-level perspective on the added value of Grit over
BFI2 Conscientiousness. *PLOS ONE*, *15*(2), e0228969.
<https://doi.org/10.1371/journal.pone.0228969>

Schmidt, F. T. C., Nagy, G., Fleckenstein, J., Möller, J., & Retelsdorf,
J. (2018). Same Same, but Different? Relations between Facets of
Conscientiousness and Grit. *European Journal of Personality*, *32*(6),
705–720. <https://doi.org/10.1002/per.2171>
