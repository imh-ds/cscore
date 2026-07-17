# cscore 1.0.4

## Bug fixes

- `discriminant_score()` no longer clobbers the analysis `name` when a
  `composite_model` is used with `return_metrics = TRUE`. The predictor-scoring
  loop reused `name` as its iterator, so the exported workbook title received
  the last predictor's name instead of the user-supplied study name. The loop
  now uses a separate variable.
- Higher-order predictors in a `composite_model` are now PCA-weighted regardless
  of the requested `weight`. Previously they were scored with the user's
  `weight`, so `weight = "irt"` ran `mirt` on continuous composite scores
  (non-convergence, invalid weights). Higher-order composites now match the
  non-model path, which always uses PCA for continuous inputs. Lower-order
  composites still honor the requested weight.
- `family = "multinomial"` no longer errors in GLM predictive weighting.
  `coef()` returns a per-class list for multinomial models; the previous
  extraction dropped a class and passed a list to `safe_normalize()`. A new
  helper combines per-class coefficients into one importance-per-predictor
  vector (mean absolute coefficient across classes). See
  `changelog/2026-07-17-discriminant-model-fixes.md`.

# cscore 1.0.3

## Bug fixes

- Mutual-information weighting now uses a single entropy estimator throughout.
  The NMI numerator (`infotheo::mutinformation()`) previously always used the
  default empirical estimator while the normalizing entropies used the
  user-supplied `entropy` argument, so for `entropy != "emp"` the NMI was
  internally inconsistent (two identical variables did not yield NMI = 1). The
  numerator now also uses `entropy`. This changes `mutual_info` weights/scores
  when `entropy` is `"mm"`, `"shrink"`, or `"sg"`.
- `is_discrete_variable()` now honors its `threshold` argument for integer-like
  doubles. It previously classified *any* integer-like numeric as discrete
  regardless of cardinality, so high-cardinality counts (e.g., income, days)
  bypassed `infotheo::discretize()` and produced badly biased entropy/MI. It now
  requires a double to be integer-like **and** have at most `threshold` unique
  values, matching the documented behaviour. May change `mutual_info` scores
  when a high-cardinality integer-like indicator is present. See
  `changelog/2026-07-17-mutual-information-consistency.md`.

# cscore 1.0.2

## Bug fixes

- Reported indicator loadings are now standardized loadings from a single
  common-factor (minres one-factor) solution, replacing the corrected
  item-total (item-rest) correlations used previously. Item-rest correlations
  are attenuated relative to factor loadings, which systematically **deflated**
  `ave` and `rhoc` and biased the Fornell-Larcker verdicts toward FAIL. With
  factor loadings, `rhoc` reproduces McDonald's omega for unit weights and AVE
  recovers the true value. Composite scores are unchanged. See
  `changelog/2026-07-17-factor-analytic-loadings.md`.

## New features

- Outcome-informed (predictive) weighting now emits a circularity `warning()`.
  When a `composite_model` is supplied to the discriminant family, predictor
  composites are reweighted to predict their downstream outcomes; testing those
  same predictor-outcome paths on the same data is circular and optimistically
  biased. The warning names the affected composites and recommends validating
  on held-out or independent data. See
  `changelog/2026-07-17-outcome-weighting-circularity.md`.

# cscore 1.0.1

## Bug fixes

- Fixed circular weighting in the `"regression"` scheme: replaced the tautological
  `lm(cor_composite ~ items)` approach with Thomson (1951) factor score weights
  (`R^{-1} * λ_PC1`) via `psych::principal()$weights`.
- Fixed circular weighting in the `"pca"`/`"glm"` discriminant scheme: replaced
  elastic-net re-regression of PC1 scores onto the same items with direct PC1
  loadings (`psych::principal()$loadings`).
- Fixed the missing-data path in `"median_decay"` / `"median_gauss"`: missing
  items' weights are now zeroed out before row-wise renormalization, matching
  the behaviour of `weighted_row_mean()`.
- Median distances are now scaled by each respondent's within-row item range
  so `decay_rate` and `sigma` are scale-invariant across response formats.
- Corrected AVE label from "discriminant validity" to "convergent validity"
  throughout; added Fornell-Larcker / HTMT note for discriminant validity.

## New features

- `on_scale_mismatch = c("warn", "error")` argument replaces the interactive
  `utils::menu()` call in `discriminant_score()`, making the function safe in
  non-interactive contexts (batch jobs, RMarkdown, Shiny).
- `impute = FALSE` parameter in `composite_score()`: optional pre-imputation
  via `missRanger` for non-discriminant families. Discriminant family always
  imputes internally.
- Per-composite warnings when > 50 % of a respondent's items are missing.
- `safe_normalize()` replaces `w / mean(w)` throughout: denominator is now
  `mean(abs(w))`, preventing explosion when mixed-sign weights produce a
  near-zero mean. `weighted_row_mean()` similarly uses `rowSums(abs(w))`.
- `set.seed()` is now propagated to `cv.glmnet()` (via explicit `foldid`)
  and to manual RF fold sampling, ensuring reproducible cross-validation.
- Numeric input guards and zero-variance protection in covariance and SD
  families: informative errors/warnings replace silent `NA` or `Inf` weights.
- `ltm`, `infotheo`, `mirt`, `glmnet`, `ranger`, and `missRanger` moved from
  `Imports` to `Suggests`; each function that uses them checks availability
  at runtime and stops with an actionable install message if the package is
  absent.

## Documentation

- `@param nfolds` now correctly states it applies to *both* GLM elastic-net
  cross-validation and RF k-fold resampling.
- `@param decay_rate` / `@param sigma` updated to explain scale-invariant
  interpretation after range-scaling.
- `Cronbach's alpha` documentation clarifies tau-equivalence assumption;
  recommends `rhoc` for weighted composites.
- `Authors@R`, `URL`, and `BugReports` added to DESCRIPTION.
- `CITATION` file added.
