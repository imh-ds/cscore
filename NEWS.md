# cscore 1.0.1

## Bug fixes

- Fixed circular weighting in the `"regression"` scheme: replaced the tautological
  `lm(cor_composite ~ items)` approach with Thomson (1951) factor score weights
  (`R^{-1} * λ_PC1`) via `psych::principal()$weights`.
- Fixed circular weighting in the `"pca"`/`"glm"` discriminant scheme: replaced
  elastic-net re-regression of PC1 scores onto the same items with direct PC1
  loadings (`psych::principal()$loadings`).
- Replaced part-whole item–total correlations with corrected item-total
  correlations (CITC): each item is now correlated against the composite built
  from all *other* items, eliminating inflation of loadings, AVE, and `rhoc`.
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
