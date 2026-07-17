# Three fixes in the discriminant family's structural-model path

**Date:** 2026-07-17
**Type:** Bug fixes (correctness + crash)
**Scope:** `discriminant_score()` / `composite_score(weight = c("irt","pca","glm"))`
when a `composite_model` is supplied, plus GLM predictive weighting for the
`"multinomial"` family. Composite scores and metric labels can change for the
affected model-based paths; other paths are unaffected.

This document is written so the same corrections can be ported to the Python
implementation (`cscorepy`).

---

## Bug 1 — analysis `name` clobbered by the predictor loop

### Problem

In `discriminant_score()`, the structural-model rescoring loop iterated with the
same identifier as the function's `name` argument:

```r
for (name in exp_pred_vars) { ... }
```

When `return_metrics = TRUE`, after the loop finished `name` held the **last
predictor composite's name**, and that stale value was passed to
`finalize_metric_output()` → `export_metrics()`. The exported workbook title
therefore showed the last predictor's name (e.g., `"HI"`) instead of the
user-supplied study name (e.g., `"MY STUDY"`). The `return_metrics = FALSE`
branch shadowed `name` too, but never used it downstream, so only the metrics
path was visibly affected.

### Fix

Rename the loop variable to `comp_name` in **both** model loops (the
`return_metrics = FALSE` and `return_metrics = TRUE` branches), and update every
in-loop reference (`pred_varlist[[comp_name]]`, `from == comp_name`,
`data[[comp_name]]`, `name = comp_name` in the metrics loop). The function's
`name` argument is now untouched and flows correctly to the workbook.

**File:** `R/score_discriminant.R` (both `if (!is.null(composite_model))` loops).

---

## Bug 2 — higher-order predictors scored with the user's `weight` (incl. IRT)

### Problem

Higher-order composites are defined over **other composites' scores**, which are
continuous. Everywhere else in the package they are PCA-weighted for that reason
(the non-model higher-order branches call
`calc_discriminant_composite(..., weight = "pca")`). But inside the
structural-model rescoring loop, **every** predictor — including higher-order
ones — was scored with the user's `weight`:

```r
calc_discriminant_composite(..., weight = weight, outcomes = outcomes, ...)
```

With `weight = "irt"`, this ran `mirt::mirt()` (an ordinal item-response model)
on continuous composite scores. Observed symptom: `"EM cycles terminated after
500 iterations"` (non-convergence), yielding invalid discrimination weights.

### Fix

Choose the weight per composite inside the loop: PCA for higher-order
composites (detected via `comp_name %in% names(composite_list[["higher"]])`),
the user's `weight` otherwise:

```r
comp_weight <- if (comp_name %in% names(composite_list[["higher"]])) "pca" else weight
calc_discriminant_composite(..., weight = comp_weight, outcomes = outcomes, ...)
```

This matches the non-model higher-order path. Lower-order predictors still use
the requested `weight` (e.g., IRT for item-level indicators).

**File:** `R/score_discriminant.R` (both model loops).

---

## Bug 3 — `family = "multinomial"` crashed in GLM predictive weighting

### Problem

In `calc_discriminant_composite()`, GLM predictive coefficients were extracted as:

```r
coefficients <- coef(cv_fit, s = "lambda.min")[-1]
```

For `"gaussian"`, `"binomial"`, and `"poisson"`, `coef()` returns a single
`(p+1) x 1` sparse matrix and `[-1]` correctly drops the intercept. For
`"multinomial"`, `coef()` returns a **list** of one `(p+1) x 1` matrix per
class; `[-1]` then drops an entire **class** and hands a list to
`safe_normalize()`, producing:

```
Error: non-numeric argument to mathematical function
```

### Fix

Add an internal helper `extract_glmnet_coefs()` that returns one non-negative
importance value per predictor for every family:

```r
extract_glmnet_coefs <- function(cv_fit) {
  co <- stats::coef(cv_fit, s = "lambda.min")
  if (is.list(co)) {                 # multinomial: one vector per class
    per_class <- lapply(co, function(m) abs(as.numeric(m)[-1]))
    Reduce(`+`, per_class) / length(per_class)   # mean |coef| per predictor
  } else {
    as.numeric(co)[-1]               # drop intercept
  }
}
```

The output length equals `ncol(x)` in predictor order for all families, so the
downstream normalization/averaging is unchanged. Because `lower.limits = 0`
constrains glmnet coefficients to be non-negative, the mean-absolute
aggregation is just their mean for the constrained fit.

**File:** `R/internal_calc_discriminant_composite.R` (GLM predictive block +
new helper).

---

## How to reproduce / verify (R)

```r
devtools::load_all("cscore")
set.seed(42); n <- 150
f1 <- rnorm(n); f2 <- 0.5*f1 + rnorm(n)
mk <- function(l) round(pmin(pmax(3 + l + rnorm(n, sd=.8), 1), 5))
df <- data.frame(a1=mk(f1),a2=mk(f1),a3=mk(f1),
                 b1=mk(f2),b2=mk(f2),b3=mk(f2),
                 c1=mk(f2),c2=mk(f2),c3=mk(f2))
cl <- composite_list(A=c("a1","a2","a3"), B=c("b1","b2","b3"),
                     C=c("c1","c2","c3"), HI=c("A","B"))
cm <- composite_model(link(from = "HI", to = "C"))

# Bug 1: name passed to export is the study name, not a predictor name.
# Bug 2: with weight = "irt", the higher-order HI is scored with PCA (no mirt).
# Bug 3: multinomial no longer errors:
dat <- df; dat$grp <- factor(sample(c("x","y","z"), n, TRUE))
cscore:::calc_discriminant_composite(
  data = dat, var = c("a1","a2","a3"), weight = "pca", outcomes = "grp",
  pred_type = "glm", family = "multinomial", alpha = .5, nfolds = 5, seed = 1,
  return_metrics = FALSE)   # returns finite scores
```

(See `tests/testthat/test-score_discriminant.R` for the automated versions,
which use `testthat::local_mocked_bindings()` to capture the exported `name` and
the per-composite `weight`.)

---

## Porting guidance for `cscorepy` (Python)

- **Bug 1**: ensure the predictor-rescoring loop uses its own iterator variable
  and does **not** overwrite the run/analysis name argument that is later used
  to title exported output.
- **Bug 2**: when rescoring predictors under a structural model, detect
  higher-order composites (those whose "indicators" are other composite names)
  and force the PCA/first-component weighting for them, regardless of the
  user-selected weighting method. Only apply IRT-style weighting to lower-order,
  item-level composites.
- **Bug 3**: when extracting penalized-regression coefficients for predictive
  weighting, handle the multiclass case: scikit-learn's
  `LogisticRegression(multi_class="multinomial").coef_` is a `(n_classes,
  n_features)` array. Collapse it to one importance per feature via the mean
  absolute coefficient across classes (`np.abs(coef_).mean(axis=0)`), matching
  the binary/continuous case which yields one coefficient per feature. Drop any
  intercept term.
