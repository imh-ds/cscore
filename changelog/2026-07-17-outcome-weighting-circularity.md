# Circularity warning for outcome-informed weighting

**Date:** 2026-07-17
**Type:** Safety / diagnostic warning (+ documentation)
**Scope:** `discriminant_score()` (and `composite_score(weight = c("irt","pca","glm"))`)
when a `composite_model` is supplied. **No numeric output changes** — this adds a
`warning()` and documentation only.

This document is written so the same behavior can be ported to the Python
implementation (`cscorepy`).

---

## 1. The problem

When a `composite_model` is supplied to the discriminant family, predictor
composites are **rescored with outcome-informed (predictive) weights**: their
indicator weights are tuned (via elastic net or random-forest importance) to
predict that composite's model-implied downstream outcome(s).

Because the outcome is used to *construct* the predictor, the predictor is no
longer independent of the outcome. Any subsequent estimation or hypothesis test
of those **same predictor → outcome paths on the same data** is circular and
**optimistically biased**: effect sizes are inflated and inference is
anticonservative. This is a form of leakage / double-dipping, analogous to
selecting features on the full sample and then evaluating on that same sample.

Previously the package applied this reweighting silently, so a user could feed
the resulting composites straight into a regression of the outcome on the
predictors and obtain over-optimistic results without any signal that the
weights had been trained on that outcome.

---

## 2. The change

Emit a single `warning()` whenever outcome-informed weighting is applied, naming
the affected composites and recommending validation on independent data. Add
matching documentation.

### Files changed

| File | Change |
|------|--------|
| `R/score_discriminant.R` | Added a `warning()` in the `composite_model` preparation block, gated on `length(exp_pred_vars) > 0` (the set of composites that will receive predictive weights). Updated `@details` (new "Circularity caution" paragraph) and the `@param composite_model` text. |
| `R/composite_score.R` | Added a circularity note to `@param composite_model`. |
| `tests/testthat/test-score_discriminant.R` | Added two tests: warning fires with a model; no warning without a model. |

No executable change to any weight, score, or metric.

### Where and how the warning fires

`discriminant_score()` splits the composite list into composites that are model
**outcomes** (scored normally) and composites that are model **predictors**
(rescored with outcome-informed weights). The predictor set, after expanding
higher-order predictors into their components, is `exp_pred_vars`. Immediately
after that vector is finalized:

```r
exp_pred_vars <- unique(exp_pred_vars)

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
```

The warning is emitted **once per call**, in the shared preparation block, so it
fires for both `return_metrics = FALSE` and `return_metrics = TRUE`, and only
when a `composite_model` actually induces predictive reweighting.

---

## 3. How to reproduce / verify (R)

```r
devtools::load_all("cscore")
set.seed(42); n <- 150
f1 <- rnorm(n); f2 <- 0.5 * f1 + rnorm(n)
mk <- function(l) round(pmin(pmax(3 + l + rnorm(n, 0, .8), 1), 5))
df <- data.frame(a1=mk(f1),a2=mk(f1),a3=mk(f1),
                 b1=mk(f2),b2=mk(f2),b3=mk(f2),
                 c1=mk(f2),c2=mk(f2),c3=mk(f2))
cl <- composite_list(A=c("a1","a2","a3"), B=c("b1","b2","b3"), C=c("c1","c2","c3"))
cm <- composite_model(link(from = c("A","B"), to = "C"))

# WITH a model -> warns about circularity, naming A and B:
discriminant_score(df, cl, composite_model = cm, weight = "pca", seed = 1)

# WITHOUT a model -> no circularity warning:
discriminant_score(df, cl, weight = "pca", seed = 1)
```

---

## 4. Porting guidance for `cscorepy` (Python)

- Locate the branch that applies **outcome-informed / predictive** reweighting
  (the path that fits GLM/RF models of a composite's downstream outcome to
  derive weights). This is the direct analogue of `exp_pred_vars`.
- Before (or as) that reweighting runs, and **only when it will run** (i.e., a
  model with predictor→outcome edges is supplied and the predictor set is
  non-empty), emit a warning naming the affected composites. In Python:

  ```python
  import warnings
  if predictor_composites:
      warnings.warn(
          "Outcome-informed weighting will be applied to: "
          + ", ".join(predictor_composites)
          + ". These composites' weights are tuned to predict their downstream "
          "outcome(s), so testing the same predictor-outcome path(s) on this "
          "same data is circular and optimistically biased. Validate those "
          "paths on held-out or independent data.",
          stacklevel=2,
      )
  ```

- Emit it **once per call**, not once per composite.
- No numeric parity work is required — this is a message only. The acceptance
  check is behavioral: warning present when a predictive model is supplied,
  absent otherwise.
