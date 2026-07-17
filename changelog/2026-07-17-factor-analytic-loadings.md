# Factor-analytic loadings for AVE and composite reliability

**Date:** 2026-07-17
**Type:** Methodological correction (statistical bug)
**Scope:** Reported indicator *loadings*, and the `ave` and `rhoc` columns of
the validity table, for every multi-indicator composite in every weighting
family. Composite **scores themselves are unchanged** — only the reliability /
convergent-validity metrics change.

This document is written so the same correction can be ported to the Python
implementation (`cscorepy`). It states exactly what was wrong, exactly what
replaced it, and how to verify the port numerically.

---

## 1. The problem

`calc_metrics()` reports, for each indicator `j`, a "loading" `λ_j`, and then
computes two composite-level metrics from those loadings and the scoring
weights `w_j`:

```
AVE  = Σ(λ_j² · w_j) / [ Σ(λ_j² · w_j) + Σ((1 − λ_j²) · w_j) ]
rhoc = (Σ λ_j w_j)²  / [ (Σ λ_j w_j)²  + Σ((1 − λ_j²) · w_j²) ]
```

For unit weights (`w_j = 1`) these reduce to the **standard** definitions:

```
AVE  = mean(λ_j²)                         (average variance extracted)
rhoc = (Σλ_j)² / [ (Σλ_j)² + Σ(1 − λ_j²) ] (McDonald's ω / composite reliability)
```

Both formulas are **derived for standardized loadings on the common factor**
(`λ_j = Cor(indicator_j, latent construct)`). They are only correct when `λ_j`
is that factor loading.

The previous implementation instead used a **corrected item–total (item-rest)
correlation**:

```
λ_j = Cor( x_j , weighted composite of all OTHER items )   # WRONG for these formulas
```

An item-rest correlation is **systematically attenuated** relative to a factor
loading. The cleanest illustration is a 2-item congeneric scale with equal
loadings `λ` and inter-item correlation `r = λ²`:

| quantity                         | value            |
|----------------------------------|------------------|
| true standardized loading        | `λ = √r`         |
| item-rest correlation (old code) | `r = λ²`  (← attenuated) |
| item-total vs. full composite    | `√((1+r)/2)` (← inflated) |
| **factor loading (new code)**    | `≈ √r`  (← correct) |

Because the reported loading was too small, `1 − λ²` (the implied error
variance) was too large, so **AVE and rhoc were deflated**. Downstream, the
Fornell–Larcker check compares `√AVE` (deflated) against *un*-attenuated
inter-construct correlations, so its PASS/FAIL verdicts were biased toward FAIL.

### Empirical demonstration of the old bug

3-item scale, true loadings `.8/.7/.6`, so true `AVE = mean(λ²) = 0.497`, 500 obs:

| loading definition            | reported AVE | reported rhoc |
|-------------------------------|--------------|---------------|
| item-rest (old code)          | **0.302**    | **0.564**     |
| first principal component     | 0.647        | 0.846         |
| **1-factor minres (new code)**| **0.475**    | **0.730**     |

`psych::omega(df, 1)$omega.tot` for the same data = **0.730**, i.e. the new
`rhoc` reproduces McDonald's ω exactly, and the new AVE recovers the truth
(0.475 vs 0.497). Principal-component loadings overshoot and are *not* used.

---

## 2. The fix

Replace the item-rest loading with the **standardized loading on a single
common factor**, estimated by a minimum-residual (`minres` / OLS) one-factor
extraction from the indicators' **pairwise-complete correlation matrix**.

### Files changed

| File | Change |
|------|--------|
| `R/internal_calc_metrics.R` | Removed the item-rest `sapply` loop; `loadings <- fa_loadings(df, composite_score)`. Added new internal helper `fa_loadings()`. Everything downstream (`loadings_squared`, `errors_ave`, `errors_reliability`, `ave`, `rhoc`, ranges, table assembly) is **unchanged**. |
| `R/score_correlation.R`, `R/score_sd.R`, `R/score_information.R`, `R/score_median.R`, `R/score_discriminant.R`, `R/export_metrics.R`, `R/composite_score.R` | Roxygen `@details` / `@return` text updated from "corrected item-total correlations" to "standardized single-factor loadings". No executable code. |

No new package dependency: `psych` is already in `Imports` and is already used
for regression/PCA weights.

### New algorithm (`fa_loadings(df, composite_score)`)

1. Compute the pairwise-complete Pearson correlation matrix
   `R = cor(df, use = "pairwise.complete.obs")` (zero-variance columns yield
   `NA` rows/cols).
2. Mark indicators **usable** if their entire row/column in `R` is finite
   (excludes zero-variance / all-`NA` items). Require `ncol(df) ≥ 2` overall.
3. If **≥ 2** usable indicators:
   a. Extract one factor from the usable sub-matrix:
      `psych::fa(R_usable, nfactors = 1, fm = "minres", rotate = "none", n.obs = nrow(df))`
      and take `$loadings[, 1]`.
   b. **Fallback 1** — if `fa` errors or returns any non-finite loading, use the
      first principal-component loadings
      `psych::principal(R_usable, nfactors = 1, rotate = "none")$loadings[, 1]`.
4. **Fallback 2** — any indicator still without a finite loading (a degenerate
   item, or no factor solution at all) gets
   `Cor(x_j, composite_score, use = "pairwise.complete.obs")`.
5. Any indicator *still* non-finite (fully degenerate) is set to `0` (it then
   contributes nothing to AVE/rhoc, consistent with getting weight 0 upstream).
6. **Clamp** all loadings to `[-1, 1]` so `1 − λ²` remains a valid (non-negative)
   error variance under Heywood cases, keeping `AVE, rhoc ∈ [0, 1]`.

### Sign convention

`psych::fa` orients the factor so loadings are predominantly positive.
Reverse-keyed indicators (which already trigger a negative-weight warning
upstream) receive a negative loading *and* have a negative weight, so the
products `λ_j w_j` in `rhoc` and `λ_j² w_j` in `AVE` stay positive/consistent.

---

## 3. How to reproduce / verify (R)

```r
devtools::load_all("cscore")
set.seed(1)
f  <- rnorm(500); lam <- c(.8,.7,.6,.5)
df <- as.data.frame(sapply(lam, function(l) l*f + sqrt(1-l^2)*rnorm(500)))
names(df) <- paste0("x", 1:4)

r  <- average_score(df, composite_list(y = names(df)), return_metrics = TRUE)
c(rhoc = r$validity$rhoc,
  omega = psych::omega(df, 1, plot = FALSE)$omega.tot,   # should match rhoc
  ave  = r$validity$ave,
  true_ave = mean(lam^2))
#   rhoc ≈ omega ≈ 0.743 ;  ave ≈ 0.42 ≈ true_ave 0.435
```

Invariant checks (all must hold): for every `weight` family, `AVE ∈ [0,1]` and
`rhoc ∈ [0,1]`; a zero-variance indicator does not error and receives loading 0.

---

## 4. Porting guidance for `cscorepy` (Python)

The correction is localized to the loadings routine feeding AVE/rhoc. Port the
**algorithm in §2**, not the exact R internals.

- **One-factor minres loadings**: use
  `factor_analyzer.FactorAnalyzer(n_factors=1, rotation=None, method="minres")`,
  fit on the correlation matrix (`is_corr_matrix=True`), read `loadings_[:, 0]`.
  `factor_analyzer`'s minres closely matches `psych::fa(fm="minres")`.
- **Correlation matrix**: pairwise-complete — e.g. `pandas.DataFrame.corr()`
  (pairwise by default). Detect zero-variance columns (`std == 0` or all-NaN)
  and exclude them exactly as in step 2.
- **Fallback 1 (PCA loadings)**: eigen-decompose the correlation sub-matrix
  `R`; PC1 loadings are `sqrt(eigval_1) * eigvec_1`, sign-flipped so the sum is
  positive. (`numpy.linalg.eigh`.)
- **Fallback 2**: `numpy`/`pandas` correlation of the raw indicator with the
  computed composite score, pairwise-complete.
- **Clamp** loadings to `[-1, 1]`; set fully-degenerate loadings to `0`.
- Keep the AVE/rhoc formulas in §1 **exactly**; only the `λ_j` input changed.
- **Acceptance test**: replicate the §3 example in Python and assert
  `rhoc ≈ omega_total` (within ~1e-2) and `ave ≈ mean(true λ²)`; assert
  `0 ≤ AVE, rhoc ≤ 1` across all weighting families.

### Expected minor cross-language differences

`minres` is iterative; `factor_analyzer` and `psych` can differ by ~1e-3 in the
loadings depending on starting values and convergence tolerance, so AVE/rhoc may
differ at the third decimal. This is acceptable (documented parity policy) as
long as the `rhoc ≈ omega` and boundedness checks pass.
