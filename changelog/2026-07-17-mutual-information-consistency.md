# Mutual-information estimator consistency and discreteness threshold

**Date:** 2026-07-17
**Type:** Statistical bug fixes (two related issues in the information family)
**Scope:** `weight = "mutual_info"` / `information_score()`. Both fixes can change
the mutual-information **weights**, and therefore the composite **scores**, for
the information family. No other weighting family is affected.

This document is written so the same corrections can be ported to the Python
implementation (`cscorepy`).

---

## Issue 1 — NMI mixed two entropy estimators

### Problem

`calc_mi_composite()` builds a Normalized Mutual Information (NMI) matrix. For a
pair of indicators `(i, j)`:

```
NMI_geometric = MI(i, j) / sqrt( H(i) * H(j) )
NMI_average   = 2 * MI(i, j) / ( H(i) + H(j) )
```

The normalizing entropies `H(.)` were estimated with the user-supplied
`entropy` argument (`"emp"`, `"mm"`, `"shrink"`, `"sg"`), but the numerator
`MI(i, j)` was computed with `infotheo::mutinformation()` using its **default
empirical estimator**. Numerator and denominator therefore used **different
estimators** whenever `entropy != "emp"`.

Consequence: the NMI was not a coherent normalized quantity. Concretely, two
**identical** variables did not yield `NMI = 1`. Measured before the fix
(`entropy = "mm"`, 4-level variable): `NMI ≈ 0.97`; other estimators could even
exceed 1. Because these NMI values are averaged into the indicator weights, the
`mutual_info` weights (and scores) were biased for any non-empirical estimator.

### Fix

Pass the same estimator to the numerator:

```r
mi_raw <- infotheo::mutinformation(df[, i], df[, j], method = entropy)
```

The entropy calls already used `method = entropy`. After the fix, for identical
variables `MI = H`, so `NMI = 1` exactly for every estimator (verified for
`emp`, `mm`, `shrink`, `sg`).

**File:** `R/internal_calc_mi_composite.R` (the pairwise NMI loop).

---

## Issue 2 — `is_discrete_variable()` ignored its `threshold`

### Problem

`is_discrete_variable(x, threshold = 10)` decides whether a variable is treated
as categorical or is handed to `infotheo::discretize()` for binning. Its
documentation states a numeric vector is discrete when it has "no more than
`threshold` unique values **and** all unique values are integer-like."

The code instead returned `TRUE` for **any** integer-like double, regardless of
cardinality:

```r
is_integer_like <- all(abs(x_no_na - round(x_no_na)) < .Machine$double.eps^0.5)
if (is_integer_like) return(TRUE)   # <-- ignored `threshold`
```

Consequence: a high-cardinality count stored as a double (income, days, age in
months, ...) was declared "discrete" and **bypassed** `infotheo::discretize()`.
Entropy and mutual information were then estimated over hundreds of singleton
categories, which is severely biased, again distorting the `mutual_info`
weights.

### Fix

Require **both** conditions, matching the documentation:

```r
if (is.numeric(x)) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) return(FALSE)
  unique_vals <- unique(x_no_na)
  is_integer_like <- all(
    abs(unique_vals - round(unique_vals)) < .Machine$double.eps^0.5
  )
  return(is_integer_like && length(unique_vals) <= threshold)
}
return(FALSE)
```

Unchanged: factors, ordered factors, and **integer-typed** vectors remain
discrete by type (as documented). Only the double branch changed. A
high-cardinality integer-like double now returns `FALSE`, so the caller bins it
via `infotheo::discretize()`.

**File:** `R/internal_check_discrete_variable.R` (the `is.numeric` branch).

### Interaction with `coerce_to_discrete_if_integer_like()`

`calc_mi_composite()` first calls `coerce_to_discrete_if_integer_like(x, threshold)`,
which rounds a numeric to integers only when it has `<= threshold` unique values.
High-cardinality variables are left unrounded and, after this fix, are correctly
flagged non-discrete and passed to `infotheo::discretize()`. The two helpers now
use the threshold consistently.

---

## How to reproduce / verify (R)

```r
devtools::load_all("cscore")

# Issue 1: identical variables -> NMI = 1 for every estimator
v <- sample(1:4, 300, TRUE)
for (est in c("emp","mm","shrink","sg")) {
  mi <- infotheo::mutinformation(v, v, method = est)
  e  <- infotheo::entropy(v, method = est)
  stopifnot(abs(mi / sqrt(e*e) - 1) < 1e-8)
}

# Issue 2: threshold is honored
stopifnot(
   cscore:::is_discrete_variable(as.numeric(1:5),  10),   # TRUE
  !cscore:::is_discrete_variable(as.numeric(1:300),10),   # FALSE (was TRUE)
   cscore:::is_discrete_variable(as.numeric(1:10), 10),   # TRUE  (boundary)
  !cscore:::is_discrete_variable(as.numeric(1:11), 10)    # FALSE (boundary)
)
```

---

## Porting guidance for `cscorepy` (Python)

- **Issue 1**: wherever pairwise MI is computed for the NMI matrix, use the
  **same** entropy/MI estimator as the normalizing entropies. If the Python
  backend exposes an estimator/method parameter (e.g., a `pyinform`/`sklearn`
  MI with a matching entropy call, or a custom estimator), thread the single
  `entropy` choice through **both** the MI numerator and the entropy
  denominators. Acceptance test: identical discrete variables give `NMI == 1`
  (within ~1e-8) for every supported estimator.
- **Issue 2**: in the discreteness predicate, a float column is discrete only
  when it is integer-like **and** has `<= threshold` unique values. Keep
  categorical/integer dtypes discrete by type. Acceptance test: an integer-like
  float with `> threshold` unique values is classified non-discrete (and thus
  gets binned), while one with `<= threshold` unique values is discrete;
  boundary at exactly `threshold` is inclusive.
- Both are numeric-behavior changes for the information family; expect
  `mutual_info` weights/scores to differ from the pre-fix version whenever a
  non-empirical estimator or a high-cardinality integer-like indicator is used.
