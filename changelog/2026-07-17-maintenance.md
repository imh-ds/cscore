# Maintenance: CRAN examples, required `data`, dead-code removal, tests

**Date:** 2026-07-17
**Type:** Maintenance / packaging (no change to computed results)
**Scope:** Package hygiene and API polish. No weighting, scoring, or metric
formula changed.

Most items here are R-packaging-specific and have **no Python analogue**; the
one item relevant to the `cscorepy` port is the required-`data` behavior (see
§2). It is documented for completeness.

---

## 1. CRAN example policy — write to `tempdir()`, wrap slow examples

**Problem.** Every scoring function's `@examples` wrote the optional metrics
workbook to the working directory (`file = "composite.xlsx"`) and then
`unlink()`-ed it. CRAN forbids examples/tests/vignettes from writing to the
user's working directory or home filespace; scratch output must go to
`tempdir()`.

**Fix.** Each example now does:

```r
out <- file.path(tempdir(), "composite.xlsx")
average_score(..., file = out)
unlink(out)
```

In addition, the `discriminant_score()` and `information_score()` examples are
wrapped in `\donttest{}`, because they depend on suggested packages
(`mirt`/`glmnet`/`missRanger`, and `infotheo`) and are comparatively slow
(`discriminant_score` was ~11-15s in `R CMD check`, over CRAN's 5s guideline).

**Files.** `R/score_average.R`, `R/score_correlation.R`, `R/score_sd.R`,
`R/score_median.R`, `R/score_information.R`, `R/score_discriminant.R`
(examples only).

---

## 2. `data` is now a required argument (was `data = .`)

**Problem.** Every exported scoring function declared its first argument as
`data = .`. The `.` was intended as a magrittr placeholder, but as a *default
value* it is an anti-pattern: calling a function without `data` produced a
cryptic `object '.' not found` rather than a missing-argument error.

**Fix.** The default was removed, so `data` is now required:

```r
composite_score <- function(data, composite_list, ...)   # was data = .
```

**Piping is unaffected** — both pipes fill the first positional argument:

```r
grit |> composite_score(composite_list = cl)   # -> composite_score(grit, ...)
grit %>% composite_score(composite_list = cl)  # -> composite_score(grit, ...)
```

Verified for `%>%`, `|>`, and direct calls. Affected functions:
`composite_score()` (and aliases `cscore`/`cs`), `average_score()`,
`correlation_score()`, `sd_score()`, `median_score()`, `information_score()`,
`discriminant_score()`.

**Python relevance.** The `cscorepy` equivalents should likewise make the data
argument required/positional rather than defaulting it to a sentinel; this is
the natural Python default already (a required first parameter), so typically no
change is needed — just confirm there is no sentinel default that hides a
missing-data error.

---

## 3. Removed dead code: `check_dag()`

**Problem.** `R/internal_check_dag.R` defined `check_dag()`, a DAG cycle-check
that was never called anywhere. Cycle detection is actually performed by
`topo_sort_df()` (which `stop()`s on a remaining edge) and by
`composite_model()` (which errors when no terminal outcome exists).

**Fix.** Deleted `R/internal_check_dag.R` and its generated `man/check_dag.Rd`.
No callers, exports, or tests referenced it.

---

## 4. Anchored `.Rbuildignore` patterns

**Problem.** Several entries were unanchored regexes (`archive*`, `notebook*`,
`example*`, `manuscript*`, `.github*`, `.Rhistory`). `archive*` is the regex
"archiv" + zero-or-more "e", matched anywhere in a path, so it could match
unintended files (e.g., a source file whose path merely contains "archiv").

**Fix.** Anchored each to the intended top-level path:

```
^archive$
^notebook
^example
^manuscript$
^\.github$
\.Rhistory$
```

---

## 5. Test coverage

- Added `tests/testthat/test-score_information.R`: `information_score()` returns
  finite scores for lower- and higher-order composites; the `return_metrics`
  list has the expected structure with AVE/rhoc in `[0, 1]`; and scoring runs
  across all four entropy estimators (`emp`, `mm`, `shrink`, `sg`) and both NMI
  methods (`geometric`, `average`).
- The `composite_model` predictive-weighting path is now exercised by the
  discriminant bug-fix tests added in 1.0.4
  (`tests/testthat/test-score_discriminant.R`).

These close the two gaps noted in review (no `information_score()` tests; no
`composite_model` path tests).
