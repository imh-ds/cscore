skip_if_not_installed("infotheo")

# 6-item, two-factor-ish Likert data plus a higher-order composite.
set.seed(11)
n <- 120
f1 <- rnorm(n); f2 <- 0.4 * f1 + rnorm(n)
mk <- function(l) round(pmin(pmax(3 + l + rnorm(n, sd = 0.8), 1), 5))
info_df <- data.frame(
  p1 = mk(f1), p2 = mk(f1), p3 = mk(f1),
  q1 = mk(f2), q2 = mk(f2), q3 = mk(f2)
)
info_cl <- composite_list(
  P  = c("p1", "p2", "p3"),
  Q  = c("q1", "q2", "q3"),
  PQ = c("P", "Q")
)

test_that("information_score returns finite scores for all composites", {
  res <- suppressWarnings(information_score(data = info_df, composite_list = info_cl))
  expect_true(all(c("P", "Q", "PQ") %in% names(res)))
  expect_true(all(is.finite(res$P)))
  expect_true(all(is.finite(res$Q)))
  expect_true(all(is.finite(res$PQ)))
  expect_equal(nrow(res), n)
})

test_that("information_score returns the full metrics structure", {
  res <- suppressWarnings(
    information_score(data = info_df, composite_list = info_cl,
                      return_metrics = TRUE)
  )
  expect_named(res, c("data", "metrics", "validity",
                      "discriminant_summary", "fornell_larcker", "htmt"))
  # AVE and rhoc bounded in [0, 1].
  expect_true(all(res$validity$ave >= 0 & res$validity$ave <= 1))
  expect_true(all(res$validity$rhoc >= 0 & res$validity$rhoc <= 1))
})

test_that("information_score runs across entropy estimators and NMI methods", {
  for (est in c("emp", "mm", "shrink", "sg")) {
    for (nmi in c("geometric", "average")) {
      res <- suppressWarnings(
        information_score(data = info_df, composite_list = info_cl,
                          entropy = est, nmi_method = nmi)
      )
      expect_true(all(is.finite(res$P)),
                  info = paste("entropy", est, "nmi", nmi))
    }
  }
})

test_that("mutual-information composite errors informatively without infotheo", {
  skip_if(requireNamespace("infotheo", quietly = TRUE),
          "infotheo is installed — cannot test missing-package guard")
  expect_error(
    information_score(data = info_df,
                      composite_list = composite_list(P = c("p1", "p2", "p3"))),
    regexp = "infotheo"
  )
})
