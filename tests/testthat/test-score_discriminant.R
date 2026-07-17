skip_if_not_installed("psych")

composite_list_simple <- composite_list(y = c("x1", "x2", "x3", "x4", "x5"))

# Moderately correlated 5-item Likert-style scale
set.seed(42)
n <- 50
latent <- rnorm(n)
disc_df <- data.frame(
  x1 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x2 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x3 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x4 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5)),
  x5 = round(pmin(pmax(latent + rnorm(n, 0, 0.5) + 3, 1), 5))
)

test_that("PCA discriminant composite returns finite scores", {
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "pca"
  )
  expect_true(all(is.finite(result$y)))
  expect_equal(length(result$y), n)
})

test_that("PCA discriminant composite returns metrics when requested", {
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "pca",
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity",
                         "discriminant_summary", "fornell_larcker", "htmt"))
  expect_true("y" %in% names(result$data))
})

test_that("IRT discriminant composite runs when mirt is available", {
  skip_if_not_installed("mirt")
  result <- discriminant_score(
    data = disc_df,
    composite_list = composite_list_simple,
    weight = "irt",
    seed = 42
  )
  expect_true(all(is.finite(result$y)))
})

test_that("discriminant family errors informatively when mirt is absent", {
  skip_if(requireNamespace("mirt", quietly = TRUE),
          "mirt is installed — cannot test missing-package guard")
  expect_error(
    discriminant_score(
      data = disc_df,
      composite_list = composite_list_simple,
      weight = "irt"
    ),
    regexp = "mirt"
  )
})

test_that("outcome-informed weighting warns about circularity", {
  skip_if_not_installed("glmnet")

  set.seed(7)
  nn <- 80
  f1 <- rnorm(nn); f2 <- 0.5 * f1 + rnorm(nn)
  mk <- function(l) round(pmin(pmax(3 + l + rnorm(nn, 0, 0.8), 1), 5))
  mdf <- data.frame(
    a1 = mk(f1), a2 = mk(f1), a3 = mk(f1),
    b1 = mk(f2), b2 = mk(f2), b3 = mk(f2),
    c1 = mk(f2), c2 = mk(f2), c3 = mk(f2)
  )
  cl <- composite_list(
    A = c("a1", "a2", "a3"),
    B = c("b1", "b2", "b3"),
    C = c("c1", "c2", "c3")
  )
  cm <- composite_model(link(from = c("A", "B"), to = "C"))

  # A model that triggers predictive reweighting must warn about circularity.
  expect_warning(
    suppressMessages(
      discriminant_score(mdf, cl, composite_model = cm, weight = "pca", seed = 1)
    ),
    regexp = "circular"
  )
})

test_that("no circularity warning without a composite_model", {
  expect_no_warning(
    suppressMessages(
      discriminant_score(
        data = disc_df,
        composite_list = composite_list_simple,
        weight = "pca"
      )
    )
  )
})
