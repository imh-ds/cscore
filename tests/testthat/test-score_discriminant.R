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
  expect_named(result, c("data", "metrics", "validity"))
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
