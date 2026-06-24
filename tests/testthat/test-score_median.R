composite_list_simple <- composite_list(y = c("x1", "x2", "x3"))

test_that("unweighted median composite equals row-wise median", {
  df <- data.frame(x1 = c(1, 2, 3), x2 = c(3, 4, 5), x3 = c(5, 6, 7))
  result <- median_score(data = df, composite_list = composite_list_simple,
                         weight = "median")
  expected <- apply(df[, c("x1", "x2", "x3")], 1, median)
  expect_equal(result$y, expected)
})

test_that("median composite handles NA with row-wise exclusion", {
  df <- data.frame(x1 = c(1, NA), x2 = c(3, 2), x3 = c(5, 4))
  result <- median_score(data = df, composite_list = composite_list_simple,
                         weight = "median")
  expect_equal(result$y[1], 3)   # median(1, 3, 5) = 3
  expect_equal(result$y[2], 3)   # median(2, 4) = 3
})

test_that("median composite returns metrics when requested", {
  df <- data.frame(x1 = 1:10, x2 = 2:11, x3 = 3:12)
  result <- median_score(
    data = df,
    composite_list = composite_list_simple,
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity"))
})

test_that("median_decay returns finite scores on a simple dataset", {
  df <- data.frame(x1 = 1:5, x2 = 2:6, x3 = 3:7)
  result <- median_score(data = df, composite_list = composite_list_simple,
                         weight = "median_decay", decay_rate = 0.5)
  expect_true(all(is.finite(result$y)))
})

test_that("median_gauss returns finite scores on a simple dataset", {
  df <- data.frame(x1 = 1:5, x2 = 2:6, x3 = 3:7)
  result <- median_score(data = df, composite_list = composite_list_simple,
                         weight = "median_gauss", sigma = 0.5)
  expect_true(all(is.finite(result$y)))
})

test_that("median_decay equals median when all items equal (distances = 0)", {
  # All items identical within each row → all distances 0 → all weights equal
  # → decay-weighted mean equals the median (and the mean)
  df <- data.frame(x1 = 1:5, x2 = 1:5, x3 = 1:5)
  result <- median_score(data = df, composite_list = composite_list_simple,
                         weight = "median_decay", decay_rate = 0.5)
  expect_equal(result$y, as.numeric(1:5), tolerance = 1e-10)
})
