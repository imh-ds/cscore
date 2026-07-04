composite_list_simple <- composite_list(y = c("x1", "x2", "x3"))

# Perfectly correlated items: all pairwise r = 1, so all weights equal,
# and the weighted composite must equal the unweighted row mean.
perfectly_correlated <- data.frame(
  x1 = 1:10,
  x2 = 1:10 + 1,
  x3 = 1:10 + 2
)

test_that("correlation composite equals rowMeans for perfectly correlated items", {
  result <- correlation_score(
    data = perfectly_correlated,
    composite_list = composite_list_simple,
    weight = "correlation"
  )
  expect_equal(result$y, rowMeans(perfectly_correlated[, c("x1", "x2", "x3")]),
               tolerance = 1e-10)
})

test_that("regression composite equals rowMeans for perfectly correlated items", {
  result <- correlation_score(
    data = perfectly_correlated,
    composite_list = composite_list_simple,
    weight = "regression"
  )
  expect_equal(result$y, rowMeans(perfectly_correlated[, c("x1", "x2", "x3")]),
               tolerance = 1e-6)
})

test_that("correlation weighting warns on negative weights (reverse-keyed item)", {
  df <- data.frame(
    x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x2 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    x3 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)  # reverse-keyed
  )
  expect_warning(
    correlation_score(data = df, composite_list = composite_list_simple,
                      weight = "correlation"),
    regexp = "reverse-keyed"
  )
})

test_that("non-numeric inputs stop with an informative error", {
  df <- data.frame(x1 = letters[1:5], x2 = 1:5, x3 = 1:5,
                   stringsAsFactors = FALSE)
  expect_error(
    correlation_score(data = df, composite_list = composite_list_simple,
                      weight = "correlation"),
    regexp = "Non-numeric"
  )
})

test_that("zero-variance item warns in correlation weighting", {
  df <- data.frame(x1 = rep(3, 10), x2 = 1:10, x3 = 1:10)
  expect_warning(
    correlation_score(data = df, composite_list = composite_list_simple,
                      weight = "correlation"),
    regexp = "Zero-variance"
  )
})

test_that("correlation composite returns metrics when requested", {
  result <- correlation_score(
    data = perfectly_correlated,
    composite_list = composite_list_simple,
    weight = "correlation",
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity",
                         "discriminant_summary", "fornell_larcker", "htmt"))
  expect_true(all(c("alpha", "rhoc", "ave") %in% names(result$validity)))
})
