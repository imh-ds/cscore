composite_list_simple <- composite_list(y = c("x1", "x2", "x3"))

test_that("average composite equals rowMeans", {
  df <- data.frame(x1 = 1:5, x2 = 2:6, x3 = 3:7)
  result <- average_score(data = df, composite_list = composite_list_simple)
  expect_equal(result$y, rowMeans(df[, c("x1", "x2", "x3")]))
})

test_that("average composite handles missing values", {
  df <- data.frame(x1 = c(1, NA, 3), x2 = c(2, 2, NA), x3 = c(3, 3, 3))
  result <- average_score(data = df, composite_list = composite_list_simple)
  expect_equal(result$y[1], 2)      # (1+2+3)/3
  expect_equal(result$y[2], 2.5)    # (2+3)/2 — x1 missing
  expect_equal(result$y[3], 3)      # (3+3)/2 — x2 missing
})

test_that("average composite returns metrics when requested", {
  df <- data.frame(x1 = 1:10, x2 = 2:11, x3 = 3:12)
  result <- average_score(
    data = df,
    composite_list = composite_list_simple,
    return_metrics = TRUE
  )
  expect_named(result, c("data", "metrics", "validity",
                         "discriminant_summary", "fornell_larcker", "htmt"))
  expect_true("y" %in% names(result$data))
  expect_true("composite" %in% names(result$metrics))
  expect_true("rhoc" %in% names(result$validity))
})

test_that("single-indicator composite is the indicator itself", {
  df <- data.frame(x1 = c(1, 2, 3, 4, 5))
  cl <- composite_list(y = "x1")
  result <- average_score(data = df, composite_list = cl)
  expect_equal(result$y, df$x1)
})
